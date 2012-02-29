{- Dysfunctional reactive programming! 
 - FRP by awful IO 
 - Marek Materzok -}

module Control.DysFRP (
    Event, Behavior, BehaviorGen,
    runBehavior, mkE,
    liftBG, bindBG,
    utcTimeB, elapsedTimeB, elapsedTimeNumB,
    dswitchB, switchB, constB, accumB, ifB,
    genIntegralB, trapIntegralB,
    nullE, appendE, concatE, snapshotE, snapshotWithE, filterE, whenE, whenCondE, constE,
    feedbackB, genToE, joinE,
    condChangeE, changeE
)where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Fix
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Unique
import Data.Time.Clock
import Data.Functor.Contravariant
import System.Mem.Weak
import System.IO.Unsafe

nthMod l k = l !! abs (k `mod` length l)
nthModMaybe [] k = Nothing
nthModMaybe l k = Just $ nthMod l k

type ReactM = ReaderT Int IO

type ReactRef a = IORef (Int, a, a)

runReactM m = newUnique >>= runReaderT m . hashUnique

-- | Gets the current value of the `Behavior`.
runBehavior = runReactM . openBehavior

data Handler a = Handler { runHandler :: IO (Maybe (a -> ReactM ())) }  

-- | Discrete events.
data Event a = Event { addHandler :: Handler a -> IO () }

-- | Continuous time functions.
data Behavior a = Behavior { openBehavior :: ReactM a }

-- | Time functions with an additional time parameter, corresponding to a starting point.
type BehaviorGen a = Behavior(Behavior a)

instance Contravariant Handler where
    contramap f = alterH (. f)

instance Functor Event where
    fmap f = alterE (contramap f)

instance Functor Behavior where
    fmap f io = Behavior $ fmap f $ openBehavior io

instance Applicative Behavior where
    pure x = Behavior $ pure x
    b1 <*> b2 = Behavior $ openBehavior b1 <*> openBehavior b2

instance Monad Behavior where
    return = pure
    b >>= bf = Behavior $ openBehavior b >>= openBehavior . bf

instance MonadFix Behavior where
    mfix m = Behavior $ mfix $ openBehavior . m

instance Monoid (Event a) where
    mempty = nullE
    mappend = appendE

instance Monoid a => Monoid (Behavior a) where
    mempty = constB mempty
    mappend = liftA2 mappend

instance Num a => Num (Behavior a) where
    b1 + b2 = liftA2 (+) b1 b2
    b1 - b2 = liftA2 (-) b1 b2
    b1 * b2 = liftA2 (*) b1 b2
    negate b = fmap negate b
    abs b = fmap abs b
    signum = fmap abs signum
    fromInteger = constB . fromInteger

instance Fractional a => Fractional (Behavior a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = constB . fromRational

instance Floating a => Floating (Behavior a) where
    pi = constB pi
    exp = fmap exp
    sqrt = fmap sqrt
    log = fmap log
    (**) = liftA2 (**)
    logBase = liftA2 logBase
    sin = fmap sin
    cos = fmap cos
    tan = fmap tan
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

newReactRef :: a -> ReactM (ReactRef a)
newReactRef v = do
    a <- ask
    liftIO $ newIORef (a, v, v)

readReactRef :: ReactRef a -> ReactM a
readReactRef r = do
    (a, v1, v2) <- liftIO $ readIORef r
    a' <- ask
    return $ if a == a' then v1 else v2

writeReactRef :: ReactRef a -> a -> ReactM ()
writeReactRef r v = do
    (a, v1, v2) <- liftIO $ readIORef r
    a' <- ask
    liftIO $ if a == a' then writeIORef r (a, v1, v) else writeIORef r (a', v2, v)

updateReactRef :: ReactRef a -> ReactM a -> ReactM ()
updateReactRef r m = do
    (a, v1, v2) <- liftIO $ readIORef r
    a' <- ask
    when (a /= a') $ do
	liftIO $ writeIORef r (a', v2, undefined)
	writeReactRef r =<< m

mkH :: k -> (a -> ReactM ()) -> IO (Handler a)
mkH k f = do
    w <- mkWeak k f Nothing
    return $ Handler $ deRefWeak w

mksH :: (a -> ReactM ()) -> IO (Handler a)
mksH v = v `seq` mkH v v

alterH :: ((a -> ReactM ()) -> b -> ReactM ()) -> Handler a -> Handler b
alterH f h = Handler $ fmap (fmap f) (runHandler h)

contramapH :: (a -> ReactM b) -> Handler b -> Handler a
contramapH f = alterH (f >=>)

alterE :: (Handler a -> Handler b) -> Event b -> Event a
alterE f e = Event $ addHandler e . f

reactMapE :: (a -> ReactM b) -> Event a -> Event b
reactMapE f = alterE (contramapH f)

ioMapE :: (a -> IO b) -> Event a -> Event b
ioMapE f = alterE (contramapH $ liftIO . f)

-- | Creates a new `Event`. Calling the returned action fires the event.
mkE :: IO (a -> IO(), Event a)
mkE = do
    r <- newIORef []
    let f x = do
        hs <- readIORef r
        (hs', fs) <- fmap unzip $ foldM (\l h -> runHandler h >>= return . maybe l (\y -> (h,y):l)) [] hs
        writeIORef r (reverse hs')
        runReactM $ forM_ fs ($ x)
    let g h = modifyIORef r (h:)
    return (f, Event g)

mkBG :: ReactM (ReactM a) -> BehaviorGen a
mkBG io = Behavior $ fmap Behavior io 

liftBG :: Behavior a -> BehaviorGen a
liftBG = Behavior . openBehavior . constB

bindBG :: (Behavior a -> BehaviorGen b) -> BehaviorGen a -> BehaviorGen b
bindBG f g = Behavior $ openBehavior g >>= openBehavior . f

-- | A behavior which gives the current time.
utcTimeB :: Behavior UTCTime
utcTimeB = Behavior $ liftIO $ getCurrentTime

elapsedTime :: IO (Behavior NominalDiffTime)
elapsedTime = do
    t <- getCurrentTime
    return $ Behavior $ fmap (`diffUTCTime` t) $ liftIO $ getCurrentTime

elapsedTimeNum :: Fractional a => IO (Behavior a)
elapsedTimeNum = fmap (fmap (fromRational . toRational)) $ elapsedTime

-- | A `BehaviorGen` which gives the time from the starting point, in seconds.
elapsedTimeB :: BehaviorGen NominalDiffTime
elapsedTimeB = Behavior $ liftIO $ elapsedTime

-- | A `BehaviorGen` which gives the time from the starting point, in seconds.
elapsedTimeNumB :: Fractional a => BehaviorGen a
elapsedTimeNumB = Behavior $ liftIO $ elapsedTimeNum

-- | A `BehaviorGen` which mirrors the given `Behavior` from the starting point, and switches to the
--   new behaviors (parametrized by the last value before the switch) given by the `Event`.
dswitchB :: Behavior a -> Event (a -> Behavior a) -> BehaviorGen a
dswitchB iob ioe = mkBG $ do
    r <- newReactRef $ openBehavior iob
    let io = join $ readReactRef r
    h <- liftIO $ io `seq` mkH io $ \iobf -> readReactRef r >>= id >>= writeReactRef r . openBehavior . iobf
    liftIO $ addHandler ioe h
    return io

-- | A specialization of `dswitchB`. 
switchB :: Behavior a -> Event (Behavior a) -> BehaviorGen a
switchB iob ioe = mkBG $ do
    r <- newReactRef $ openBehavior iob
    let io = join $ readReactRef r
    h <- liftIO $ io `seq` mkH io $ \iob' -> writeReactRef r $ openBehavior iob'
    liftIO $ addHandler ioe h
    return $ io

-- | A constant `Behavior`.
constB :: a -> Behavior a
constB x = Behavior $ return x

stepB :: a -> Event a -> BehaviorGen a
stepB v e = switchB (constB v) (fmap constB e)

accumB :: a -> Event (a -> a) -> BehaviorGen a
accumB v e = dswitchB (constB v) (fmap (constB .) e)

ifB :: Behavior Bool -> Behavior a -> Behavior a -> Behavior a
ifB = liftA3 (\c t e -> if c then t else e) 

snapshotWithE :: (b -> a -> c) -> Behavior a -> Event b -> Event c
snapshotWithE f beh = alterE (contramapH $ \x -> fmap (f x) $ openBehavior beh)

nullE :: Event a
nullE = Event $ \_ -> return ()

appendE :: Event a -> Event a -> Event a
appendE e1 e2 = Event $ \h -> addHandler e1 h >> addHandler e2 h

concatE :: [Event a] -> Event a
concatE = mconcat

snapshotE :: Behavior a -> Event b -> Event a
snapshotE = snapshotWithE $ const id

filterWhenE :: Behavior (a -> Bool) -> Event a -> Event a
filterWhenE b = alterE (alterH $ \io x -> openBehavior b >>= flip when (io x) . ($ x))

filterE :: (a -> Bool) -> Event a -> Event a
filterE f = filterWhenE (constB f)

whenE :: Behavior Bool -> Event a -> Event a
whenE b = filterWhenE (fmap const b)

whenCondE :: Behavior a -> (a -> Bool) -> Event b -> Event a 
whenCondE b p e = whenE (fmap p b) (snapshotE b e)

constE :: a -> Event b -> Event a
constE x = fmap (const x)

genIntegralB :: (Num t, Num a, Num b) => ((t, a) -> (t, a) -> b -> b) -> Event x -> Behavior t -> b -> Behavior a -> BehaviorGen b
genIntegralB ns tick time start fun = mkBG $ do
    val <- liftIO $ newIORef start
    prev <- liftIO $ newIORef (0, 0)
    let addPoint = do
        new <- liftM2 (,) (openBehavior time) (openBehavior fun)
        liftIO $ ns <$> readIORef prev <*> return new <*> readIORef val >>= writeIORef val >> writeIORef prev new
    let io = addPoint >> liftIO (readIORef val)
    h <- liftIO $ io `seq` mkH io $ \_ -> addPoint
    liftIO $ addHandler tick h
    return $ io

trapIntegralB :: (Eq a, Fractional a) => Event x -> Behavior a -> a -> Behavior a -> BehaviorGen a
trapIntegralB = genIntegralB trapezoid where
    trapezoid (x1, y1) (x2, y2) p | x1==0 && y1==0 = 0
                                  | otherwise = p + (x2-x1)*(y1+y2)/2

feedbackB :: a -> Behavior a -> BehaviorGen a 
feedbackB x beh = mkBG $ do
    val <- newReactRef x
    return $ do
        updateReactRef val $ openBehavior beh
        readReactRef val

genToE :: (a -> BehaviorGen b) -> Event a -> Event (Behavior b)
genToE f = reactMapE $ openBehavior . f

joinE :: Event (Event a) -> Behavior (Event a)
joinE ee = Behavior $ liftIO $ do
    events <- newIORef []
    handlers <- newIORef [] -- todo prune handlers
    h <- mkH handlers $ \evt -> liftIO $ (readIORef handlers >>= mapM (addHandler evt)) >> modifyIORef events (evt:)
    addHandler ee h
    return $ Event $ \h -> (readIORef events >>= mapM (flip addHandler h)) >> modifyIORef handlers (h:)
        
condChangeE :: Eq a => (a -> a -> Bool) -> a -> Behavior a -> Event b -> Behavior (Event a)
condChangeE c x b e = Behavior $ do
    prev <- newReactRef x
    return $ flip alterE e $ alterH $ \h _ -> do
        pv <- readReactRef prev 
        v <- openBehavior b
        when (v `c` pv) $ writeReactRef prev v >> h v
    
changeE :: Eq a => a -> Behavior a -> Event b -> Behavior (Event a)
changeE = condChangeE (/=)

