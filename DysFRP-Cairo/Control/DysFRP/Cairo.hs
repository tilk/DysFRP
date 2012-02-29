module Control.DysFRP.Cairo(GtkEvent, mkTickerE, 
    addRefresher, addBufRefresher, addDrawingAreaRefresher,
    addDrawingAreaRefresherG,
    addTick,
    reactiveOn, eventData, 
    prerenderBG,
    concatContextB,
    RenderContext, mkContext,
    keyValE, keyNameE, modifierE, keyValModifierE, keyLeftE, keyRightE, keyUpE, keyDownE) where

import qualified Control.DysFRP.Internal as R
import Data.IORef
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Reader
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import qualified Graphics.Rendering.Cairo as C
import Foreign.Ptr
import Data.Traversable

data GtkEvent a = GtkEvent { fromGtkEvent :: Ptr a }

type RenderContext = Double -> Double -> IO (C.Render ())

zeroRenderContext = \_ _ -> return (return ())
appendRenderContext c1 c2 = \a b -> liftM2 (>>) (c1 a b) (c2 a b)
concatContext = foldr appendRenderContext zeroRenderContext
mkContext c = \_ _ -> return c

drawContextOnSurface :: C.Surface -> RenderContext -> IO ()
drawContextOnSurface srf fig = do
    w <- C.imageSurfaceGetWidth srf
    h <- C.imageSurfaceGetHeight srf
    rnd <- fig (fromIntegral w) (fromIntegral h)
    C.renderWith srf rnd

drawContextOnDrawable :: DrawableClass d => d -> RenderContext -> IO ()
drawContextOnDrawable srf fig = do
    (w, h) <- drawableGetSize srf
    rnd <- fig (fromIntegral w) (fromIntegral h)
    renderWithDrawable srf rnd

mkTickerE :: Int -> IO (HandlerId, R.Event ())
mkTickerE k = do
    (f, e) <- R.mkE
    c <- timeoutAdd (f () >> return True) k
    return (c, e)

addRefresher :: DrawableClass d => Int -> d -> R.Behavior RenderContext -> IO HandlerId
addRefresher k srf fig = flip timeoutAdd k $ R.runBehavior fig >>= drawContextOnDrawable srf >> return True

addBufRefresher :: DrawableClass d => Int -> d -> R.Behavior RenderContext -> IO HandlerId
addBufRefresher k srf2 fig = do
    (mx, my) <- drawableGetSize srf2
    srf1 <- pixmapNew (Just srf2) mx my Nothing
    gc <- gcNew srf2
    flip timeoutAdd k $ R.runBehavior fig >>= drawContextOnDrawable srf1 >> do
        drawDrawable srf2 gc srf1 0 0 0 0 (-1) (-1) >> return True

addDrawingAreaRefresher k drawingarea fig = do
    canvas <- widgetGetDrawWindow drawingarea
    (mx, my) <- {-fmap fixSize $-} drawableGetSize canvas
    srfp <- newIORef =<< pixmapNew (Just canvas) mx my Nothing
    gc <- gcNew canvas
{-    on drawingarea configureEvent $ do
        (x, y) <- fmap fixSize $ eventSize
        liftIO $ print (x,y)
        liftIO $ writeIORef srfp =<< pixmapNew (Just canvas) x y Nothing
        return True-}
    flip timeoutAdd k $ do
        srf <- readIORef srfp
        drawContextOnDrawable srf =<< R.runBehavior fig
        drawDrawable canvas gc srf 0 0 0 0 (-1) (-1) >> return True
{-    where
    fixSize (a, b) | fromIntegral a/(px :: Double)*py > fromIntegral b = 
                                   (round $ fromIntegral b/py*px, b)
                   | otherwise   = (a, round $ fromIntegral a/px*py)-}

addDrawingAreaRefresherG k drawingarea fig = addDrawingAreaRefresher k drawingarea =<< R.runBehavior fig

addTick :: Int -> IO (R.Event ())
addTick k = do
    (f, e) <- R.mkE
    flip timeoutAdd k $ f () >> return True
    return e

reactiveOn :: a -> Signal a (EventM b Bool) -> IO (ConnectId a, R.Event (GtkEvent b))
reactiveOn obj sig = do
    (f, e) <- R.mkE 
    c <- on obj sig $ ask >>= liftIO . f . GtkEvent >> return True
    return (c, e)

prerenderBG :: (Int,Int) -> RenderContext -> R.Event RenderContext -> R.BehaviorGen RenderContext
prerenderBG (w,h) fig e = R.mkBG $ liftIO $ do
    pixmap <- C.createImageSurface C.FormatARGB32 w h
    drawContextOnSurface pixmap fig
    let io = do
        C.setSourceSurface pixmap 0 0
        C.rectangle 0 0 (fromIntegral w) (fromIntegral h)
        C.fill 
    hn <- io `seq` R.mkH io $ liftIO . drawContextOnSurface pixmap
    R.addHandler e hn
    return $ return $ mkContext io

eventData :: EventM a b -> R.Event (GtkEvent a) -> R.Event b
eventData e = R.ioMapE $ runReaderT e . fromGtkEvent

concatContextB :: [R.Behavior RenderContext] -> R.Behavior RenderContext
concatContextB = fmap concatContext . sequenceA

keyValE :: R.Event (GtkEvent EKey) -> R.Event KeyVal
keyValE = eventData eventKeyVal

keyNameE :: R.Event (GtkEvent EKey) -> R.Event String
keyNameE = eventData eventKeyName

modifierE :: R.Event (GtkEvent EKey) -> R.Event [Modifier]
modifierE = eventData eventModifier

keyValModifierE :: R.Event (GtkEvent EKey) -> R.Event (KeyVal, [Modifier])
keyValModifierE = eventData (liftM2 (,) eventKeyVal eventModifier)

keyLeftE :: R.Event (GtkEvent EKey) -> R.Event ()
keyLeftE = fmap (const ()) . R.filterE (== 0xff51) . keyValE

keyRightE :: R.Event (GtkEvent EKey) -> R.Event ()
keyRightE = fmap (const ()) . R.filterE (== 0xff53) . keyValE

keyUpE :: R.Event (GtkEvent EKey) -> R.Event ()
keyUpE = fmap (const ()) . R.filterE (== 0xff52) . keyValE

keyDownE :: R.Event (GtkEvent EKey) -> R.Event ()
keyDownE = fmap (const ()) . R.filterE (== 0xff54) . keyValE

