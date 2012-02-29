module Control.DysFRP.Craftwerk(
    drawFigureOnDrawable, 
    render, renderB,
    compositionB, rotateB, translateB, translate2B, scaleB, scale2B
) where

import qualified Control.DysFRP as R
import Control.DysFRP.Cairo
import Data.IORef
import Control.Applicative
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.Craftwerk.Core
import Graphics.Craftwerk.UI.Gtk hiding (RenderContext)
import qualified Graphics.Rendering.Cairo as C
import Foreign.Ptr
import qualified Data.Map as Map
import Data.Traversable

drawFigureOnDrawable :: DrawableClass d => Double -> Double -> d -> Figure -> IO ()
drawFigureOnDrawable px py srf fig = drawContextOnDrawable srf (render px py fig)

renderB :: Double -> Double -> R.Behavior Figure -> R.Behavior (RenderContext)
renderB px py = fmap (render px py)

render :: Double -> Double -> Figure -> RenderContext
render px py fig = cairo (renderFigure px py (\_ -> return fig)) Map.empty

compositionB :: [R.Behavior Figure] -> R.Behavior Figure
compositionB = fmap composition . sequenceA

rotateB :: R.Behavior Double -> R.Behavior Figure -> R.Behavior Figure
rotateB = liftA2 rotate

translateB :: R.Behavior Vector -> R.Behavior Figure -> R.Behavior Figure
translateB = liftA2 translate

translate2B :: R.Behavior Double -> R.Behavior Double -> R.Behavior Figure -> R.Behavior Figure
translate2B x y = translateB (liftA2 (,) x y)

scaleB :: R.Behavior Vector -> R.Behavior Figure -> R.Behavior Figure
scaleB = liftA2 scale

scale2B :: R.Behavior Double -> R.Behavior Double -> R.Behavior Figure -> R.Behavior Figure
scale2B x y = scaleB (liftA2 (,) x y)

