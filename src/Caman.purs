module Caman where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Data.String (joinWith)
import Data.Array (index)
import Data.Maybe (fromMaybe)

foreign import data ProcessPoint :: Type

type RGB =
  { red :: Int
  , green :: Int
  , blue :: Int
  }

type RGBA = 
  { red :: Int
  , green :: Int
  , blue :: Int
  , alpha :: Int
  }

data Channels 
  = Red
  | Green
  | Blue

instance showChannels :: Show Channels where
  show = case _ of
    Red -> "r"
    Green -> "g"
    Blue -> "b"

type Point =
  { x :: Int
  , y :: Int
  }

type CurvePoints =
  { p0 :: Point
  , p1 :: Point
  , p2 :: Point
  , p3 :: Point
  }

type ProcessFn = Int -> ProcessPoint -> RGB -> Effect RGB

data Processor 
  = Processor 
    { name :: String 
    , function :: ProcessFn
    }

foreign import registerImpl 
  :: String 
  -> (ProcessPoint -> Int -> Int -> Int -> Int -> Effect (Array Int))
  -> Effect Unit

foreign import ppFunctionWrapper 
  :: ProcessPoint 
  -> String
  -> Array String
  -> Effect (Array Int)

getPixelRelative :: ProcessPoint -> Point -> Effect RGBA
getPixelRelative pp p = do 
  rgba <- ppFunctionWrapper pp "getPixelRelative" [show p.x, show p.y]
  pure 
    { red: fromMaybe 0 $ index rgba 0
    , green: fromMaybe 0 $ index rgba 1
    , blue: fromMaybe 0 $ index rgba 2
    , alpha: fromMaybe 0 $ index rgba 3
    }

putPixelRelative :: ProcessPoint -> Point -> RGBA -> Effect Unit
putPixelRelative pp p rgba = do
  _ <- ppFunctionWrapper pp "putPixelRelative" 
    [ show p.x
    , show p.y
    , show rgba.red
    , show rgba.green
    , show rgba.blue
    , show rgba.alpha
    ]
  pure unit

putPixel :: ProcessPoint -> Point -> RGBA -> Effect Unit
putPixel pp p rgba = do
  _ <- ppFunctionWrapper pp "putPixel" 
    [ show p.x
    , show p.y
    , show rgba.red
    , show rgba.green
    , show rgba.blue
    , show rgba.alpha
    ]
  pure unit

type ProcessKernel =
  { name :: String
  , matrix :: Array Int
  }

foreign import processKernelImpl :: String -> Array Int -> Effect Unit

processKernel :: ProcessKernel -> Effect Unit
processKernel p = 
  processKernelImpl p.name p.matrix

register :: Processor -> Effect Unit
register (Processor { name, function }) = 
  registerImpl name
    \pp adjust r g b -> do
      rgb <- function adjust pp { red: r, green: g, blue: b }
      pure [rgb.red, rgb.green, rgb.blue]

data Blender 
  = Normal
  | Multiply
  | Screen
  | Overlay
  | Difference
  | Addition
  | Exclusion
  | SoftLight
  | Lighten
  | Darken

instance showBlender :: Show Blender where
  show = case _ of
    Normal -> "normal"
    Multiply -> "multiply"
    Screen -> "screen"
    Overlay -> "overlay"
    Difference -> "difference"
    Addition -> "addition"
    Exclusion -> "exclusion"
    SoftLight -> "softLight"
    Lighten -> "lighten"
    Darken -> "darken"

type Size =
  { width :: Int
  , height :: Int
  }

data Filter
  = Brightness Int
  | Contrast Int
  | Sepia Int
  | Saturation Int
  | Exposure Int
  | Gamma Number
  | Greyscale
  | Invert
  | Noise Int
  | Hue Int
  | Vibrance Int
  | Channels RGB
  | Resize Size
  | Crop Size
  | Colorize RGB Int
  | ColorizeHex String Int
  | Curves (Array Channels) CurvePoints
  | CustomFilter String 

convertFilterArray :: Filter -> Array String
convertFilterArray = case _ of
  Brightness i -> ["brightness", show i]
  Contrast i -> ["contrast", show i]
  Sepia i -> ["sepia", show i]
  Saturation i -> ["saturation", show i]
  Exposure i -> ["exposure", show i]
  Gamma n -> ["gamma", show n]
  Greyscale -> ["greyscale"]
  Invert -> ["invert"]
  Hue i -> ["hue", show i]
  Noise i -> ["noise", show i]
  Vibrance i -> ["vibrance", show i]
  Resize s -> ["resize", show s.width, show s.height]
  Crop s -> ["crop", show s.width, show s.height]
  Curves channels curve -> 
    ["curves"
    , (joinWith "" $ map show channels)
    ] 
    <> map show
    [ curve.p0.x
    , curve.p0.y
    , curve.p1.x
    , curve.p1.y
    , curve.p2.x
    , curve.p2.y
    , curve.p3.x
    , curve.p3.y
    ]
  Channels { red:r, green:g, blue:b } -> 
    ["channels"
    , show r
    , show g
    , show b 
    ]
  Colorize { red, green, blue } strength -> 
    ["colorize"
    , show red
    , show green
    , show blue
    , show strength
    ]
  ColorizeHex hex strength ->
    ["colorize"
    , hex
    , show strength
    ]
  CustomFilter name -> 
    [ name ] 

data LayerOption
  = BlendingMode Blender
  | FillColor String
  | CopyParent 
  | Opacity Int

convertLayerOptionArray :: LayerOption -> Array String
convertLayerOptionArray = case _ of
  BlendingMode b -> ["setBlendingMode", show b]
  FillColor c -> ["fillColor", c]
  CopyParent -> ["copyParent"]
  Opacity i -> ["opacity", show i]

foreign import functionWrapper :: String -> Array (Array String) -> EffectFnAff Unit
foreign import newLayerImpl :: String -> Array (Array String) -> Array (Array String) -> EffectFnAff Unit

render :: String -> Array Filter -> Aff Unit
render elid f = 
  fromEffectFnAff $ functionWrapper elid $ map convertFilterArray f
  
newLayer :: String -> Array LayerOption -> Array Filter -> Aff Unit
newLayer elid lo f =
  fromEffectFnAff $ newLayerImpl elid (map convertLayerOptionArray lo) (map convertFilterArray f)
