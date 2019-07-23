module Caman
  ( RGB
  , RGBA
  , Channels(..)
  , Point
  , CurvePoints
  , ProcessFn
  , ProcessPoint
  , Processor(..)
  , getPixelRelative
  , getPixel
  , putPixelRelative
  , putPixel
  , register
  , Filter(..)
  , Blender(..)
  , Size
  , LayerOption(..)
  , Layer(..)
  , render
  )
where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Data.String (joinWith)
import Data.Array (index)
import Data.Maybe (fromMaybe)

foreign import data ProcessPoint :: Type

--| Is used for defining red, green and blue values of a pixel
type RGB =
  { red :: Int
  , green :: Int
  , blue :: Int
  }

--| Is used for defining red, green, blue and alpha values for a pixel
type RGBA = 
  { red :: Int
  , green :: Int
  , blue :: Int
  , alpha :: Int
  }

--| Defining the channels of a pixel or image
data Channels 
  = Red
  | Green
  | Blue

instance showChannels :: Show Channels where
  show = case _ of
    Red -> "r"
    Green -> "g"
    Blue -> "b"

--| Defining a point in a Curve Points type
type Point =
  { x :: Int
  , y :: Int
  }

--| Defining a bezier curve 
type CurvePoints =
  { p0 :: Point
  , p1 :: Point
  , p2 :: Point
  , p3 :: Point
  }

--| Processor Function to modify a given point and its surrounding points
--| The function has to return the modified pixel red, green and blue value
type ProcessFn = Int -> ProcessPoint -> RGB -> Effect RGB

--| Processors define the Categories of diffrent image functinalities
--|
--| PointProcessor 
--|   is a processor which defines a function how to 
--|   modify the pixel and its surrounding
--|
--| Kernel 
--|   define a simple matrix (1 dimensional array) that describes 
--|   how to modify a certain pixel based on the ones around it. 
--|   The GIMP documentation does a great job at explaining this. 
data Processor 
  = PointProcessor 
    { name :: String 
    , function :: ProcessFn
    }
  | Kernel
    { name :: String
    , matrix :: Array Int
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

--| Used in a PointProcessor. This function returns the pixel on the given
--| relative position
getPixelRelative :: ProcessPoint -> Point -> Effect RGBA
getPixelRelative pp p = do 
  rgba <- ppFunctionWrapper pp "getPixelRelative" [toInt p.x, toInt p.y]
  pure 
    { red: fromMaybe 0 $ index rgba 0
    , green: fromMaybe 0 $ index rgba 1
    , blue: fromMaybe 0 $ index rgba 2
    , alpha: fromMaybe 0 $ index rgba 3
    }

--| Used in a PointProcessor. Gets the current pixel.
getPixel:: ProcessPoint -> Point -> Effect RGBA
getPixel pp p = do 
  rgba <- ppFunctionWrapper pp "getPixel" [toInt p.x, toInt p.y]
  pure 
    { red: fromMaybe 0 $ index rgba 0
    , green: fromMaybe 0 $ index rgba 1
    , blue: fromMaybe 0 $ index rgba 2
    , alpha: fromMaybe 0 $ index rgba 3
    }

--| Used in a PointProcessor. Sets the pixel on the given relative position
putPixelRelative :: ProcessPoint -> Point -> RGBA -> Effect Unit
putPixelRelative pp p rgba = do
  _ <- ppFunctionWrapper pp "putPixelRelative" 
    [ toInt p.x
    , toInt p.y
    , toInt rgba.red
    , toInt rgba.green
    , toInt rgba.blue
    , toInt rgba.alpha
    ]
  pure unit

--| Used in a PointProcessor. Sets the current pixel
putPixel :: ProcessPoint -> Point -> RGBA -> Effect Unit
putPixel pp p rgba = do
  _ <- ppFunctionWrapper pp "putPixel" 
    [ toInt p.x
    , toInt p.y
    , toInt rgba.red
    , toInt rgba.green
    , toInt rgba.blue
    , toInt rgba.alpha
    ]
  pure unit

foreign import processKernelImpl :: String -> Array Int -> Effect Unit

--| Used with a Processor it registers a new filter with a given name.
--| The filter could be called via the Filter CustomFilter ADT
register :: Processor -> Effect Unit
register (PointProcessor { name, function }) = 
  registerImpl name
    \pp adjust r g b -> do
      rgb <- function adjust pp { red: r, green: g, blue: b }
      pure [rgb.red, rgb.green, rgb.blue]
register (Kernel { name, matrix }) =
  processKernelImpl name matrix

--| Blending Modes for a new layer. That can be used in a NewLayer rendering
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

--| Size is used for two filters Resize and Crop
type Size =
  { width :: Int
  , height :: Int
  }

--| Built in filter functions that can be used in render function.
data Filter
  = Brightness Int
  | Revert
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

toInt :: Int -> String
toInt i = "parseInt(" <> show i <> ")"

toString :: String -> String
toString s = "'" <> s <> "'"

toFloat :: Number -> String
toFloat f = "parseFloat(" <> show f <> ")"

toArray :: forall a. Show a => Array a -> String
toArray a = "[" <> (joinWith "," $ map show a) <> "]"

convertFilterArray :: Filter -> Array String
convertFilterArray = case _ of
  Revert -> ["revert"]
  Brightness i -> ["brightness", toInt i]
  Contrast i -> ["contrast", toInt i]
  Sepia i -> ["sepia", toInt i]
  Saturation i -> ["saturation", toInt i]
  Exposure i -> ["exposure", toInt i]
  Gamma n -> ["gamma", toFloat n]
  Greyscale -> ["greyscale"]
  Invert -> ["invert"]
  Hue i -> ["hue", toInt i]
  Noise i -> ["noise", toInt i]
  Vibrance i -> ["vibrance", toInt i]
  Resize s -> ["resize", toInt s.width, toInt s.height]
  Crop s -> ["crop", toInt s.width, toInt s.height]
  Curves channels curve -> 
    ["curves"
    , toString (joinWith "" $ map show channels)
    , toArray 
      [ curve.p0.x
      , curve.p0.y
      ]
    , toArray
      [ curve.p1.x
      , curve.p1.y
      ]
    , toArray
      [ curve.p2.x
      , curve.p2.y
      ]
    , toArray
      [ curve.p3.x
      , curve.p3.y
      ]
    ]
  Channels { red:r, green:g, blue:b } -> 
    ["channels"] <> map toInt [ r, g, b ]
  Colorize { red, green, blue } strength -> 
    ["colorize"] <> map toInt [ red, green, blue, strength ]
  ColorizeHex hex strength ->
    ["colorize"
    , toString hex
    , toInt strength
    ]
  CustomFilter name -> 
    [ name ] 

--| Layer Options are used in a New layer to do adjustments before 
--| modifing the new layer with filters
data LayerOption
  = BlendingMode Blender
  | FillColor String
  | CopyParent 
  | Opacity Int

--| Layer types that can be rendered
--| MainLayer is the Image itself
--| NewLayer is a new layer that can be arranged with LayerOptions 
--| and modified by filters like the main layer
data Layer 
  = MainLayer
    { elementId :: String
    , filters :: Array Filter
    }
  | NewLayer
    { elementId :: String
    , filters :: Array Filter
    , layerOptions :: Array LayerOption
    }

convertLayerOptionArray :: LayerOption -> Array String
convertLayerOptionArray = case _ of
  BlendingMode b -> ["setBlendingMode", toString $ show b]
  FillColor c -> ["fillColor", toString c]
  CopyParent -> ["copyParent"]
  Opacity i -> ["opacity", toInt i]

foreign import functionWrapper :: String -> Array (Array String) -> EffectFnAff Unit
foreign import newLayerImpl :: String -> Array (Array String) -> Array (Array String) -> EffectFnAff Unit

--| Render the filters to a given layer
render :: Layer -> Aff Unit
render (MainLayer { elementId, filters }) = 
  fromEffectFnAff $ functionWrapper elementId $ map convertFilterArray filters
render (NewLayer { elementId, filters, layerOptions }) = 
  fromEffectFnAff $ newLayerImpl elementId (map convertLayerOptionArray layerOptions) (map convertFilterArray filters)
