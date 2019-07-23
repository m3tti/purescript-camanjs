# Purescript Camanjs Wrapper
Tries to get the whole functionality of Camanjs canvas manipulation library into purescript.

## JS Dependencies
You have to make sure camanjs.full has to be present in your distribution

[CamanJs Homepage](http://camanjs.com)

## Usage

### Applying filters

```purescript
main :: Effect Unit
main = launchAff $ do
    _ <- render 
      $ MainLayer 
        { elementId: "#lena" -- this is the canvas element id
        , filters: 
          [ Brightness 95 
          , Gamma 0.2        
          , Hue 22
          ]
        }
```

### Register filters
```purescript
main :: Effect Unit
main = do
   register 
     $ Kernel 
       { name: "test"
       , matrix: 
          [ 5, 5, 5
          , 5, 5, 5
          , 5, 5, 5
          ] 
       }
```
