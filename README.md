# ueberzug-hs

Haskell bindings for ueberzug to display images in the terminal

Make sure ueberzug is installed in the system path.

Note: ueberzug only works on Linux

Port of [Ueberzug-rs](https://github.com/Adit-Chauhan/Ueberzug-rs/)

## Example

Draw the example image for 2 seconds, then clear it and wait for 1 second before exiting

```hs
import Control.Concurrent
import Graphics.Ueberzug

main = do
  let ub = newUeberzug
  -- `draw` returns a new Ueberzug object; use the new object in the future
  Right new_ub <-
    draw ub $ defaultUbConf
      { identifier = "75933779_p0"
      , path = "./test/75933779_p0.jpg"
      , x = 10
      , y = 2
      , width = Just 10
      , height = Just 10
      , scaler = Just FitContain
      }
  threadDelay 2000000

  clear new_ub "75933779_p0"
  threadDelay 1000000
```

## License

MIT
