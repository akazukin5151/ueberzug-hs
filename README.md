# ueberzug-hs

[![Hackage](https://img.shields.io/badge/Hackage-blueviolet?logo=haskell)](https://hackage.haskell.org/package/ueberzug)

[![Documentation](https://img.shields.io/static/v1?label=Documentation&message=Available&color=success)](https://hackage.haskell.org/package/ueberzug/docs/Graphics-Ueberzug.html)

Haskell bindings for ueberzug to display images in the terminal

Make sure ueberzug is installed in the system path.

Note: ueberzug only works on Linux

Port of [Ueberzug-rs](https://github.com/Adit-Chauhan/Ueberzug-rs/)

## Examples

This draws the example image for 2 seconds, then clear it and wait for 1 second before exiting

```hs
import Control.Concurrent
import Graphics.Ueberzug

main = do
  ub <- newUeberzug
  -- assert it suceeded
  Right () <-
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

  clear ub "75933779_p0"
  threadDelay 1000000
```
This draws the example image for 1 second, draws another one next to it for 1 second, clear the first image, wait 1 second, then clear the second image, and wait 1 second before exiting

```hs
main = do
  ub <- newUeberzug
  draw ub $ defaultUbConf
    { identifier = "75933779_p0_0"
    , path = "test/75933779_p0.jpg"
    , x = 10
    , y = 2
    , width = Just 10
    , height = Just 10
    , scaler = Just FitContain
    }
  threadDelay 1000000
  draw ub $ defaultUbConf
    { identifier = "75933779_p0_1"
    , path = "test/75933779_p0.jpg"
    , x = 20
    , y = 2
    , width = Just 10
    , height = Just 10
    , scaler = Just FitContain
    }
  threadDelay 1000000
  clear ub "75933779_p0_0"
  threadDelay 1000000
  clear ub "75933779_p0_1"
  threadDelay 1000000
```

Note that CI tests will fail on Hackage because there is no tty attached. They only work if manually ran.

## License

MIT
