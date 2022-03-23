# Changelog

## Version 0.2.0.0

### Changed

- The ueberzug process is now started in `newUeberzug` instead of `draw`
    - `newUeberzug` now has type `IO Ueberzug` instead of `Ueberzug`
    - `draw` and `clear` now return `IO (Either String ())` instead of `IO (Either String Ueberzug)`

## Version 0.1.0.0

- Initial release
