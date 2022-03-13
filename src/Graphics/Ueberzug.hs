module Graphics.Ueberzug
    ( Ueberzug ()
    , newUeberzug
    , draw
    , clear
    , Actions (..)
    , Scalers (..)
    , UbConf (..)
    , defaultUbConf
    , toJson
    ) where

import System.Process (createProcess, proc, CreateProcess (std_in, std_out), StdStream (CreatePipe))
import Data.Maybe (fromJust)
import GHC.IO.Handle (hPutStr, Handle, hFlush)
import Control.Exception (tryJust, IOException)
import System.IO.Error (isFullError, isPermissionError)

-- | The main struct storing a handle to the ueberzug process
newtype Ueberzug = Ueberzug {process :: Maybe Handle}

-- | Create a new Ueberzug instance with an empty process handle
newUeberzug :: Ueberzug
newUeberzug = Ueberzug { process = Nothing }

-- | Draw an image using the @ub@ instance with config @config@
draw :: Ueberzug -> UbConf -> IO (Either String Ueberzug)
draw ub config =
  case toJson config of
    Right cmd -> run ub cmd
    Left  xx  -> pure (Left xx)

-- | Clear an image with identifier @identifier_@. The @ub@ instance
-- | should be the same one that @draw@ returned, when drawing that image.
clear :: Ueberzug -> String -> IO (Either String Ueberzug)
clear ub identifier_ = do
  case toJson config of
    Right cmd -> run ub cmd
    Left  xx  -> pure (Left xx)
  where
    config = defaultUbConf { action = Remove, identifier = identifier_ }

hExceptions :: IOException -> Maybe String
hExceptions e =
  case e of
    ex | isFullError ex       -> Just "Device is full"
    ex | isPermissionError ex -> Just "Permission Error"
    _                         -> Nothing

-- | Pipe the command @cmd@ to the process in @ub@
run :: Ueberzug -> String -> IO (Either String Ueberzug)
run ub cmd = do
  stdin <- stdin_h
  a <- tryJust hExceptions (hPutStr stdin cmd)
  case a of
    Left e -> pure $ Left e
    _ -> do
      b <- tryJust hExceptions (hFlush stdin)
      case b of
        Left e -> pure $ Left e
        _ -> pure $ Right $ Ueberzug { process = Just stdin }

  where
    fst4 (a, _, _, _) = a
    created_stdin_h =
      fst4 <$>
        createProcess (proc "ueberzug" ["layer", "--silent"])
          { std_in = CreatePipe
          , std_out = CreatePipe
          }
    stdin_h =
      case process ub of
        Nothing -> fromJust <$> created_stdin_h
        Just a -> pure a

-- | The available actions on the ueberzug image
data Actions = Add | Remove

-- | The available scalers to scale the image
data Scalers = Crop
             | Distort
             | FitContain
             | Contain
             | ForcedCover
             | Cover

instance Show Scalers where
  show Crop        = "crop"
  show Distort     = "distort"
  show FitContain  = "fit_contain"
  show Contain     = "contain"
  show ForcedCover = "forced_cover"
  show Cover       = "cover"

-- | The ueberzug configuration record. Contains all the data needed to draw the image
data UbConf =
  UbConf
    { action             :: Actions
    , identifier         :: String
    , x                  :: Int
    , y                  :: Int
    , path               :: FilePath
    , width              :: Maybe Int
    , height             :: Maybe Int
    , scaler             :: Maybe Scalers
    , should_draw        :: Maybe Bool
    , synchronously_draw :: Maybe Bool
    , scaling_position_x :: Maybe Float
    , scaling_position_y :: Maybe Float
    }

-- | Default config with approx. "mempty" values for convenience
defaultUbConf :: UbConf
defaultUbConf =
  UbConf
    { action             = Add
    , identifier         = ""
    , x                  = 0
    , y                  = 0
    , path               = ""
    , width              = Nothing
    , height             = Nothing
    , scaler             = Nothing
    , should_draw        = Nothing
    , synchronously_draw = Nothing
    , scaling_position_x = Nothing
    , scaling_position_y = Nothing
    }

-- | Converts the ueberzug config to JSON, so that it can be passed into
-- | the ueberzug process
toJson :: UbConf -> Either String String
toJson conf = do
  iden <-
    case identifier conf of
      "" -> Left "Incomplete Information : Identifier Not Found"
      a -> pure a
  case action conf of
    Remove ->
      pure ("{\"action\":\"remove\",\"identifier\":\"" <> iden <> "\"}\n")
    Add -> do
      path_ <-
        case path conf of
           "" -> Left "Incomplete Information : Path Not Found"
           a -> pure a
      pure $ jsn path_ iden
  where
    jsn path_ iden =
        "{\"action\": \"add"
        <> "\", \"path\": \"" <> path_
        <> "\", \"identifier\": \"" <> iden
        <> "\", \"x\": \"" <> show (x conf)
        <> "\", \"y\": \"" <> show (y conf)
        <> ifJust "width" (width conf)
        <> ifJust "height" (height conf)
        <> ifJust "scaler" (scaler conf)
        <> ifJust "draw" (should_draw conf)
        <> ifJust "sync" (synchronously_draw conf)
        <> ifJust "scaling_position_x" (scaling_position_x conf)
        <> ifJust "scaling_position_y" (scaling_position_y conf)
        <> "\"}\n"

ifJust :: Show a => String -> Maybe a -> String
ifJust name = maybe "" (\a -> "\", \"" <> name <> "\": \"" <> show a)
