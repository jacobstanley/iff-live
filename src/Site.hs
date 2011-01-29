{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Data.Maybe
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           IFF


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = ifTop $ heistLocal (bindSplices indexSplices) $ render "index"
  where
    indexSplices = 
        [ ("start-time",   startTimeSplice)
        , ("current-time", currentTimeSplice)
        ]


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" message) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | Renders the announce page.
announce :: Application ()
announce = do
    match <- fromMaybe 0 <$> getParamInt "match"
    goals <- liftIO (scrapeGoals match)
    let splices = [("goals", goalsSplice goals)]
    heistLocal (bindSplices splices) $ render "announce"

goalsSplice :: [Goal] -> Splice Application
goalsSplice = liftM (concat . catMaybes) . mapM renderGoal

renderGoal :: Goal -> TemplateMonad Application (Maybe Template)
renderGoal (Goal g ti s a te) = callTemplate "goal"
    [ ("goal",   B.pack g)
    , ("time",   B.pack ti)
    , ("scorer", B.pack s)
    , ("assist", B.pack a)
    , ("team",   B.pack te)
    ]


getParamInt :: MonadSnap m => ByteString -> m (Maybe Int)
getParamInt p = do
    mstr <- getParam p
    return (decode mstr)
  where
    decode :: Maybe ByteString -> Maybe Int
    decode Nothing  = Nothing
    decode (Just x) = dropBS (B.readInt x)

    dropBS :: Maybe (Int, ByteString) -> Maybe Int
    dropBS Nothing       = Nothing
    dropBS (Just (x, _)) = Just x

------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",             index)
             , ("/echo/:stuff",  echo)
             , ("/announce/:match", announce)
             ]
       <|> fileServe "resources/static"
