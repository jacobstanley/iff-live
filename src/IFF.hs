module IFF
  ( scrapeGoals
  , Goal (..)
  ) where

import           Network.HTTP
import           Text.HTML.TagSoup

scrapeGoals :: Int -> IO [Goal]
scrapeGoals = scrapeMatch findGoals

findGoals :: [Tag String] -> [Goal]
findGoals tags =
    map parseGoal $
    partitions (~== "<tr>") $
    takeWhile (/= TagComment " Here starts the penalties list. ") $
    dropWhile (/= TagComment " Here starts the goal listing. ") tags

data Goal = Goal
   { goal   :: String
   , time   :: String
   , scorer :: String
   , assist :: String
   , team   :: String
   } deriving (Show)

parseGoal :: [Tag String] -> Goal
parseGoal tags = Goal a b c d e
  where
    (a:b:c:d:e:_) = map f $ partitions (~== "<td>") tags
    f = unwords . words . innerText

------------------------------------------------------------------------------

matchURL :: String
matchURL = "http://www.floorball.org/ottelukooste.asp?sarjaId=&ottelu_id="

scrapeMatch :: ([Tag String] -> a) -> Int -> IO a
scrapeMatch f match = scrape f (matchURL ++ show match)

scrape :: ([Tag String] -> a) -> String -> IO a
scrape f url = do
    body <- openURL url
    return $ f $ parseTags body

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)
