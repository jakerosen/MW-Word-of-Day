module Main where

import Control.Monad (when)
import Data.Foldable
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable
import System.Environment (getArgs)
import System.Exit
import Text.HTML.Scalpel
import Text.Printf (printf)
import Text.Read (readMaybe)

-- earliestDate = "2006-08-31"

baseUrl :: URL
baseUrl =
  "https://www.merriam-webster.com/word-of-the-day/"

main :: IO ()
main = do
  args <- getArgs
  let argsAsNum :: [Int]
      argsAsNum = catMaybes (map readMaybe args)
      year = head argsAsNum
      month = last argsAsNum

  when (length argsAsNum /= 2) usageFail
  when (month < 1 || month > 12) usageFail

  let days = [1 .. numDaysInMonth year month]
      urls = map (dateToUrl year month) days

  result <- catMaybes <$> for urls extractWord

  for_ result (putStrLn . Text.unpack)

usageFail :: IO ()
usageFail = do
  putStrLn "usage: wotd <year> <month>"
  exitFailure

extractWord :: URL -> IO (Maybe Text)
extractWord url = scrapeURL url scraper
  where
    -- The word happens to be in the only h1 element, so I assume that and just
    -- grab it
    selectWord = "h1"
    scrapeWord = text "h1"
    scraper = chroot selectWord scrapeWord

type Year = Int

type Month = Int

type Day = Int

numDaysInMonth :: Year -> Month -> Int
numDaysInMonth year = \case
  1 -> 31
  2 -> if leapYear then 29 else 28
  3 -> 31
  4 -> 30
  5 -> 31
  6 -> 30
  7 -> 31
  8 -> 31
  9 -> 30
  10 -> 31
  11 -> 30
  12 -> 31
  _ -> error "invalid month"
  where
    leapYear = year `mod` 4 == 0

dateToUrl :: Year -> Month -> Day -> URL
dateToUrl = printf "%s%04d-%02d-%02d" baseUrl
