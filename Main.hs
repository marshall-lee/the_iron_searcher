module Main where

import Data.Maybe (mapMaybe)
import Data.Foldable (Foldable, foldrM, forM_)
import Data.Traversable (mapAccumL)
import Data.List (unfoldr)
import Data.Array ((!))

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Text.Regex.TDFA as TDFA (Regex, makeRegex, matchAllText)
import qualified Text.Regex.TDFA.UTF8

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.Chan
import Control.DeepSeq (force)

import System.Environment (getArgs)
import System.Directory.Tree
import Text.Printf (printf)

type MatchData = [(B.ByteString, B.ByteString)]
type MatchFunc = B.ByteString -> Maybe MatchData
type MatchedLine = (Int, MatchData)
type Line = (Int, B.ByteString)

buildFileReader :: (FilePath -> IO B.ByteString) -> FilePath -> IO (FilePath, B.ByteString)
buildFileReader reader path =
  do
    file <- reader path
    return (path, file)

lazyReader :: FilePath -> IO (FilePath, B.ByteString)
lazyReader = buildFileReader B.readFile

regexMatcher needle haystack =
  let
      ms = TDFA.matchAllText needle haystack
      ms' = map (!0) ms
      haystackLength = B.length haystack
      haystackSuffix i = B.drop (fromIntegral i) haystack
      haystackSlice i n = B.take (fromIntegral n) (haystackSuffix i)
      combine i (m,(j,n)) = (j+n, (haystackSlice i (j - i), m))
      (iLast, tmp) = mapAccumL combine 0 ms'
      result = if (haystackLength == fromIntegral iLast)
               then tmp
               else tmp ++ [(haystackSuffix iLast, B.empty)]
  in
      case ms of
        [] -> Nothing
        _  -> Just result

matchLine :: MatchFunc -> Line -> Maybe MatchedLine
matchLine finder (n, line) = finder line >>= return . ((,) n)

matchLines :: MatchFunc -> B.ByteString -> [MatchedLine]
matchLines finder haystack =
  let lines = [1..] `zip` (UTF8.lines haystack)
  in mapMaybe (matchLine finder) lines

highlight s =
  let
    hBegin = UTF8.fromString "\x1b[30;43m"
    hEnd   = UTF8.fromString "\x1b[0m"
  in
    if s == B.empty
    then s
    else hBegin `B.append` s `B.append` hEnd

formatMatchedLine :: MatchedLine -> String
formatMatchedLine (n, ms) =
  printf "%i: %s" n (UTF8.toString h)
  where
    h = B.concat $ map (\(s,t) -> s `B.append` (highlight t)) ms

interpretArgs [needle, path] = (UTF8.fromString needle, path)
interpretArgs [needle]       = interpretArgs [needle, "./"]
interpretArgs []             = interpretArgs ["foo"]

perform :: (Foldable f) => f (FilePath, B.ByteString) -> B.ByteString -> IO ()
perform files pattern = do
  fChannel <- newChan
  let
    spawnSearch (path, file) tasks = do
      t <- async (doMatch path file)
      return (t : tasks)

    doMatch path file = case matches of
                          (m : matches') -> do mChannel <- startWithFile path
                                               reportFileMatch mChannel m
                                               forM_ (force matches') $ reportFileMatch mChannel
                                               endWithFile mChannel
                          [] -> return ()
                        where matches = matchLines finder file

    startWithFile path = do mChannel <- newChan
                            writeChan fChannel (Just (path, mChannel))
                            return mChannel

    reportFileMatch mChannel m = writeChan mChannel (Just m)

    endWithFile mChannel = writeChan mChannel Nothing

    printMatches mChannel = printMatches'
      where printMatches' = readChan mChannel >>= printMatch
            printMatch (Just m) = do putStrLn $ formatMatchedLine m
                                     printMatches'
            printMatch Nothing = putChar '\n'
    printResultsLoop = readChan fChannel >>= printResults
      where printResults (Just (path, mChannel)) =
              do printf "%s:\n" path
                 printMatches mChannel
                 printResultsLoop
            printResults Nothing = return ()

  tasks <- foldrM spawnSearch [] files
  printTask <- async printResultsLoop
  forM_ tasks wait
  writeChan fChannel Nothing
  wait printTask
  where
    regex = TDFA.makeRegex (Text.Regex.TDFA.UTF8.Utf8 pattern) :: TDFA.Regex
    finder = regexMatcher regex

main :: IO ()
main = do
  (needle, path) <- getArgs >>= return . interpretArgs
  (_ :/ dirTree) <- readDirectoryWith lazyReader path
  perform dirTree needle
