{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Maybe (mapMaybe)
import Data.Foldable (Foldable, foldrM, forM_)
import Data.Traversable (mapAccumL)
import Data.List (unfoldr)
import Data.Array ((!))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Text.Regex.Base.RegexLike (RegexLike)
import qualified Text.Regex.TDFA as TDFA (Regex, makeRegex, matchAllText)
import qualified Text.Regex.TDFA.UTF8

import qualified Data.ByteString.Lazy.Search as BoyerMoore (nonOverlappingIndices)

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.Chan
import Control.DeepSeq (force)

import System.Environment (getArgs)
import System.Directory.Tree
import Text.Printf (printf)

type MatchData = [(BL.ByteString, BL.ByteString)]
type MatchFunc = BL.ByteString -> Maybe MatchData
type MatchedLine = (Int, MatchData)
type Line = (Int, BL.ByteString)

buildFileReader :: (FilePath -> IO BL.ByteString) -> FilePath -> IO (FilePath, BL.ByteString)
buildFileReader reader path =
  do
    file <- reader path
    return (path, file)

lazyReader :: FilePath -> IO (FilePath, BL.ByteString)
lazyReader = buildFileReader BL.readFile

regexMatcher :: RegexLike regex BL.ByteString => regex -> MatchFunc
regexMatcher needle haystack =
  let
      ms = TDFA.matchAllText needle haystack
      ms' = map (!0) ms
      haystackLength = BL.length haystack
      haystackSuffix i = BL.drop (fromIntegral i) haystack
      haystackSlice i n = BL.take (fromIntegral n) (haystackSuffix i)
      combine i (m,(j,n)) = (j+n, (haystackSlice i (j - i), m))
      (iLast, tmp) = mapAccumL combine 0 ms'
      result = if (haystackLength == fromIntegral iLast)
               then tmp
               else tmp ++ [(haystackSuffix iLast, BL.empty)]
  in
      case ms of
        [] -> Nothing
        _  -> Just result

boyerMooreMatcher :: B.ByteString -> MatchFunc
boyerMooreMatcher needle haystack =
  case occurences of
    [] -> Nothing
    _  -> Just result
  where
    indices = BoyerMoore.nonOverlappingIndices needle
    needleLength = B.length needle
    lazyNeedle = BL.fromStrict needle

    occurences = map fromIntegral $ indices haystack
    haystackLength = BL.length haystack
    haystackSuffix i = BL.drop (fromIntegral i) haystack
    haystackSlice i n = BL.take (fromIntegral n) (haystackSuffix i)
    combine i j = (j + needleLength, (haystackSlice i (j - i), lazyNeedle))
    (iLast, tmp) = mapAccumL combine 0 occurences
    result = if (haystackLength == fromIntegral iLast)
             then tmp
             else tmp ++ [(haystackSuffix iLast, BL.empty)]

matchLine :: MatchFunc -> Line -> Maybe MatchedLine
matchLine finder (n, line) = finder line >>= return . ((,) n)

matchLines :: MatchFunc -> BL.ByteString -> [MatchedLine]
matchLines finder haystack =
  let lines = [1..] `zip` (UTF8.lines haystack)
  in mapMaybe (matchLine finder) lines

highlight s =
  let
    hBegin = UTF8.fromString "\x1b[30;43m"
    hEnd   = UTF8.fromString "\x1b[0m"
  in
    if s == BL.empty
    then s
    else hBegin `BL.append` s `BL.append` hEnd

formatMatchedLine :: MatchedLine -> String
formatMatchedLine (n, ms) =
  printf "%i: %s" n (UTF8.toString h)
  where
    h = BL.concat $ map (\(s,t) -> s `BL.append` (highlight t)) ms

interpretArgs [needle, path] = (UTF8.fromString needle, path)
interpretArgs [needle]       = interpretArgs [needle, "./"]
interpretArgs []             = interpretArgs ["foo"]

perform :: (Foldable f) => f (FilePath, BL.ByteString) -> BL.ByteString -> IO ()
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
    {- regex = TDFA.makeRegex (Text.Regex.TDFA.UTF8.Utf8 pattern) :: TDFA.Regex -}
    {- finder = regexMatcher regex -}
    needle = BL.toStrict pattern
    finder = boyerMooreMatcher needle

main :: IO ()
main = do
  (needle, path) <- getArgs >>= return . interpretArgs
  (_ :/ dirTree) <- readDirectoryWith lazyReader path
  async (perform dirTree needle) >>= wait
