{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Monad
import qualified Data.Map.Lazy as M
import System.Random (randomRIO)
import System.IO
import Data.List.Split
import Data.List (last)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S

import System.Directory (doesFileExist)
import qualified Data.ByteString as BS
import Data.Serialize

type MarkovMap = M.Map InputType (S.Set String)
type InputType = [String]

getMapping :: MarkovMap -> InputType -> IO (Maybe String)
getMapping mMap input = do
  let maybeResult = M.lookup input mMap
  case maybeResult of
    Nothing -> return Nothing
    Just res -> sample res

sample :: S.Set a -> IO (Maybe a)
sample list = if S.null list then return Nothing
              else randomRIO (0, length list - 1) >>= return . Just . (`S.elemAt` list)

genMap :: String -> MarkovMap
genMap lexicon =
  let ws = words lexicon
      chains = getChains ws
  in foldl (\acc v ->
              let key = fst v
                  val = snd v
              in if M.member key acc
                    then M.adjust (\set -> S.union set (S.singleton val)) key acc
                    else M.insert key (S.singleton val) acc)
           M.empty
           chains

getChains :: [String] -> [(InputType, String)]
getChains list =
  zip (zipWith3 (\a b c -> [a,b,c])
                (rotate 0 list)
                (rotate 1 list)
                (rotate 2 list))
      (rotate 3 list)

rotate :: Int -> [a] -> [a]
rotate n  l = case l of [] -> []; list -> let f = take n list in drop n list ++ f

genString :: MarkovMap -> InputType -> IO String
genString mMap input = go [input !! 2, input !! 1, input !! 0]
  where go :: [String] -> IO String
        go acc = do
          let newInput = reverse $ take 3 acc
          maybeMapping <- getMapping mMap newInput
          if | isNothing maybeMapping -> return $ unwords $ reverse acc
             | last (fromJust maybeMapping) == '.' -> return $ unwords $ reverse $ fromJust maybeMapping : acc
             | otherwise -> go $ fromJust maybeMapping : acc

main :: IO ()
main =  withFile "lexicon.txt" ReadMode $ \h -> do
  lexiContents <- hGetContents h
  mapFileExists <- doesFileExist "markov.cache"
  markovMap <- if mapFileExists
               then do putStrLn "Reading cache.."
                       c <- BS.hGetContents =<< openFile "markov.cache" ReadMode
                       let mMap =
                             case decode c of
                               Left e -> error e
                               Right m -> m
                       return mMap
               else do putStrLn "Building map and writing cache.."
                       let mMap = genMap lexiContents
                       openFile "markov.cache" WriteMode >>= \h -> BS.hPut h (encode mMap)
                       return mMap
  putStrLn "loading done. please type"
  forever $ do
    c <- getLine
    let ws = words c
    print =<< genString markovMap (take 3 ws)
