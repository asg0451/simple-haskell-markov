{-# LANGUAGE MultiWayIf #-}
module Main where

import Prelude hiding (fst, snd)
import qualified Prelude as P
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

type MarkovMap = M.Map InputTuple (S.Set String)
type InputTuple = (String, String, String)

getMapping :: MarkovMap -> InputTuple -> IO (Maybe String)
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
              let key = P.fst v
                  val = P.snd v
              in if M.member key acc
                    then M.adjust (\set -> S.union set (S.singleton val)) key acc
                    else M.insert key (S.singleton val) acc)
           M.empty
           chains

getChains :: [String] -> [(InputTuple, String)]
getChains list = zip (zip3 (rotate 0 list) (rotate 1 list) (rotate 2 list)) (rotate 3 list)

rotate :: Int -> [a] -> [a]
rotate n  l = case l of [] -> []; list -> let f = take n list in drop n list ++ f

genString :: MarkovMap -> InputTuple -> IO String
genString mMap input = go [trd input, snd input, fst input]
  where go :: [String] -> IO String
        go acc = do
          let newInput = (acc !! 2, acc !! 1, acc !! 0)
          maybeMapping <- getMapping mMap newInput
          if | isNothing maybeMapping -> return $ unwords $ reverse acc
             | last (fromJust maybeMapping) == '.' -> return $ unwords $ reverse $ fromJust maybeMapping : acc
             | otherwise -> go $ fromJust maybeMapping : acc

main :: IO ()
main =  withFile "lexicon.txt" ReadMode $ \h -> do
  lexiContents <- hGetContents h
  mapFileExists <- doesFileExist "markov.cache"
  markovMap <- if mapFileExists
               then do c <- BS.hGetContents =<< openFile "markov.cache" ReadMode
                       let mMap =
                             case decode c of
                               Left e -> error e
                               Right m -> m
                       return mMap
               else do putStrLn "Writing cache.."
                       let mMap = genMap lexiContents
                       openFile "markov.cache" WriteMode >>= \h -> BS.hPut h (encode mMap)
                       return mMap
  putStrLn "loading done"
  forever $ do
    c <- getLine
    let ws = words c
    print =<< genString markovMap (ws !! 0, ws !! 1, ws !! 2)


-----
fst (x,_,_) = x
snd (_,x,_) = x
trd (_,_,x) = x
