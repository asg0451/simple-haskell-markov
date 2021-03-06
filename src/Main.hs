{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.Monad
import qualified Data.Map.Lazy as M
import System.Random (randomRIO)
import System.IO
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S
import System.Environment (getArgs)
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
sample set = if S.null set then return Nothing
             else randomRIO (0, length set - 1) >>= return . Just . (`S.elemAt` set)

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
  let listRotations = [rotate n list | n <- [0 .. chainLength - 1]]
  in zip (foldl (\acc sublist ->
                   zipWith (++)
                           acc
                           (pure <$> sublist))
                (pure <$> head listRotations)
                (drop 1 listRotations))
         (rotate chainLength list)

rotate :: Int -> [a] -> [a]
rotate n  l = case l of [] -> []; list -> let f = take n list in drop n list ++ f

genString :: MarkovMap -> InputType -> IO String
genString mMap input = go $ reverse $ take chainLength input
  where go :: [String] -> IO String
        go acc = do
          let newInput = reverse $ take chainLength acc
          maybeMapping <- getMapping mMap newInput
          if | isNothing maybeMapping -> return $ unwords $ reverse acc
             | last (fromJust maybeMapping) == '.' -> return $ unwords $ reverse $ fromJust maybeMapping : acc
             | otherwise -> go $ fromJust maybeMapping : acc

main :: IO ()
main =  do
  args <- getArgs
  let lexiconName = if not $ null args then head args else "lexicon.txt"
  withFile lexiconName ReadMode $ \h -> do
    lexiContents <- hGetContents h
    mapFileExists <- doesFileExist $ lexiconName ++ ".cache"
    markovMap <- if mapFileExists
                 then do putStrLn "Reading cache.."
                         c <- BS.hGetContents =<< openFile (lexiconName ++ ".cache") ReadMode
                         case decode c of
                           Left _ -> do putStrLn "couldn't read cache, rebuilding"
                                        buildMap lexiconName lexiContents
                           Right m -> return m
                 else buildMap lexiconName lexiContents
    putStrLn $ "loading done. please type " ++ show chainLength ++ " words"
    forever $ do
      c <- getLine
      let ws = words c
      print =<< genString markovMap (take chainLength ws)

        where buildMap lexiName c = do putStrLn "Building map and writing cache.."
                                       let mMap = genMap c
                                       openFile (lexiName ++ ".cache") WriteMode >>= \h -> BS.hPut h (encode mMap)
                                       return mMap
chainLength :: Int
chainLength = 3
