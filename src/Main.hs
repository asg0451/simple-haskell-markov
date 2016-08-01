module Main where


import qualified Data.Map.Lazy as M
import System.Random (randomRIO)
import System.IO
import Data.List.Split
import Data.List (last)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as S

type MarkovMap = M.Map InputTuple [String]
type InputTuple = (String, String)

getMapping :: MarkovMap -> InputTuple -> IO (Maybe String)
getMapping mMap input = do
  let maybeResult = M.lookup input mMap :: Maybe [String]
  case maybeResult of
    Nothing -> return Nothing
    Just res -> sample res

sample :: [a] -> IO (Maybe a)
sample [] = return Nothing
sample list  = randomRIO (0, length list - 1) >>= return . Just . (list !!)

genMap :: String -> MarkovMap
genMap lexicon =
  let ws = words lexicon
      chains = getChains ws
  in foldl (\acc v ->
              let key = fst v
                  val = snd v
              in if M.member key acc
                    then M.adjust (\list -> list ++ [val]) key acc
                    else M.insert key [val] acc)
           M.empty
           chains

getChains :: [String] -> [(InputTuple, String)]
getChains list = zip (zip (rotate 0 list) (rotate 1 list)) (rotate 2 list)

rotate :: Int -> [a] -> [a]
rotate n  l = case l of [] -> []; list -> let f = take n list in drop n list ++ f

genString :: MarkovMap -> InputTuple -> IO String
genString mMap input = go [snd input, fst input]
  -- build result backwards for efficiency
  where go :: [String] -> IO String
        go acc = do
          let newInput = (acc !! 1, acc !! 0)
          maybeMapping <- getMapping mMap newInput
          if isNothing maybeMapping || last (fromJust maybeMapping) == '.'
            then return $ unwords $ reverse acc
            else go $ fromJust maybeMapping : acc

main :: IO ()
main = withFile "lexicon.txt" ReadMode $ \h -> do
  contents <- hGetContents h
  let markovMap = genMap contents
  print =<< genString markovMap ("And", "he")
