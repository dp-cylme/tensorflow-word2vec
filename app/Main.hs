{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sortBy)
import Data.Function (on)
import Data.Map.Strict (Map)
import TensorFlow.Examples.Word2Vec.InputData
import TensorFlow.Examples.Word2Vec.Parse
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BSLC8

import Prelude hiding (Word)

main :: IO ()
main = do
    words <- readSamples =<< trainingTextData
    print ("Data size " ++ show (length words))
    let dataset = buildDataset 5000 words
    print
        ("Most common words (+UNK) " ++
         showBSL (mostCommonWords 5 (_count dataset)))
    print
        ("Sample data " ++
         show (take 10 (_data dataset)) ++
         " " ++
         showBSL
             (sampleData
                  (take 10 (_data dataset))
                  (_reverse_dictionary dataset)))


mostCommonWords :: Int -> Map Word Int -> [Word]
mostCommonWords cnt counts =
    map fst $
    take cnt $ reverse $ sortBy (compare `on` snd) $ Map.toList counts


sampleData :: [Int] -> Map Int Word -> [Word]
sampleData indexes words =
    [ Map.findWithDefault "UNK2" index words
    | index <- indexes ]

showBS = BSLC8.unpack

showBSL = show . map showBS
