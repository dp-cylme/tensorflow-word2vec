{-# LANGUAGE OverloadedStrings #-}

module TensorFlow.Examples.Word2Vec.Parse where

import Data.List (sortBy)
import Data.Function (on)
import Path.Internal (Path(..))
import Data.Map.Strict (Map)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy as LazyMap

import Prelude hiding (Word)


type Word = ByteString


data Dataset = Dataset
    { _data :: [Int]  -- ^ Encode words by index
    , _count :: Map Word Int  -- ^ count word in data set
    , _dictionary :: Map Word Int  -- ^ word -> index
    , _reverse_dictionary :: Map Int Word  -- ^ index -> word
    }

-- | Reads the data into a list of strings.
readSamples :: FilePath -> IO [Word]
readSamples path = do
    raw <-
        Zip.withArchive
            (Path path)
            (Zip.getEntries >>=
             \entries ->
                  mapM Zip.getEntry (LazyMap.keys entries))
    return $ BSC8.words $ BSC8.unwords raw

space :: Word8
space = 32

-- | Build the dictionary and replace rare words with UNK token.
buildDataset :: Int -> [Word] -> Dataset
buildDataset vocabularySize words =
    Dataset
    { _data = data_
    , _count = Map.insert "UNK" countUNK count
    , _dictionary = dictionary
    , _reverse_dictionary = Map.fromList
          (map
               (\(k,v) ->
                     (v, k))
               (Map.toList dictionary))
    }
  where
    precount =
        Map.fromListWith (+) $
        map
            (\word ->
                  (word, 1))
            words
    count =
        Map.fromList $
        take (vocabularySize - 1) $
        reverse $ sortBy (compare `on` snd) $ Map.toList precount
    dictionary =
        Map.insert "UNK" 0 $ Map.fromList $ zip (Map.keys count) [1 ..]
    data_ =
        [ Map.findWithDefault 0 word dictionary
        | word <- words ]
    countUNK =
        foldr
            (\word acc ->
                  if Map.notMember word dictionary
                      then acc + 1
                      else acc)
            0
            words
