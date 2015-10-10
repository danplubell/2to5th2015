module DNA where

import qualified Data.List       as DL
import           Data.Map.Strict (Map, adjust, fromList)

validNucleotideList::String
validNucleotideList = "GACT"

-- | Count the number of instances of a specific nucleotide in a string of nucleotides
count :: Char -> String -> Int
count elemToCount elements
  | elemToCount `notElem` validNucleotideList = error $ "invalid nucleotide: " ++ show elemToCount
  | otherwise                                 = length $ filter (== elemToCount) elements

-- | Count the number of instances of each nucleotide in a nucleotide string
nucleotideCounts :: String -> Map Char Int
nucleotideCounts xs = countNucleotides (fromList [(x,0)|x<-"ACGT"])
  where countNucleotides initialMap = DL.foldl' updtCnt initialMap xs
        updtCnt resultMap k = adjust succ k resultMap

-- | Count the number of differences in homologous strands of DNA
hammingDistance :: String -> String -> Int
hammingDistance xs ys |length xs /= length ys                      = error   "Input strands must have same length"
                      |length (filter validateNucleotide xs) == 0  = error $ "Input strand contains invalid nucleotide. Valid values are AGTC: " ++ xs
                      |length (filter validateNucleotide ys) == 0  = error $ "Input strand contains invalid nucleotide. Valid values are AGTC: " ++ ys
                      |otherwise                                  = length $ filter not $ zipWith (==) xs ys
                where
                  validateNucleotide::Char -> Bool
                  validateNucleotide n = elem n validNucleotideList
