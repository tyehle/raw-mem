module Main where

import System.IO
import qualified Data.ByteString as B
import Data.Bits (popCount)
import Data.List (foldl')

main :: IO ()
main = do
  handle <- openFile "/dev/mem" ReadMode
  bytes <- B.hGet handle 1000000
  let (ones, total) = countPops bytes
  putStrLn $ "Set bits: " ++ show ones
  putStrLn $ "Total bits: " ++ show total
  putStrLn $ "Ratio of set bits: " ++ show (fromIntegral ones / fromIntegral total)

countPops :: B.ByteString -> (Integer, Integer)
countPops bytes = (ones, total)
  where
    ones = sum . map (fromIntegral . popCount) $ B.unpack bytes
    total = 8 * fromIntegral (B.length bytes)
