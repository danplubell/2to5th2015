module Main where

import qualified Data.Text      as T
import qualified Data.Text.Read as TR

main :: IO ()
main = do
  putStrLn "Enter weight in kilograms"
  ws <- getLine
  let wr =  (TR.decimal (T.pack ws))::Either String (Int, T.Text)
  case wr of
    Left err -> error err
    Right (w,_) -> putStrLn $ "The fluid maintenance value for " ++ show w ++ "kg" ++ " is " ++ show (calcMaintance w)
                           ++ "mL per day"
  where
    calcMaintance::Int -> Int
    calcMaintance w | w <= 10      = 100 * w
                    | w <= 20      = 1000 + ((w-10) * 50)
                    | w <= 70      = 1500 + ((w-20) * 20)
                    | w > 70      = 2500
                    | otherwise   = error "weight value is out of range"
