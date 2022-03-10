module Playground(main) where

integer = 10 :: Int
str = "Hello, World!" :: String
character = 'H' :: Char
tuple = (integer, str, character) :: (Int, String, Char)

float = 3.2 :: Float

toInt :: Float -> Int
toInt = round

main = print (toInt 12.3) :: IO ()
