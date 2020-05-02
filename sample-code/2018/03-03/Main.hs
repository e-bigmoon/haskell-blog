module Main where

{-@ type NonZero = {v:Int | v /= 0} @-}

{-@ myDiv :: Int -> NonZero -> Int @-}
myDiv :: Int -> Int -> Int
myDiv = div

{-@ lazy main @-}
main :: IO ()
main = do
  n <- getLine
  m <- getLine
  case safeDiv (read n) (read m) of
    Just res -> print res
    Nothing -> do
      putStrLn "第二引数に0が入力されています"
      putStrLn "もう一度入力してください"
      main

{-@ safeDiv :: Int -> Int -> Maybe Int @-}
safeDiv :: Int -> Int -> Maybe Int
safeDiv n m
  | check     = Just $ div n m
  | otherwise = Nothing
  where
   check = m /= 0