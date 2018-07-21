#!/usr/bin/env stack
-- stack script --resolver lts-11.17
import           Data.Char

main :: IO ()
main = do
  -- 小文字
  print $ isUpper 'a'
  print $ isAsciiUpper 'a'

  -- 大文字
  print $ isUpper 'A'
  print $ isAsciiUpper 'A'

  -- 記号
  print $ isUpper '@'
  print $ isAsciiUpper '@'

  -- 日本語
  print $ isUpper 'あ'
  print $ isAsciiUpper 'あ'

  print $ isUpper 'A'
  print $ isAsciiUpper 'A'

  print $ isUpper 'Ａ'
  print $ isAsciiUpper 'Ａ'
