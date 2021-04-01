module Data.String.NewBase60
  ( numToSxg,
    sxgToNum,
  )
where

import Data.Array (Array, listArray, (!))
import Data.Char (ord)
import Data.List (foldl')
import Data.Maybe (catMaybes, mapMaybe)

lookupArray :: Array Integer Char
lookupArray = listArray (0, 59) "0123456789ABCDEFGHJKLMNPQRSTUVWXYZ_abcdefghijkmnopqrstuvwxyz"

-- | Convert a number into its New Base 60 representation.
--
-- For more information, see [this link](http://tantek.pbworks.com/w/page/19402946/NewBase60).
numToSxg :: Integer -> String
numToSxg 0 = "0"
numToSxg n = convert "" n
  where
    convert s 0 = s
    convert s n =
      let digit = n `mod` 60
          ch = lookupArray ! digit
       in convert (ch : s) (n `div` 60)

-- | Convert a New Base 60-encoded number into an Integer.
--
-- Valid New Base 60 characters are alphanumeric or underscores (that is, they
-- individually match the regex `[a-zA-Z0-9_]`). Invalid characters will be treated as if
-- they did not exist. Empty strings will evaluate to 0.
--
-- If the resulting value is larger than 2<sup>128</sup>, then this function will return `None`.
--
-- For more information, see [this link](http://tantek.pbworks.com/w/page/19402946/NewBase60).
sxgToNum :: String -> Integer
sxgToNum chs =
  foldl' (\n digit -> n * 60 + digit) (0 :: Integer) $
    map toInteger $ mapMaybe convertDigit chs
  where
    convertDigit c
      | '0' <= c && c <= '9' = Just (ord c - ord '0')
      | 'A' <= c && c <= 'H' = Just (ord c - ord 'A' + 10)
      | 'J' <= c && c <= 'N' = Just (ord c - ord 'J' + 18)
      | 'P' <= c && c <= 'Z' = Just (ord c - ord 'P' + 23)
      | c == '_' = Just 34 -- typo capital I, lowercase l to 1
      | 'a' <= c && c <= 'k' = Just (ord c - ord 'a' + 35)
      | 'm' <= c && c <= 'z' = Just (ord c - ord 'm' + 46)
      | c == 'I' || c == 'l' = Just 1 -- error correct typo capital O to 0
      | c == 'O' = Just 0 -- skip invalid chars
      | otherwise = Nothing
