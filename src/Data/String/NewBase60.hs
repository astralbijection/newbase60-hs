module Data.String.NewBase60
  ( numToSxg,
  )
where

import Data.Array (Array, listArray, (!))

lookupArray :: Array Integer Char
lookupArray = listArray (0, 59) "0123456789ABCDEFGHJKLMNPQRSTUVWXYZ_abcdefghijkmnopqrstuvwxyz"

numToSxg :: Integer -> String
numToSxg 0 = "0"
numToSxg n = convert "" n
  where
    convert s 0 = s
    convert s n =
      let d = n `mod` 60
          ch = lookupArray ! d
       in convert (ch : s) (n `div` 60)
