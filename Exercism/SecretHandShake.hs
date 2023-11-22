module SecretHandshake (handshake) where

import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import Data.Maybe (mapMaybe)

-- https://www.perplexity.ai/search/can-this-haskell-9A265w6PQEK0tswBdeE9Eg?s=c
handshake :: Int -> [String]
handshake n = let bin = padTo5 (showIntAtBase 2 intToDigit n "")
                  binTuple = zip [0..4] bin
              in if head bin == '1'
                 then mapMaybe toAction binTuple
                 else reverse $ mapMaybe toAction binTuple

toAction :: (Int, Char) -> Maybe String
toAction (1, '1') = Just "jump"
toAction (2, '1') = Just "close your eyes"
toAction (3, '1') = Just "double blink"
toAction (4, '1') = Just "wink"
toAction _ = Nothing

padTo5 :: String -> String
padTo5 s = replicate (5 - length s) '0' ++ s


{-
module SecretHandshake (handshake) where

import Data.Char(intToDigit)
import Numeric(showIntAtBase)

handshake :: Int -> [String]
handshake 0 = []
handshake n = let str = filter (/= "") (buildActions n) 
              in if last str == "reverse" then reverse (init str) else str

buildActions :: Int -> [String]
buildActions n = foldr (\(ord, x) acc -> checkAction (ord, x) : acc) [] (zip [0..4] (reverse $ toBin n))
  where checkAction (pos, '1') = case pos of
                                   0 -> "wink"
                                   1 -> "double blink"
                                   2 -> "close your eyes"
                                   3 -> "jump"
                                   4 -> "reverse"
        checkAction (pos, '0') = ""

toBin :: Int -> String
toBin n = let bin = showIntAtBase 2 intToDigit n ""
              len = length bin
          in if len < 5 then replicate (5 - len) '0' ++ bin else bin

-}