{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Data.List.Split
  
proxies :: IO (Maybe [String])
proxies = scrapeURL "https://free-proxy-list.net/" (htmls "tr")
   
main :: IO ()
main = do
  xxs <- proxies
  case xxs of
    Just xs -> putStrLn $ show (splitOn "</td>" (xs !! 1))
      
  return ()






