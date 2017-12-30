{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Data.List.Split
import qualified Data.Text as T
  
proxiesRawHtml :: IO (Maybe [String])
proxiesRawHtml = scrapeURL "https://free-proxy-list.net/" (htmls "tr")

mapToProxies :: [String] -> [[String]]
mapToProxies xs =
  init $ tail
       $ map
         (\x -> splitOn "</td>" x)
         xs  
          
main :: IO ()
main = do
  xxs <- proxiesRawHtml
  case xxs of
    Just xs -> putStrLn $ show (mapToProxies xs)
      
  return ()






