{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Data.List.Split
import qualified Data.Text as T

data Proxy =
  Proxy {ip::String,
         port::String,
         code::String,
         country::String,
         anonymity::String,
         lastChecked::String}

instance Show Proxy where
  show (Proxy ip port code country anonymity lastChecked) =
    ip ++ "\t"
    ++ port ++ "\t"
    ++ code ++ "\t"
    ++ country ++ "\t"
    ++ anonymity ++ "\t"
    ++ lastChecked

proxiesRawHtml :: IO (Maybe [String])
proxiesRawHtml = scrapeURL "https://free-proxy-list.net/" $ htmls "tr"

replaceWithBlank :: String -> [String] -> String
replaceWithBlank =
  foldl (\acc x -> T.unpack $ T.replace (T.pack x) (T.pack "") $ T.pack acc)

mapToProxies :: [String] -> [Proxy]
mapToProxies xs =
  init $ tail
         $ map
             (\x -> Proxy{ip=replaceWithBlank (x !! 0) ["<tr>", "<td>"],
                          port=replaceWithBlank (x !! 1) ["<td>"],
                          code=replaceWithBlank (x !! 2) ["<td>"],
                          country=replaceWithBlank (x !! 3) ["<td class=\"hm\">"],
                          anonymity=replaceWithBlank (x !! 4) ["<td>"],
                          lastChecked=replaceWithBlank (x !! 7) ["<td class=\"hm\">"]})
             $ map
                 (splitOn "</td>") 
                 xs 
           
main :: IO ()
main = do
  xxs <- proxiesRawHtml
  case xxs of
    Just xs ->
      putStrLn $ concatMap ((++ "\n") . show) $ mapToProxies xs 
    Nothing -> putStrLn "something wrong"  



