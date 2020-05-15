module Main where

import           Control.Monad
import qualified Data.List      as L
import           System.Process

checkIsLow :: [String] -> Bool
checkIsLow lst = case status of
    "Discharging" -> batPer < 20
    "Full" -> False
    "Charging" -> False
    _ -> error "Weird Case"
  where
    batPer = read . (L.\\"%,") . (!!) lst $ 3 :: Int
    status = init $ lst !! 2

main :: IO ()
main = forever $ do
    lst <- words <$> readProcess "acpi" ["-b"] []
    if checkIsLow lst
       then callCommand $ notification <> unwords lst <> "\""
       else return ()
    callCommand "sleep 20"
  where
    notification = "notify-send --urgency=critical \"Low Battery\" \""
