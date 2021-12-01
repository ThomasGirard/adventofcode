module Helper (
	readAndParse, 
	boolToInt
	) where

import Text.Parsec

{-
import qualified Network.HTTP as H
import Control.Monad (join)

targetPath number = join ["data/Day", show number, "Data.txt"]
dataUrl year number = join ["http://adventofcode.com/", show year, "/day/", show number, "/input"]

 Doesn't work because Network.HTTP doesn't support HTTPS (TLS)
fetchTestData year number = let
	url = dataUrl year number
	req = H.getRequest url
	in do
		res <- H.simpleHTTP req
		case res of
			Left ce -> error $ "HTTP Connection error" ++ (show ce)
			Right resp -> do
				putStrLn $ "Got response with code " ++ (show $ H.rspCode resp)
				return $ H.rspBody resp
-}

boolToInt True = 1
boolToInt False = 0

readAndParse path parser callback = do
	text <- readFile path
	case parse parser "" text of
		Left e -> error $ "Parse error: " ++ (show e)
		Right parsed -> return $ callback parsed