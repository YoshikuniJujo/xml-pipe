{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Maybe
import Data.Pipe
import Data.Pipe.List
import Text.XML.Pipe

import qualified Data.ByteString.Char8 as BSC

littleIndians :: Int -> BSC.ByteString
littleIndians n = "<indians>" `BSC.append`
	BSC.pack (show n) `BSC.append` " little, " `BSC.append`
	BSC.pack (show $ n + 1) `BSC.append` " little, " `BSC.append`
	BSC.pack (show $ n + 2) `BSC.append` " little Indians" `BSC.append`
	"</indians>"

infiniteIndians :: [BSC.ByteString]
infiniteIndians = map littleIndians [1, 4 .. ]

xml :: [BSC.ByteString]
xml = "<?xml version='1.0'?><song>" : infiniteIndians

main :: IO ()
main = void . runPipe $ fromList xml
	=$= xmlEvent
	=$= convert fromJust
	=$= (xmlBegin >>= xmlNode)
	=$= takeP 8
	=$= printP

takeP :: Monad m => Int -> Pipe a a m ()
takeP 0 = return ()
takeP n = do
	mx <- await
	maybe (return ()) (\x -> yield x >> takeP (n - 1)) mx

convert :: Monad m => (a -> b) -> Pipe a b m ()
convert f = do
	mx <- await
	maybe (return ()) (\x -> yield (f x) >> convert f) mx

printP :: Show a => Pipe a () IO ()
printP = do
	mx <- await
	maybe (return ()) (\x -> lift (print x) >> printP) mx
