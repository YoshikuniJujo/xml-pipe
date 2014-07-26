{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Pipe
import Data.Pipe.List
import Text.XML.Pipe

main :: IO ()
main = void . runPipe $ fromList [
	"<?xml version='1.0'?>",
	"<sample id='XXX-XXXX' ",
		"no='xxyy'>sample data<",
		"/sample>" ]
	=$= xmlEvent
	=$= checkJust
	=$= (xmlBegin >>= xmlNode)
	=$= printP

checkJust :: Monad m => Pipe (Maybe a) a m ()
checkJust = do
	mmx <- await
	case mmx of
		Just (Just x) -> yield x >> checkJust
		Just _ -> error "not Just"
		_ -> return ()

printP :: Show a => Pipe a () IO ()
printP = do
	mx <- await
	case mx of
		Just x -> lift (print x) >> printP
		_ -> return ()
