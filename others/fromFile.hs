{-# LANGUAGE PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Pipe
import System.IO
import System.Environment
import Text.XML.Pipe

import qualified Data.ByteString as BS

main :: IO ()
main = do
	fp : _ <- getArgs
	void . runPipe $ readFileP fp
		=$= xmlEvent
		=$= checkJust
		=$= (xmlBegin >>= xmlNode)
		=$= printP

readFileP :: FilePath -> Pipe () BS.ByteString IO ()
readFileP fp = bracket (openFile fp ReadMode) hClose hRead

hRead :: Handle -> Pipe () BS.ByteString IO ()
hRead h = do
	eof <- lift $ hIsEOF h
	unless eof $ do
		l <- lift $ BS.hGetLine h
		yield l
		hRead h

printP :: Show a => Pipe a () IO ()
printP = do
	mx <- await
	case mx of
		Just x -> lift (print x) >> printP
		_ -> return ()

checkJust :: Monad m => Pipe (Maybe a) a m ()
checkJust = do
	mmx <- await
	case mmx of
		Just (Just x) -> yield x >> checkJust
		Just _ -> error "bad"
		_ -> return ()
