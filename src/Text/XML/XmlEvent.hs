module Text.XML.XmlEvent (xmlEvent, XmlEvent(..), Xmlns, XEQName) where

import Control.Monad
import Data.Pipe
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Text.XML.Lexer
import Text.XML.Papillon

xmlEvent :: Monad m => Pipe BS.ByteString (Maybe XmlEvent) m ()
xmlEvent = sepTag =$= convert parseXmlEvent =$= filterP (maybe True notEmpty)

notEmpty :: XmlEvent -> Bool
notEmpty (XECharData cd) = not $ BSC.all isSpace cd
notEmpty _ = True

filterP :: Monad m => (a -> Bool) -> Pipe a a m ()
filterP p = await >>=
	maybe (return ()) (\x -> when (p x) (yield x) >> filterP p)
