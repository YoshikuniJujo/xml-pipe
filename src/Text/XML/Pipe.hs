{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Text.XML.Pipe (
	-- * Functions
	-- ** Decode
	xmlEvent, xmlBegin, xmlNode, xmlReborn, xmlNodeUntil,
	-- ** Encode
	xmlString,
	-- * Types
	XmlEvent(..), XmlNode(..),
	-- ** Type Synonyms
	XEQName, Xmlns, QName, nullQ) where

import Control.Arrow
import Control.Monad
import Data.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Text.XML.XmlCreate

xmlReborn :: Monad m => Pipe XmlEvent XmlNode m ()
xmlReborn = xmlBegin >>= xmlNode >>= flip when xmlReborn

xmlString :: [XmlNode] -> BS.ByteString
xmlString = BS.concat . map eventToS . toEvent

toEvent :: [XmlNode] -> [XmlEvent]
toEvent [] = []
toEvent (XmlDecl v : ns) = XEXmlDecl v : toEvent ns
toEvent (XmlStart ((q, _), n) nss atts : ns) =
	XESTag (q, n) nss (map (first $ first fst) atts) : toEvent ns
toEvent (XmlEnd ((q, _), n) : ns) = XEETag (q, n) : toEvent ns
toEvent (XmlNode ((q, _), n) nss atts ns : ns') =
	XESTag (q, n) nss (map (first $ first fst) atts) :
		toEvent ns ++ [XEETag (q, n)] ++ toEvent ns'
toEvent (XmlCharData cd : ns) = XECharData cd : toEvent ns

eventToS :: XmlEvent -> BS.ByteString
eventToS (XEXmlDecl (j, n)) = BS.concat [
	"<?xml version='", BSC.pack $ show j, ".",  BSC.pack $ show n, "'?>" ]
eventToS (XESTag qn nss atts) = BS.concat [
	"<", qNameToS qn,
	BS.concat $ map nsToS nss,
	BS.concat $ map attToS atts, ">" ]
eventToS (XEETag qn) = BS.concat ["</", qNameToS qn, ">"]
eventToS (XEEmptyElemTag qn nss atts) = BS.concat [
	"<", qNameToS qn,
	BS.concat $ map nsToS nss,
	BS.concat $ map attToS atts, "/>" ]
eventToS (XECharData cd) = quote cd

qNameToS :: (BS.ByteString, BS.ByteString) -> BS.ByteString
qNameToS ("", n) = n
qNameToS (q, n) = BS.concat [q, ":", n]

nsToS :: (BS.ByteString, BS.ByteString) -> BS.ByteString
nsToS ("", s) = BS.concat [" xmlns='", s, "'"]
nsToS (ns, s) = BS.concat [" xmlns:", ns, "='", s, "'"]

attToS :: ((BS.ByteString, BS.ByteString), BS.ByteString) -> BS.ByteString
attToS (qn, v) = BS.concat [" ", qNameToS qn, "='", quote v, "'"]

quote :: BS.ByteString -> BS.ByteString
quote bs
	| Just (h, t) <- BSC.uncons bs = case h of
		'&' -> "&amp;" `BS.append` quote t
		'<' -> "&lt;" `BS.append` quote t
		'>' -> "&gt;" `BS.append` quote t
		'"' -> "&quot;" `BS.append` quote t
		'\'' -> "&apos;" `BS.append` quote t
		_ -> h `BSC.cons` quote t
	| otherwise = ""

nullQ :: BS.ByteString -> QName
nullQ = (("", Nothing) ,)
