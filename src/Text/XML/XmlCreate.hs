{-# LANGUAGE OverloadedStrings #-}

module Text.XML.XmlCreate (
	xmlEvent, XmlEvent(..), XmlNode(..), xmlBegin, xmlNode, xmlNodeUntil,
	QName, Xmlns, XEQName) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Pipe
import qualified Data.ByteString as BS

import Text.XML.XmlEvent

type QName = ((BS.ByteString, Maybe BS.ByteString), BS.ByteString)

data XmlNode
	= XmlDecl (Int, Int)
	| XmlStart QName [Xmlns] [(QName, BS.ByteString)]
	| XmlEnd QName
	| XmlNode QName [Xmlns] [(QName, BS.ByteString)] [XmlNode]
	| XmlCharData BS.ByteString
	deriving (Eq, Show)

toQName ::
	[(BS.ByteString, BS.ByteString)] -> (BS.ByteString, BS.ByteString) -> QName
toQName nss (q, n) = ((q, lookup q nss), n)

xmlBegin :: Monad m => Pipe XmlEvent XmlNode m [Xmlns]
xmlBegin = do
	mxe <- await
	case mxe of
		Just (XESTag n nss atts) -> do
			yield $ XmlStart (toQName nss n) nss
				(map (first $ toQName nss) atts)
			return nss
		Nothing -> return []
		_ -> xmlBegin

xmlNodeUntil :: Monad m => (XmlNode -> Bool) ->
	[Xmlns] -> Pipe XmlEvent XmlNode m ()
xmlNodeUntil p nss = do
	mnd <- xmlNd nss
	case mnd of
		Right nd -> do
			yield nd
			unless (p nd) $ xmlNodeUntil p nss
		Left (XEXmlDecl _) -> return ()
		_ -> return ()

xmlNode :: Monad m => [Xmlns] -> Pipe XmlEvent XmlNode m Bool
xmlNode nss = do
	mnd <- xmlNd nss
	case mnd of
		Right nd -> yield nd >> xmlNode nss
		Left (XEXmlDecl _) -> return True
		_ -> return False

xmlNd :: Monad m =>
	[(BS.ByteString, BS.ByteString)] -> Pipe XmlEvent a m (Either XmlEvent XmlNode)
xmlNd nss = do
	mxe <- await
	case mxe of
		Just (XESTag n nss' atts) -> do
			nds <- xmlNds (nss' ++ nss)
			return . Right $ XmlNode (toQName (nss' ++ nss) n) nss'
				(map (first $ toQName (nss' ++ nss)) atts) nds
		Just (XEEmptyElemTag n nss' atts) ->
			return . Right $ XmlNode (toQName (nss' ++ nss) n) nss'
				(map (first $ toQName (nss' ++ nss)) atts) []
--		Just (XEETag n) -> return $ Left (XEE
		Just (XECharData cd) -> return . Right $ XmlCharData cd
--		Just (XEXmlDecl v) -> return . Just $ XmlDecl v
		Just xe -> return $ Left xe
		Nothing -> return . Left $ XECharData ""
--		_ -> error $ "bad in xmlNd: " ++ show mxe
--		_ -> error $ "bad: " ++ show mxe

xmlNds :: Monad m => [(BS.ByteString, BS.ByteString)] -> Pipe XmlEvent a m [XmlNode]
xmlNds nss = do
	mxn <- xmlNd nss
	case mxn of
		Right xn -> (xn :) <$> xmlNds nss
		_ -> return []
{-
	mxe <- await
	case mxe of
		Just (XESTag n nss' atts) -> do
			nds <- xmlNds nss
			return . (: nds) $ XmlNode (toQName (nss ++ nss') n)
				(map (first $ toQName (nss ++ nss')) atts) []
		_ -> return []
		-}
