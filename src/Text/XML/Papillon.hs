{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes #-}

module Text.XML.Papillon(Xmlns, XEQName, parseXmlEvent, XmlEvent(..)) where

import Control.Arrow
import Data.List
import Data.Char
import Data.ByteString.Char8 (ByteString, pack)
import Text.Papillon
import Numeric

import qualified Data.ByteString.Char8 as BSC

type Xmlns = (BSC.ByteString, BSC.ByteString)
type XEQName = (BSC.ByteString, BSC.ByteString)

data XmlEvent
	= XEXmlDecl (Int, Int)
	| XESTag XEQName [Xmlns] [(XEQName, BSC.ByteString)]
	| XEETag XEQName
	| XEEmptyElemTag XEQName [Xmlns] [(XEQName, BSC.ByteString)]
	| XECharData BSC.ByteString
	deriving Show

data Attribute
	= NSAttribute BSC.ByteString BSC.ByteString
	| Attribute (BSC.ByteString, BSC.ByteString) BSC.ByteString
	deriving Show

procAtts :: [Attribute] -> (
	[(BSC.ByteString, BSC.ByteString)],
	[((BSC.ByteString, BSC.ByteString), BSC.ByteString)])
procAtts = (map fromNSAttribute *** map fromAttribute) . partition isNSAtt

fromNSAttribute :: Attribute -> (BSC.ByteString, BSC.ByteString)
fromNSAttribute (NSAttribute k v) = (k, v)
fromNSAttribute _ = error "bad"

fromAttribute :: Attribute -> ((BSC.ByteString, BSC.ByteString), BSC.ByteString)
fromAttribute (Attribute k v) = (k, v)
fromAttribute _ = error "bad"

isNSAtt :: Attribute -> Bool
isNSAtt (NSAttribute _ _) = True
isNSAtt _ = False

parseXmlEvent :: ByteString -> Maybe XmlEvent
parseXmlEvent = either (const Nothing) (Just . fst) . runError . xmlEvent . parse

fromHex :: String -> Char
fromHex = chr . fst . head . readHex

[papillon|

source: ByteString

xmlEvent :: XmlEvent
	= et:emptyElemTag	{ et }
	/ st:sTag		{ st }
	/ et:eTag		{ et }
	/ cd:charData		{ cd }
	/ xd:xmlDecl		{ xd }

spaces = _:(' ' / '\t' / '\r' / '\n')+

nameStartChar :: Char = <(`elem` (":_" ++ ['a' .. 'z'] ++ ['A' .. 'Z']))>

nameChar :: Char
	= s:nameStartChar				{ s }
	/ <(`elem` ("-." ++ ['0' .. '9']))>

ncNameStartChar :: Char = !':' s:nameStartChar		{ s }

ncNameChar :: Char = !':' c:nameChar			{ c }

name :: ByteString
	= sc:nameStartChar cs:(c:nameChar { c })*	{ pack $ sc : cs }

ncName :: ByteString
	= sc:ncNameStartChar cs:(c:ncNameChar { c })*	{ pack $ sc : cs }

qName :: (ByteString, ByteString)
	= pn:prefixedName				{ pn }
	/ un:unprefixedName				{ ("", un) }

prefixedName :: (ByteString, ByteString) = p:prefix ':' l:localPart
							{ (p, l) }
unprefixedName :: ByteString = l:localPart		{ l }
prefix :: ByteString = n:ncName				{ n }
localPart :: ByteString = n:ncName			{ n }

attValue :: ByteString
	= '"' v:(c:<(`notElem` "<&\"")> { c } / c:charEntRef { c })* '"'
							{ pack v }
	/ '\'' v:(c:<(`notElem` "<&'")> { c } / c:charEntRef { c })* '\''
							{ pack v }

charData :: XmlEvent
	= '>' cds:(c:<(`notElem` "<&")> { c } / c:charEntRef { c })*
							{ XECharData $ pack cds }

charEntRef :: Char = c:charRef { c } / c:entityRef { c }

charRef :: Char = '&' '#' 'x'
	ds:(<(`elem` "0123456789abcdefABCDEF")>)+ ';'	{ fromHex ds }

entityRef :: Char
	= '&' 'a' 'm' 'p' ';'				{ '&' }
	/ '&' 'l' 't' ';'				{ '<' }
	/ '&' 'g' 't' ';'				{ '>' }
	/ '&' 'q' 'u' 'o' 't' ';'			{ '"' }
	/ '&' 'a' 'p' 'o' 's' ';'			{ '\'' }

xmlDecl :: XmlEvent
	= '<' '?' 'x' 'm' 'l' vi:versionInfo _:spaces? '?' _:eof
	{ XEXmlDecl vi }

versionInfo :: (Int, Int)
	= _:spaces 'v' 'e' 'r' 's' 'i' 'o' 'n' _:eq
		vn:('"' v:versionNum '"' { v } / '\'' v:versionNum '\'' { v })
	{ vn }

eq :: () = _:spaces? '=' _:spaces?

versionNum :: (Int, Int)
	= '1' '.' d:<isDigit>+				{ (1, read d) }

sTag :: XmlEvent
	= '<' n:qName as:(_:spaces a:attribute { a })* _:spaces? _:eof
	{ uncurry (XESTag n) $ procAtts as }

emptyElemTag :: XmlEvent
	= '<' n:qName as:(_:spaces a:attribute { a })* _:spaces? '/' _:eof
	{ uncurry (XEEmptyElemTag n) $ procAtts as }

prefixedAttName :: ByteString
	= 'x' 'm' 'l' 'n' 's' ':' n:ncName		{ n }

defaultAttName = 'x' 'm' 'l' 'n' 's'

nsAttName :: ByteString
	= n:prefixedAttName				{ n }
	/ _:defaultAttName				{ "" }

attribute :: Attribute
	= n:nsAttName _:eq v:attValue			{ NSAttribute n v }
	/ n:qName _:eq v:attValue			{ Attribute n v }

eTag :: XmlEvent
	= '<' '/' n:qName _:spaces? _:eof	{ XEETag n }

eof = !_

|]

instance Source ByteString where
	type Token ByteString = Char
	data Pos ByteString = NoPos
	getToken = BSC.uncons
	initialPos = NoPos
	updatePos _ _ = NoPos
