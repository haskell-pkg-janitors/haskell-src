-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.ParseMonad
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monad for the Haskell parser.
--
-----------------------------------------------------------------------------

module Language.Haskell.ParseMonad where

import Language.Haskell.Syntax

-- | The result of a parse.
data ParseResult a
	= ParseOk a		-- ^ The parse succeeded, yielding a value.
	| ParseFailed SrcLoc String
				-- ^ The parse failed at the specified
				-- source location, with an error message.
	deriving Show

-- internal version
data ParseStatus a = Ok ParseState a | Failed SrcLoc String
	deriving Show

data LexContext = NoLayout | Layout Int
	deriving (Eq,Ord,Show)

type ParseState = [LexContext]

type P a
     =  String			-- input string
     -> SrcLoc			-- location of last token read
     -> Int			-- current line
     -> Int			-- current column
     -> ParseState		-- layout info.
     -> ParseStatus a

runParser :: P a -> String -> ParseResult a
runParser p s = case p s (SrcLoc 1 1) 1 0 [] of
	Ok _ a -> ParseOk a
	Failed loc s -> ParseFailed loc s

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \i l n c s ->
	case m i l n c s of
	    Failed loc s -> Failed loc s
	    Ok s' a -> case k a of k' -> k' i l n c s'

m `thenP_` k = m `thenP` \_ -> k

mapP :: (a -> P b) -> [a] -> P [b]
mapP f [] = returnP []
mapP f (a:as) =
     f a `thenP` \b ->
     mapP f as `thenP` \bs ->
     returnP (b:bs)

returnP a = \i l n c s -> Ok s a

failP :: String -> P a
failP err = \i l n c s -> Failed (SrcLoc n c) err

getSrcLoc :: P SrcLoc
getSrcLoc = \i l n c s -> Ok s l

getContext :: P [LexContext]
getContext = \i l n c s -> Ok s s

pushContext :: LexContext -> P ()
pushContext ctxt =
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
	\i l n c s -> Ok (ctxt:s) ()

popContext :: P ()
popContext = \i l n c stk ->
      case stk of
	(_:s) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $
            Ok s ()
        []    -> error "Internal error: empty context in popContext"
