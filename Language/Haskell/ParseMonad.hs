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
-- Monads for the Haskell parser and lexer.
--
-----------------------------------------------------------------------------

module Language.Haskell.ParseMonad(
		-- * Parsing
		P, ParseResult(..), LexContext(..),
		runParser, getSrcLoc, pushCurrentContext, popContext,
		-- * Lexing
		Lex, runL, getInput, discard, lexNewline, lexTab, lexWhile,
		alternative, checkBOL, setBOL, startToken, getOffside,
		pushContextL, popContextL
	) where

import Language.Haskell.Syntax(SrcLoc(..))

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

-- | Monad for parsing

newtype P a = P { runP ::
		        String		-- input string
		     -> Int		-- current column
		     -> Int		-- current line
		     -> SrcLoc		-- location of last token read
		     -> ParseState	-- layout info.
		     -> ParseStatus a
		}

runParser :: P a -> String -> ParseResult a
runParser (P m) s = case m s 0 1 (SrcLoc 1 1) [] of
	Ok _ a -> ParseOk a
	Failed loc s -> ParseFailed loc s

instance Monad P where
	return a = P $ \_i _x _y _l s -> Ok s a
	P m >>= k = P $ \i x y l s ->
		case m i x y l s of
		    Failed loc s -> Failed loc s
		    Ok s' a -> runP (k a) i x y l s'
	fail s = P $ \_r _col _line loc _stk -> Failed loc s

getSrcLoc :: P SrcLoc
getSrcLoc = P $ \_i _x _y l s -> Ok s l

pushCurrentContext :: P ()
pushCurrentContext = do
	SrcLoc _ c <- getSrcLoc
	pushContext (Layout c)

pushContext :: LexContext -> P ()
pushContext ctxt =
--trace ("pushing lexical scope: " ++ show ctxt ++"\n") $
	P $ \_i _x _y _l s -> Ok (ctxt:s) ()

popContext :: P ()
popContext = P $ \_i _x _y _l stk ->
      case stk of
   	(_:s) -> --trace ("popping lexical scope, context now "++show s ++ "\n") $
            Ok s ()
        []    -> error "Internal error: empty context in popContext"

-- Monad for lexical analysis:
-- a continuation-passing version of the parsing monad

newtype Lex r a = Lex { runL :: (a -> P r) -> P r }

instance Monad (Lex r) where
	return a = Lex $ \k -> k a
	Lex v >>= f = Lex $ \k -> v (\a -> runL (f a) k)
	Lex v >> Lex w = Lex $ \k -> v (\_ -> w k)
	fail s = Lex $ \_ -> fail s

-- Operations on this monad

getInput :: Lex r String
getInput = Lex $ \cont -> P $ \r -> runP (cont r) r

-- | Discard some input characters (these must not include tabs or newlines).

discard :: Int -> Lex r ()
discard n = Lex $ \cont -> P $ \r x -> runP (cont ()) (drop n r) (x+n)

-- | Discard the next character, which must be a newline.

lexNewline :: Lex a ()
lexNewline = Lex $ \cont -> P $ \(_:r) _x y -> runP (cont ()) r 1 (y+1)

-- | Discard the next character, which must be a tab.

lexTab :: Lex a ()
lexTab = Lex $ \cont -> P $ \(_:r) x -> runP (cont ()) r (nextTab x)

nextTab x = x + (tAB_LENGTH - (x-1) `mod` tAB_LENGTH)

tAB_LENGTH = 8 :: Int

-- Consume and return the largest string of characters satisfying p

lexWhile :: (Char -> Bool) -> Lex a String
lexWhile p = Lex $ \cont -> P $ \r x ->
	let (cs,rest) = span p r in
	runP (cont cs) rest (x + length cs)

-- An alternative scan, to which we can return if subsequent scanning
-- is unsuccessful.

alternative :: Lex a v -> Lex a (Lex a v)
alternative (Lex v) = Lex $ \cont -> P $ \r x y ->
	runP (cont (Lex $ \cont' -> P $ \_r _x _y ->
		runP (v cont') r x y)) r x y

-- The source location (SrcLoc y x) is the coordinates of the previous token,
-- or, while scanning a token, the start of the current token.

-- col is the current column in the source file.
-- We also need to remember between scanning tokens whether we are
-- somewhere at the beginning of the line before the first token.
-- This could be done with an extra Bool argument to the P monad,
-- but as a hack we use a col value of 0 to indicate this situation.

-- Setting col to 0 is used in two places: just after emitting a virtual
-- close brace due to layout, so that next time through we check whether
-- we also need to emit a semi-colon, and at the beginning of the file,
-- by runParser, to kick off the lexer.
-- Thus when col is zero, the true column can be taken from the loc.

checkBOL :: Lex a Bool
checkBOL = Lex $ \cont -> P $ \r x y loc@(SrcLoc _y x') ->
		if x == 0 then runP (cont True) r x' y loc
			else runP (cont False) r x y loc

setBOL :: Lex a ()
setBOL = Lex $ \cont -> P $ \r _ -> runP (cont ()) r 0

-- Set the loc to the current position

startToken :: Lex a ()
startToken = Lex $ \cont -> P $ \s x y _ -> runP (cont ()) s x y (SrcLoc y x)

-- Current status with respect to the offside (layout) rule:
-- LT: we are to the left of the current indent (if any)
-- EQ: we are at the current indent (if any)
-- GT: we are to the right of the current indent, or not subject to layout

getOffside :: Lex a Ordering
getOffside = Lex $ \cont -> P $ \r x y loc stk ->
		let ord = case stk of
			(Layout n:_) -> compare x n
			_            -> GT
		in runP (cont ord) r x y loc stk

pushContextL :: LexContext -> Lex a ()
pushContextL ctxt = Lex $ \cont -> P $ \r x y loc stk ->
		runP (cont ()) r x y loc (ctxt:stk)

popContextL :: String -> Lex a ()
popContextL fn = Lex $ \cont -> P $ \r x y loc stk -> case stk of
		(_:ctxt) -> runP (cont ()) r x y loc ctxt
		[]       -> error ("Internal error: empty context in " ++ fn)
