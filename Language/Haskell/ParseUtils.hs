-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.ParseUtils
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for the Haskell parser.
--
-----------------------------------------------------------------------------

module Language.Haskell.ParseUtils (
	  parseError		-- String -> Pa
	, splitTyConApp		-- HsType -> P (HsName,[HsType])
	, mkRecConstrOrUpdate	-- HsExp -> [HsFieldUpdate] -> P HsExp
	, checkPrec		-- Integer -> P Int
	, checkContext		-- HsType -> P HsContext
	, checkAssertion	-- HsType -> P HsAsst
	, checkDataHeader	-- HsQualType -> P (HsContext,HsName,[HsName])
	, checkSimple		-- HsType -> [HsName] -> P ((HsName,[HsName]))
	, checkPattern		-- HsExp -> P HsPat
	, checkPatterns		-- [HsExp] -> P [HsPat]
	, checkExpr		-- HsExp -> P HsExp
	, checkValDef		-- SrcLoc -> HsExp -> HsRhs -> [HsDecl] -> P HsDecl
	, checkUnQual		-- HsQName -> P HsName
 ) where

import Language.Haskell.Syntax
import Language.Haskell.ParseMonad

parseError :: String -> P a
parseError = failP

splitTyConApp :: HsType -> P (HsName,[HsType])
splitTyConApp t = split t []
 where
	split :: HsType -> [HsType] -> P (HsName,[HsType])
	split (HsTyApp t u) ts = split t (u:ts)
	split (HsTyCon (UnQual t)) ts = returnP (t,ts)
	split _ _ = parseError "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Various Syntactic Checks

checkContext :: HsType -> P HsContext
checkContext (HsTyTuple ts) =
     mapP checkAssertion ts `thenP` \cs ->
     returnP cs
checkContext t =
     checkAssertion t `thenP` \c ->
     returnP [c]

-- Changed for multi-parameter type classes

checkAssertion :: HsType -> P HsAsst
checkAssertion = checkAssertion' []
	where	checkAssertion' ts (HsTyCon c) = returnP (c,ts)
		checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
		checkAssertion' _ _ = parseError "Illegal class assertion"


checkDataHeader :: HsQualType -> P (HsContext,HsName,[HsName])
checkDataHeader (HsQualType cs t) =
   checkSimple t []	     `thenP` \(c,ts) ->
   returnP (cs,c,ts)

checkSimple :: HsType -> [HsName] -> P ((HsName,[HsName]))
checkSimple (HsTyApp l (HsTyVar a)) xs = checkSimple l (a:xs)
checkSimple (HsTyCon (UnQual t))    xs = returnP (t,xs)
checkSimple _ _ = parseError "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: HsExp -> P HsPat
checkPattern e = checkPat e []

checkPatterns :: [HsExp] -> P [HsPat]
checkPatterns es = mapP checkPattern es

checkPat :: HsExp -> [HsPat] -> P HsPat
checkPat (HsCon c) args = returnP (HsPApp c args)
checkPat (HsApp f x) args = checkPat x [] `thenP` \x -> checkPat f (x:args)
checkPat e [] = case e of
	HsVar (UnQual x)   -> returnP (HsPVar x)
	HsLit l            -> returnP (HsPLit l)
	HsInfixApp l op r  -> checkPat l [] `thenP` \l ->
			      checkPat r [] `thenP` \r ->
			      case op of
				 HsQConOp c -> returnP (HsPInfixApp l c r)
				 _ -> patFail
	HsTuple es         -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (HsPTuple ps)
	HsList es	   -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (HsPList ps)
	HsParen e	   -> checkPat e [] `thenP` (returnP . HsPParen)
	HsAsPat n e	   -> checkPat e [] `thenP` (returnP . HsPAsPat n)
	HsWildCard	   -> returnP HsPWildCard
	HsIrrPat e	   -> checkPat e [] `thenP` (returnP . HsPIrrPat)
	HsRecConstr c fs   -> mapP checkPatField fs `thenP` \fs ->
			      returnP (HsPRec c fs)
	HsNegApp (HsLit l) -> returnP (HsPNeg (HsPLit l))
	_ -> patFail

checkPat _ _ = patFail

checkPatField :: HsFieldUpdate -> P HsPatField
checkPatField (HsFieldUpdate n e) =
   checkPat e [] `thenP` \p ->returnP (HsPFieldPat n p)

patFail = parseError "Parse error in pattern"

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: HsExp -> P HsExp
checkExpr e = case e of
	HsVar _			  -> returnP e
	HsCon _			  -> returnP e
	HsLit _			  -> returnP e
	HsInfixApp e1 op e2	  -> check2Exprs e1 e2 (flip HsInfixApp op)
	HsApp e1 e2		  -> check2Exprs e1 e2 HsApp
	HsNegApp e		  -> check1Expr e HsNegApp
	HsLambda ps e		  -> check1Expr e (HsLambda ps)
	HsLet bs e		  -> check1Expr e (HsLet bs)
	HsIf e1 e2 e3		  -> check3Exprs e1 e2 e3 HsIf
	HsCase e alts		  -> mapP checkAlt alts `thenP` \alts ->
				     checkExpr e `thenP` \e ->
				     returnP (HsCase e alts)
	HsDo stmts		  -> mapP checkStmt stmts `thenP` (returnP . HsDo)
	HsTuple es		  -> checkManyExprs es HsTuple
	HsList es		  -> checkManyExprs es HsList
	HsParen e		  -> check1Expr e HsParen
	HsLeftSection e op	  -> check1Expr e (flip HsLeftSection op)
	HsRightSection op e       -> check1Expr e (HsRightSection op)
	HsRecConstr c fields	  -> mapP checkField fields `thenP` \fields ->
				     returnP (HsRecConstr c fields)
	HsRecUpdate e fields	  -> mapP checkField fields `thenP` \fields ->
				     checkExpr e `thenP` \e ->
				     returnP (HsRecUpdate e fields)
	HsEnumFrom e		  -> check1Expr e HsEnumFrom
	HsEnumFromTo e1 e2	  -> check2Exprs e1 e2 HsEnumFromTo
	HsEnumFromThen e1 e2      -> check2Exprs e1 e2 HsEnumFromThen
	HsEnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 HsEnumFromThenTo
	HsListComp e stmts        -> mapP checkStmt stmts `thenP` \stmts ->
				     checkExpr e `thenP` \e ->
				     returnP (HsListComp e stmts)
	HsExpTypeSig loc e ty     -> checkExpr e `thenP` \e ->
				     returnP (HsExpTypeSig loc e ty)
	_                         -> parseError "parse error in expression"

-- type signature for polymorphic recursion!!
check1Expr :: HsExp -> (HsExp -> a) -> P a
check1Expr e f = checkExpr e `thenP` (returnP . f)

check2Exprs :: HsExp -> HsExp -> (HsExp -> HsExp -> a) -> P a
check2Exprs e1 e2 f =
	checkExpr e1 `thenP` \e1 ->
	checkExpr e2 `thenP` \e2 ->
	returnP (f e1 e2)

check3Exprs :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> a) -> P a
check3Exprs e1 e2 e3 f =
	checkExpr e1 `thenP` \e1 ->
	checkExpr e2 `thenP` \e2 ->
	checkExpr e3 `thenP` \e3 ->
	returnP (f e1 e2 e3)

checkManyExprs es f =
	mapP checkExpr es `thenP` \es ->
	returnP (f es)

checkAlt (HsAlt loc p galts bs)
	= checkGAlts galts `thenP` \galts -> returnP (HsAlt loc p galts bs)

checkGAlts (HsUnGuardedAlt e) = check1Expr e HsUnGuardedAlt
checkGAlts (HsGuardedAlts galts)
	= mapP checkGAlt galts `thenP` (returnP . HsGuardedAlts)

checkGAlt (HsGuardedAlt loc e1 e2) = check2Exprs e1 e2 (HsGuardedAlt loc)

checkStmt (HsGenerator p e) = check1Expr e (HsGenerator p)
checkStmt (HsQualifier e)   = check1Expr e HsQualifier
checkStmt s@(HsLetStmt bs)  = returnP s

checkField (HsFieldUpdate n e) = check1Expr e (HsFieldUpdate n)

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SrcLoc -> HsExp -> HsRhs -> [HsDecl] -> P HsDecl
checkValDef srcloc lhs rhs whereBinds =
    case isFunLhs lhs [] of
	 Just (f,es) -> checkPatterns es `thenP` \ps ->
			returnP (HsFunBind [HsMatch srcloc f ps rhs whereBinds])
         Nothing     -> checkPattern lhs `thenP` \lhs ->
			returnP (HsPatBind srcloc lhs rhs whereBinds)

-- A variable binding is parsed as an HsPatBind.
-- ToDo: separate checks for valdefs in classes, in instances and elsewhere.

isFunLhs (HsInfixApp l (HsQVarOp (UnQual op)) r) es = Just (op, l:r:es)
isFunLhs (HsApp (HsVar (UnQual f)) e) es = Just (f, e:es)
isFunLhs (HsApp (HsParen f) e) es = isFunLhs f (e:es)
isFunLhs (HsApp f e) es = isFunLhs f (e:es)
isFunLhs _ _ = Nothing

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: HsQName -> P HsName
checkUnQual (Qual _ _) = parseError "Illegal qualified name"
checkUnQual (UnQual n) = returnP n

-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = returnP (fromInteger i)
checkPrec i | otherwise	       = parseError ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: HsExp -> [HsFieldUpdate] -> P HsExp
mkRecConstrOrUpdate (HsCon c) fs       = returnP (HsRecConstr c fs)
mkRecConstrOrUpdate exp       fs@(_:_) = returnP (HsRecUpdate exp fs)
mkRecConstrOrUpdate _         _        = parseError "Empty record update"
