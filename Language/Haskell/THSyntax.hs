-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.THSyntax
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Abstract syntax definitions for Template Haskell.
--
-----------------------------------------------------------------------------

module Language.Haskell.THSyntax 
-- FIXME: *urgh* we don't really want to export stuff like `counter'  -=chak
where

import Monad            ( liftM, liftM2, sequence )

import Data.IORef       ( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe ( unsafePerformIO )
import Text.PrettyPrint.HughesPJ
import Char (toLower)


-------------------------------------------------------
-- The quotation monad as IO

newtype Q a = Q (IO a)
unQ (Q a)   = a

instance Monad Q where
   return x    = Q (return x)
   (Q m) >>= k = Q (m >>= \r -> unQ (k r))
   fail s      = Q (fail s)
   
qIO :: IO a -> Q a
qIO io = Q io

runQ :: Q a -> IO a
runQ (Q io) = io

-- FIXME: What is the point of `returnQ', `bindQ, and `sequenceQ'?  As long as
--   Q is an instance of Monad, we get all this for free.  -=chak
--   Note: if this is to have these functions available in DsMeta, I think,
--   they should be moved to a different module (ie, separate the user-level
--   interface to THSyntax from the GHC-internal one)
--
returnQ :: a -> Q a
returnQ = return

bindQ :: Q a -> (a -> Q b) -> Q b
bindQ = (>>=)

sequenceQ :: [Q a] -> Q [a]
sequenceQ = sequence

-- global variable to generate unique symbols
--
counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)

gensym :: String -> Q String
gensym s = Q( do { n <- readIORef counter
                 ; writeIORef counter (n+1)
                 ; return(s++"'"++(show n)) })

class Lift t where
  lift :: t -> ExpQ
  
instance Lift Integer where
  lift = return . Lit . Integer

instance Lift Int where
  lift = return . Lit . Integer . fromIntegral

instance Lift Char where
  lift = return . Lit . Char

instance Lift a => Lift [a] where
  lift xs = listExp (map lift xs)

-- TH has a special form for literal strings,
-- which we should take advantage of
{-# RULES "TH:liftString" forall s. lift s = return (Lit (String s)) #-}



------------------------------------------------------

data Lit = Char Char 
	 | String String 
	 | Integer Integer 	-- Used for overloaded and non-overloaded literals
				-- We don't have a good way to represent non-overloaded
				-- literals at the moment.  Maybe that doesn't matter?
     | IntPrim Integer
     | FloatPrim Rational
     | DoublePrim Rational
	 | Rational Rational 	-- Ditto
	 deriving( Show )

	-- We could add Int, Float, Double etc, as we do in HsLit, 
	-- but that could complicate the
	-- suppposedly-simple THSyntax literal type

data Pat 
  = Plit Lit                      -- { 5 or 'c' }
  | Pvar String                   -- { x }
  | Ptup [Pat]                    -- { (p1,p2) }
  | Pcon String [Pat]             -- data T1 = C1 t1 t2; {C1 p1 p1} = e 
  | Ptilde Pat                    -- { ~p }
  | Paspat String Pat             -- { x @ p }
  | Pwild                         -- { _ }
  | Prec String [FieldPat]        -- f (Pt { pointx = x }) = g x
  deriving( Show )

type FieldPat = (String,Pat)

data Match = Match Pat RightHandSide [Dec]
                                    -- case e of { pat -> body where decs } 
    deriving Show
data Clause = Clause [Pat] RightHandSide [Dec]
                                    -- f { p1 p2 = body where decs }
    deriving Show
 
data Exp 
  = Var String                           -- { x }
  | Con String                           -- data T1 = C1 t1 t2; p = {C1} e1 e2  
  | Lit Lit                              -- { 5 or 'c'}
  | App Exp Exp                          -- { f x }

  | Infix (Maybe Exp) Exp (Maybe Exp)    -- {x + y} or {(x+)} or {(+ x)} or {(+)}
	-- It's a bit gruesome to use an Exp as the
	-- operator, but how else can we distinguish
	-- constructors from non-constructors?
	-- Maybe there should be a var-or-con type?
	-- Or maybe we should leave it to the String itself?

  | Lam [Pat] Exp                        -- { \ p1 p2 -> e }
  | Tup [Exp]                            -- { (e1,e2) }  
  | Cond Exp Exp Exp                     -- { if e1 then e2 else e3 }
  | Let [Dec] Exp                        -- { let x=e1;   y=e2 in e3 }
  | Case Exp [Match]                     -- { case e of m1; m2 }
  | Do [Statement]                       -- { do { p <- e1; e2 }  }
  | Comp [Statement]                     -- { [ (x,y) | x <- xs, y <- ys ] }
  | ArithSeq DotDot                      -- { [ 1 ,2 .. 10 ] }
  | ListExp [ Exp ]                      -- { [1,2,3] }
  | SigExp Exp Typ                       -- e :: t
  | RecCon String [FieldExp]             -- { T { x = y, z = w } }
  | RecUpd Exp [FieldExp]                -- { (f x) { z = w } }
  deriving( Show )

type FieldExp = (String,Exp)

-- Omitted: implicit parameters

data RightHandSide
  = Guarded [(Exp,Exp)]     -- f p { | e1 = e2 | e3 = e4 } where ds
  | Normal Exp              -- f p { = e } where ds
  deriving( Show )

data Statement
  = BindSt Pat Exp
  | LetSt [ Dec ]
  | NoBindSt Exp
  | ParSt [[Statement]]
  deriving( Show )

data DotDot = From Exp | FromThen Exp Exp | FromTo Exp Exp | FromThenTo Exp Exp Exp 
	      deriving( Show )
  
data Dec 
  = Fun String [Clause]                 -- { f p1 p2 = b where decs }
  | Val Pat RightHandSide [Dec]         -- { p = b where decs }
  | Data Cxt String [String] 
         [Con] [String]                 -- { data Cxt x => T x = A x | B (T x) deriving (Z,W)}
  | Newtype Cxt String [String] 
         Con [String]                   -- { newtype Cxt x => T x = A (B x) deriving (Z,W)}
  | TySyn String [String] Typ           -- { type T x = (x,x) }
  | Class Cxt String [String] [Dec]     -- { class Eq a => Ord a where ds }
  | Instance Cxt Typ [Dec]              -- { instance Show w => Show [w] where ds }
  | Proto String Typ                    -- { length :: [a] -> Int }
  | Foreign Foreign
  deriving( Show )

data Foreign = Import Callconv Safety String String Typ
             -- Export missing...
	     deriving( Show )

data Callconv = CCall | StdCall
	      deriving( Show )

data Safety = Unsafe | Safe | Threadsafe
	    deriving( Show )

type Cxt = [Typ]	-- (Eq a, Ord b)

data Strictness = Strict | NonStrict
         deriving( Show )

data Con = Constr String [(Strictness, Typ)]
         | RecConstr String [(String, Strictness, Typ)]
         | InfixConstr (Strictness, Typ) String (Strictness, Typ)
         deriving( Show )

type StrType = Q (Strictness, Typ)
type VarStrType = Q (String, Strictness, Typ)

data Program = Program [ Dec ] 
             deriving( Show )

-- FIXME: Why this special status for "List" (even tuples might be handled
--	  differently)? -=chak
data Tag = Tuple Int | Arrow | List | TconName String
         deriving (Eq, Show)

data Typ = TForall [String] Cxt Typ  -- forall <vars>. <ctxt> -> <type>
	 | Tvar String               -- a
         | Tcon Tag                  -- T or [] or (->) or (,,) etc
         | Tapp Typ Typ              -- T a b
 	 deriving( Show )
 
---------------------------------------------------
-- Combinator based types

type ExpQ = Q Exp
type DecQ = Q Dec
type ConQ = Q Con
type TypQ = Q Typ
type CxtQ = Q Cxt
type MatchQ = Q Match
type ClauseQ = Q Clause
type RightHandSideQ = Q RightHandSide
type StatementQ = Q Statement
type DotDotQ = Q DotDot

--runE :: ExpQ -> Exp
--runE x = runQ 0 x

--runP :: Pattern -> Pat
runP x = x

--runD :: DecQ -> Dec
--runD d = runQ 0 d




-------------------- Lowercase pattern syntax functions ---

intPrimL    = IntPrim
floatPrimL  = FloatPrim
doublePrimL = DoublePrim
integerL    = Integer
charL       = Char
stringL     = String
rationalL   = Rational

plit = Plit
pvar = Pvar
ptup = Ptup
pcon = Pcon
ptilde = Ptilde
paspat = Paspat
pwild = Pwild
prec = Prec

fieldPat :: String -> Pat -> (String, Pat)
fieldPat = (,)


--------------------------------------------------------------------------------
-- 	Statement

bindSt :: Pat -> ExpQ -> StatementQ
bindSt p e = liftM (BindSt p) e

letSt :: [DecQ] -> StatementQ
letSt ds = do { ds1 <- sequence ds; return (LetSt ds1) }

noBindSt :: ExpQ -> StatementQ
noBindSt e = do { e1 <- e; return (NoBindSt e1) }

parSt :: [[StatementQ]] -> StatementQ
parSt zs = fail "No parallel comprehensions yet"

--------------------------------------------------------------------------------
-- 	RightHandSide

normal :: ExpQ -> RightHandSideQ
normal e = do { e1 <- e; return (Normal e1) }

guarded :: [(ExpQ,ExpQ)] -> RightHandSideQ
guarded gs = do { gs1 <- mapM f gs; return (Guarded gs1) }
	   where
		f (g,e) = do { g1 <- g; e1 <- e; return (g1,e1) }

--------------------------------------------------------------------------------
-- 	Match and Clause

match :: Pat -> RightHandSideQ -> [DecQ] -> MatchQ
match p rhs ds = do { r' <- rhs;
                      ds' <- sequence ds;
                      return (Match p r' ds') }

clause :: [Pat] -> RightHandSideQ -> [DecQ] -> ClauseQ
clause ps r ds = do { r' <- r;
                      ds' <- sequence ds;
                      return (Clause ps r' ds') }


---------------------------------------------------------------------------
-- 	Exp

global :: String -> ExpQ
global s = return (Var s)

var :: String -> ExpQ
var s = return (Var s)

con :: String -> ExpQ
con s =  return (Con s)

lit :: Lit -> ExpQ
lit c = return (Lit c)

app :: ExpQ -> ExpQ -> ExpQ
app x y = do { a <- x; b <- y; return (App a b)}

infixE :: Maybe ExpQ -> ExpQ -> Maybe ExpQ -> ExpQ
infixE (Just x) s (Just y) = do { a <- x; s' <- s; b <- y; return (Infix (Just a) s' (Just b))}
infixE Nothing  s (Just y) = do { s' <- s; b <- y; return (Infix Nothing s' (Just b))}
infixE (Just x) s Nothing  = do { a <- x; s' <- s; return (Infix (Just a) s' Nothing)}
infixE Nothing  s Nothing  = do { s' <- s; return (Infix Nothing s' Nothing) }

infixApp x y z = infixE (Just x) y (Just z)
sectionL x y = infixE (Just x) y Nothing
sectionR x y = infixE Nothing x (Just y)

from :: ExpQ -> ExpQ
from x = do { a <- x; return (ArithSeq (From a)) }  

fromThen :: ExpQ -> ExpQ -> ExpQ
fromThen x y = do { a <- x; b <- y; return (ArithSeq (FromThen a b)) }  

fromTo :: ExpQ -> ExpQ -> ExpQ
fromTo x y = do { a <- x; b <- y; return (ArithSeq (FromTo a b)) }  

fromThenTo :: ExpQ -> ExpQ -> ExpQ -> ExpQ
fromThenTo x y z = do { a <- x; b <- y; c <- z; return (ArithSeq (FromThenTo a b c)) }  

lam :: [Pat] -> ExpQ -> ExpQ
lam ps e = liftM (Lam ps) e

lam1 :: Pat -> ExpQ -> ExpQ	-- Single-arg lambda
lam1 p e = lam [p] e

tup :: [ExpQ] -> ExpQ
tup es = do { es1 <- sequence es; return (Tup es1)}

cond :: ExpQ -> ExpQ -> ExpQ -> ExpQ
cond x y z =  do { a <- x; b <- y; c <- z; return (Cond a b c)}

letE :: [DecQ] -> ExpQ -> ExpQ
letE ds e = do { ds2 <- sequence ds; e2 <- e; return (Let ds2 e2) }

caseE :: ExpQ -> [MatchQ] -> ExpQ
caseE e ms = do { e1 <- e; ms1 <- sequence ms; return (Case e1 ms1) } 

doE :: [StatementQ] -> ExpQ
doE ss = do { ss1 <- sequence ss; return (Do ss1) } 

comp :: [StatementQ] -> ExpQ
comp ss = do { ss1 <- sequence ss; return (Comp ss1) } 

listExp :: [ExpQ] -> ExpQ
listExp es = do { es1 <- sequence es; return (ListExp es1) }

sigExp :: ExpQ -> TypQ -> ExpQ
sigExp e t = do { e1 <- e; t1 <- t; return (SigExp e1 t1) }

recCon :: String -> [Q (String,Exp)] -> ExpQ
recCon c fs = do { flds <- sequence fs; return (RecCon c flds) }

recUpd :: ExpQ -> [Q (String,Exp)] -> ExpQ
recUpd e fs = do { e1 <- e; flds <- sequence fs; return (RecUpd e1 flds) }

string :: String -> ExpQ
string = lit . String

fieldExp :: String -> ExpQ -> Q (String, Exp)
fieldExp s e = do { e' <- e; return (s,e') }

--------------------------------------------------------------------------------
-- 	Dec

val :: Pat -> RightHandSideQ -> [DecQ] -> DecQ
val p b ds = 
  do { ds' <- sequence ds
     ; b' <- b
     ; return (Val p b' ds')
     }

fun :: String -> [ClauseQ] -> DecQ
fun nm cs = 
 do { cs1 <- sequence cs
    ; return (Fun nm cs1)
    }

tySynD :: String -> [String] -> TypQ -> DecQ
tySynD tc tvs rhs = do { rhs1 <- rhs; return (TySyn tc tvs rhs1) }

dataD :: CxtQ -> String -> [String] -> [ConQ] -> [String] -> DecQ
dataD ctxt tc tvs cons derivs =
  do
    ctxt1 <- ctxt
    cons1 <- sequence cons
    return (Data ctxt1 tc tvs cons1 derivs)

newtypeD :: CxtQ -> String -> [String] -> ConQ -> [String] -> DecQ
newtypeD ctxt tc tvs con derivs =
  do
    ctxt1 <- ctxt
    con1 <- con
    return (Newtype ctxt1 tc tvs con1 derivs)

classD :: CxtQ -> String -> [String] -> [DecQ] -> DecQ
classD ctxt cls tvs decs =
  do 
    decs1 <- sequence decs
    ctxt1 <- ctxt
    return $ Class ctxt1 cls tvs decs1

inst :: CxtQ -> TypQ -> [DecQ] -> DecQ
inst ctxt ty decs =
  do 
    ctxt1 <- ctxt
    decs1 <- sequence decs
    ty1   <- ty
    return $ Instance ctxt1 ty1 decs1

proto :: String -> TypQ -> DecQ
proto fun ty = liftM (Proto fun) $ ty

cxt :: [TypQ] -> CxtQ
cxt = sequence

constr :: String -> [Q (Strictness, Typ)] -> ConQ
constr con strtys = liftM (Constr con) $ sequence strtys

recConstr :: String -> [Q (String, Strictness, Typ)] -> ConQ
recConstr con varstrtys = liftM (RecConstr con) $ sequence varstrtys

infixConstr :: Q (Strictness, Typ) -> String -> Q (Strictness, Typ) -> ConQ
infixConstr st1 con st2 = do st1' <- st1
                             st2' <- st2
                             return $ InfixConstr st1' con st2'


--------------------------------------------------------------------------------
-- 	Typ

tforall :: [String] -> CxtQ -> TypQ -> TypQ
tforall tvars ctxt ty = do
  do
    ctxt1 <- ctxt
    ty1   <- ty
    return $ TForall tvars ctxt1 ty1

tvar :: String -> TypQ
tvar = return . Tvar

tcon :: Tag -> TypQ
tcon = return . Tcon

tapp :: TypQ -> TypQ -> TypQ
tapp t1 t2 = do
	       t1' <- t1
	       t2' <- t2
	       return $ Tapp t1' t2'

arrowTyCon :: TypQ
arrowTyCon = return $ Tcon Arrow

listTyCon :: TypQ
listTyCon = return $ Tcon List

tupleTyCon :: Int -> TypQ
tupleTyCon i = return $ Tcon (Tuple i)

namedTyCon :: String -> TypQ
namedTyCon s = return $ Tcon (TconName s)

strict, nonstrict :: Q Strictness
strict = return $ Strict
nonstrict = return $ NonStrict

strictType :: Q Strictness -> TypQ -> Q (Strictness, Typ)
strictType = liftM2 (,)

varStrictType :: String -> Q (Strictness, Typ) -> Q (String, Strictness, Typ)
varStrictType v st = do (s, t) <- st
                        return (v, s, t)

--------------------------------------------------------------
-- useful helper functions

combine pairs = foldr f ([],[]) pairs
  where f (env,p) (es,ps) = (env++es,p:ps)

rename (Plit c)  = return([],Plit c)
rename (Pvar s)  = do { s1 <- gensym s; return([(s,s1)],Pvar s1) }
rename (Ptup ps) = do { pairs <- mapM rename ps; g(combine pairs) }
   where g (es,ps) = return (es,Ptup ps)
rename (Pcon nm ps) = do { pairs <- mapM rename ps; g(combine pairs) }
   where g (es,ps) = return (es,Pcon nm ps)
rename (Ptilde p) = do { (env,p2) <- rename p; return(env,Ptilde p2) }   
rename (Paspat s p) = 
   do { s1 <- gensym s; (env,p2) <- rename p; return((s,s1):env,Paspat s1 p2) }
rename Pwild = return([],Pwild)
rename (Prec nm fs) = do { pairs <- mapM rename ps; g(combine pairs) }
    where g (env,ps') = return (env,Prec nm (zip ss ps'))
          (ss,ps) = unzip fs

genpat p = do { (env,p2) <- rename p; return(alpha env,p2) }

alpha env s = case lookup s env of
               Just x -> return(Var x)
               Nothing -> return(Var s)

--genPE s n = [ (pvar x, var x) | i <- [1..n], let x = s ++ show i ]

genPE s n = let ns = [ s++(show i) | i <- [1..n]]
            in (map pvar ns,map var ns)

apps :: [ExpQ] -> ExpQ
apps [x] = x
apps (x:y:zs) = apps ( (app x y) : zs )

simpleM :: Pat -> Exp -> Match
simpleM p e = Match p (Normal e) []


--------------------------------------------------------------
-- 		A pretty printer (due to Ian Lynagh)
--------------------------------------------------------------

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, opPrec, noPrec :: Precedence
appPrec = 2	-- Argument of a function application
opPrec  = 1	-- Argument of an infix operator
noPrec  = 0	-- Others

parensIf :: Bool -> Doc -> Doc
parensIf True d = parens d
parensIf False d = d

------------------------------
pprExp :: Exp -> Doc
pprExp = pprExpI noPrec

pprExpI :: Precedence -> Exp -> Doc
pprExpI _ (Var v)     = text v
pprExpI _ (Con c)     = text c
pprExpI i (Lit l)     = pprLit i l
pprExpI i (App e1 e2) = parensIf (i >= appPrec) $ pprExpI opPrec e1
                                        <+> pprExpI appPrec e2
pprExpI i (Infix (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExpI opPrec e1
                          <+> pprExp op
                          <+> pprExpI opPrec e2
pprExpI _ (Infix me1 op me2) = parens $ pprMaybeExp noPrec me1
                                    <+> pprExp op
                                    <+> pprMaybeExp noPrec me2
pprExpI i (Lam ps e) = parensIf (i > noPrec) $ char '\\'
                                       <> hsep (map pprPat ps)
                                      <+> text "->" <+> pprExp e
pprExpI _ (Tup es) = parens $ sep $ punctuate comma $ map pprExp es
-- Nesting in Cond is to avoid potential problems in do statments
pprExpI i (Cond guard true false)
 = parensIf (i > noPrec) $ sep [text "if" <+> pprExp guard,
                           nest 1 $ text "then" <+> pprExp true,
                           nest 1 $ text "else" <+> pprExp false]
pprExpI i (Let ds e) = parensIf (i > noPrec) $ text "let" <+> vcat (map pprDec ds)
                                       $$ text " in" <+> pprExp e
pprExpI i (Case e ms)
 = parensIf (i > noPrec) $ text "case" <+> pprExp e <+> text "of"
                   $$ nest nestDepth (vcat $ map pprMatch ms)
pprExpI i (Do ss) = parensIf (i > noPrec) $ text "do"
                                   <+> vcat (map pprStatement ss)
pprExpI _ (Comp []) = error "Can't happen: pprExpI (Comp [])"
-- This will probably break with fixity declarations - would need a ';'
pprExpI _ (Comp ss) = text "[" <> pprStatement s
                  <+> text "|"
                  <+> (sep $ punctuate comma $ map pprStatement ss')
                   <> text "]"
  where s = last ss
        ss' = init ss
pprExpI _ (ArithSeq d) = pprDotDot d
pprExpI _ (ListExp es) = brackets $ sep $ punctuate comma $ map pprExp es
	-- 5 :: Int :: Int will break, but that's a silly thing to do anyway
pprExpI i (SigExp e t)
 = parensIf (i > noPrec) $ pprExp e <+> text "::" <+> pprTyp t
pprExpI _ (RecCon nm fs) = text nm <> braces (pprFields fs)
pprExpI _ (RecUpd e fs) = pprExpI appPrec e <> braces (pprFields fs)

pprFields :: [(String,Exp)] -> Doc
pprFields = sep . punctuate comma
          . map (\(s,e) -> text s <+> equals <+> pprExp e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExpI i e

------------------------------
pprStatement :: Statement -> Doc
pprStatement (BindSt p e) = pprPat p <+> text "<-" <+> pprExp e
pprStatement (LetSt ds) = text "let" <+> vcat (map pprDec ds)
pprStatement (NoBindSt e) = pprExp e
pprStatement (ParSt sss) = sep $ punctuate (text "|")
                         $ map (sep . punctuate comma . map pprStatement) sss

------------------------------
pprMatch :: Match -> Doc
pprMatch (Match p rhs ds) = pprPat p <+> pprRhs False rhs
                         $$ where_clause ds

------------------------------
pprRhs :: Bool -> RightHandSide -> Doc
pprRhs eq (Guarded xs) = nest nestDepth $ vcat $ map do_guard xs
  where eqd = if eq then text "=" else text "->"
        do_guard (lhs, rhs) = text "|" <+> pprExp lhs <+> eqd <+> pprExp rhs
pprRhs eq (Normal e) = (if eq then text "=" else text "->")
                   <+> pprExp e

------------------------------
pprLit :: Precedence -> Lit -> Doc
pprLit i (IntPrim x)    = parensIf (i > noPrec && x < 0)
                                   (integer x <> char '#')
pprLit i (FloatPrim x)  = parensIf (i > noPrec && x < 0)
                                   (float (fromRational x) <> char '#')
pprLit i (DoublePrim x) = parensIf (i > noPrec && x < 0)
                                   (double (fromRational x) <> text "##")
pprLit i (Integer x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (Char c)       = text (show c)
pprLit _ (String s)     = text (show s)
pprLit i (Rational rat) = parensIf (i > noPrec) $ rational rat

------------------------------
pprPat :: Pat -> Doc
pprPat = pprPatI noPrec

pprPatI :: Precedence -> Pat -> Doc
pprPatI i (Plit l)     = pprLit i l
pprPatI _ (Pvar v)     = text v
pprPatI _ (Ptup ps)    = parens $ sep $ punctuate comma $ map pprPat ps
pprPatI i (Pcon s ps)  = parensIf (i > noPrec) $ text s <+> sep (map (pprPatI appPrec) ps)
pprPatI i (Ptilde p)   = parensIf (i > noPrec) $ pprPatI appPrec p
pprPatI i (Paspat v p) = parensIf (i > noPrec) $ text v <> text "@" <> pprPatI appPrec p
pprPatI _ Pwild = text "_"
pprPatI _ (Prec nm fs)
 = parens $     text nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> text s <+> equals <+> pprPat p) fs)

------------------------------
pprDec :: Dec -> Doc
pprDec (Fun f cs)   = vcat $ map (\c -> text f <+> pprClause c) cs
pprDec (Val p r ds) = pprPat p <+> pprRhs True r
                      $$ where_clause ds
pprDec (TySyn t xs rhs) = text "type" <+> text t <+> hsep (map text xs) 
				<+> text "=" <+> pprTyp rhs
pprDec (Data cxt t xs cs ds) = text "data"
                       <+> pprCxt cxt
                       <+> text t <+> hsep (map text xs)
                       <+> sep (pref $ map pprCon cs)
                        $$ if null ds
                           then empty
                           else nest nestDepth
                              $ text "deriving"
                            <+> parens (hsep $ punctuate comma $ map text ds)
    where pref :: [Doc] -> [Doc]
          pref [] = [char '='] -- Can't happen in H98
          pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds
pprDec (Newtype cxt t xs c ds) = text "newtype"
                       <+> pprCxt cxt
                       <+> text t <+> hsep (map text xs)
                       <+> char '=' <+> pprCon c
                        $$ if null ds
                           then empty
                           else nest nestDepth
                              $ text "deriving"
                            <+> parens (hsep $ punctuate comma $ map text ds)
pprDec (Class cxt c xs ds) = text "class" <+> pprCxt cxt
                         <+> text c <+> hsep (map text xs)
                          $$ where_clause ds
pprDec (Instance cxt i ds) = text "instance" <+> pprCxt cxt <+> pprTyp i
                          $$ where_clause ds
pprDec (Proto f t) = text f <+> text "::" <+> pprTyp t
pprDec (Foreign f) = pprForeign f

------------------------------
pprForeign :: Foreign -> Doc
pprForeign (Import callconv safety impent as typ) = text "foreign import"
                                                <+> showtextl callconv
                                                <+> showtextl safety
                                                <+> text (show impent)
                                                <+> text as
                                                <+> text "::" <+> pprTyp typ

------------------------------
pprClause :: Clause -> Doc
pprClause (Clause ps rhs ds) = hsep (map pprPat ps) <+> pprRhs True rhs
                            $$ where_clause ds

------------------------------
pprCon :: Con -> Doc
pprCon (Constr c sts) = text c <+> hsep (map pprStrictTyp sts)
pprCon (RecConstr c vsts) = text c
                        <+> char '{'
                         <> hsep (punctuate comma $ map pprVarStrictTyp vsts)
                         <> char '}'
pprCon (InfixConstr st1 c st2) = pprStrictTyp st1
                             <+> text c
                             <+> pprStrictTyp st2

------------------------------
pprVarStrictTyp :: (String, Strictness, Typ) -> Doc
pprVarStrictTyp (v, str, t) = text v <+> text "::" <+> text str' <> pprTyp t
    where str' = case str of
                     Strict -> "!"
                     NonStrict -> ""

------------------------------
pprStrictTyp :: (Strictness, Typ) -> Doc
pprStrictTyp (Strict, t) = char '!' <> pprTyp t
pprStrictTyp (NonStrict, t) = pprTyp t

------------------------------
pprParendTyp :: Typ -> Doc
pprParendTyp (Tvar s) = text s
pprParendTyp (Tcon t) = pprTcon t
pprParendTyp other    = parens (pprTyp other)

pprTyp :: Typ -> Doc
pprTyp (TForall tvars ctxt ty) = 
  text "forall" <+> hsep (map text tvars) <+> text "." <+> 
  ctxtDoc <+> pprTyp ty
  where
    ctxtDoc | null ctxt = empty
	    | otherwise = parens (sep (punctuate comma (map pprTyp ctxt))) <+>
			  text "=>"
pprTyp ty		       = pprTyApp (split ty)

pprTyApp (Tcon Arrow, [arg1,arg2])
  = sep [pprTyp arg1 <+> text "->", pprTyp arg2]

pprTyApp (Tcon List, [arg])
  = brackets (pprTyp arg)

pprTyApp (Tcon (Tuple n), args)
  | length args == n
  = parens (sep (punctuate comma (map pprTyp args)))

pprTyApp (fun, args)
  = pprParendTyp fun <+> sep (map pprParendTyp args)

pprTcon :: Tag -> Doc
pprTcon (Tuple 0)    = text "()"
pprTcon (Tuple n)    = parens (hcat (replicate (n-1) comma))
pprTcon Arrow	     = parens (text "->")
pprTcon List	     = text "[]"
pprTcon (TconName s) = text s

split :: Typ -> (Typ, [Typ])	-- Split into function and args
split t = go t []
	where
	  go (Tapp t1 t2) args = go t1 (t2:args)
	  go ty		  args = (ty, args)

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt [t] = pprTyp t <+> text "=>"
pprCxt ts = parens (hsep $ punctuate comma $ map pprTyp ts) <+> text "=>"

------------------------------
pprDotDot :: DotDot -> Doc
pprDotDot = brackets . pprDotDotI

pprDotDotI :: DotDot -> Doc
pprDotDotI (From e) = pprExp e <> text ".."
pprDotDotI (FromThen e1 e2) = pprExp e1 <> text ","
                           <> pprExp e2 <> text ".."
pprDotDotI (FromTo e1 e2) = pprExp e1 <> text ".." <> pprExp e2
pprDotDotI (FromThenTo e1 e2 e3) = pprExp e1 <> text ","
                                <> pprExp e2 <> text ".."
                                <> pprExp e3

------------------------------
where_clause :: [Dec] -> Doc
where_clause [] = empty
where_clause ds = text "where" <+> vcat (map pprDec ds)

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show
