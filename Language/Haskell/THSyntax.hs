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
unQ :: Q a -> IO a
unQ (Q x) = x

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
  lift = litExp . IntegerLit

instance Lift Int where
  lift = litExp . IntegerLit . fromIntegral

instance Lift Char where
  lift = litExp . CharLit

instance Lift a => Lift [a] where
  lift xs = listExp (map lift xs)

-- TH has a special form for literal strings,
-- which we should take advantage of
{-# RULES "TH:liftString" forall s. lift s = litExp (StringLit s) #-}



------------------------------------------------------

data Lit = CharLit Char 
	 | StringLit String 
	 | IntegerLit Integer 	  -- Used for overloaded and non-overloaded
                              -- literals. We don't have a good way to
                              -- represent non-overloaded literals at
                              -- the moment. Maybe that doesn't matter?
     | IntPrimLit Integer
     | FloatPrimLit Rational
     | DoublePrimLit Rational
	 | RationalLit Rational   -- Ditto
	 deriving( Show )

	-- We could add Int, Float, Double etc, as we do in HsLit, 
	-- but that could complicate the
	-- suppposedly-simple THSyntax literal type

data Pat 
  = LitPat Lit                      -- { 5 or 'c' }
  | VarPat String                   -- { x }
  | TupPat [Pat]                    -- { (p1,p2) }
  | ConPat String [Pat]             -- data T1 = C1 t1 t2; {C1 p1 p1} = e 
  | TildePat Pat                    -- { ~p }
  | AsPat String Pat                -- { x @ p }
  | WildPat                         -- { _ }
  | RecPat String [FieldPat]        -- f (Pt { pointx = x }) = g x
  deriving( Show )

type FieldPat = (String,Pat)

data Match = Match Pat RHS [Dec]
                                    -- case e of { pat -> body where decs } 
    deriving Show
data Clause = Clause [Pat] RHS [Dec]
                                    -- f { p1 p2 = body where decs }
    deriving Show
 
data Exp 
  = VarExp String                           -- { x }
  | ConExp String                           -- data T1 = C1 t1 t2; p = {C1} e1 e2  
  | LitExp Lit                              -- { 5 or 'c'}
  | AppExp Exp Exp                          -- { f x }

  | InfixExp (Maybe Exp) Exp (Maybe Exp)    -- {x + y} or {(x+)} or {(+ x)} or {(+)}
	-- It's a bit gruesome to use an Exp as the
	-- operator, but how else can we distinguish
	-- constructors from non-constructors?
	-- Maybe there should be a var-or-con type?
	-- Or maybe we should leave it to the String itself?

  | LamExp [Pat] Exp                        -- { \ p1 p2 -> e }
  | TupExp [Exp]                            -- { (e1,e2) }  
  | CondExp Exp Exp Exp                     -- { if e1 then e2 else e3 }
  | LetExp [Dec] Exp                        -- { let x=e1;   y=e2 in e3 }
  | CaseExp Exp [Match]                     -- { case e of m1; m2 }
  | DoExp [Stmt]                       -- { do { p <- e1; e2 }  }
  | CompExp [Stmt]                     -- { [ (x,y) | x <- xs, y <- ys ] }
  | ArithSeqExp DotDot                      -- { [ 1 ,2 .. 10 ] }
  | ListExp [ Exp ]                         -- { [1,2,3] }
  | SigExp Exp Typ                          -- e :: t
  | RecConExp String [FieldExp]             -- { T { x = y, z = w } }
  | RecUpdExp Exp [FieldExp]                -- { (f x) { z = w } }
  deriving( Show )

type FieldExp = (String,Exp)

-- Omitted: implicit parameters

data RHS
  = GuardedRHS [(Exp,Exp)]     -- f p { | e1 = e2 | e3 = e4 } where ds
  | NormalRHS Exp              -- f p { = e } where ds
  deriving( Show )

data Stmt
  = BindStmt Pat Exp
  | LetStmt [ Dec ]
  | NoBindStmt Exp
  | ParStmt [[Stmt]]
  deriving( Show )

data DotDot = FromDotDot Exp | FromThenDotDot Exp Exp
            | FromToDotDot Exp Exp | FromThenToDotDot Exp Exp Exp
	      deriving( Show )
  
data Dec 
  = FunDec String [Clause]                 -- { f p1 p2 = b where decs }
  | ValDec Pat RHS [Dec]         -- { p = b where decs }
  | DataDec Cxt String [String] 
         [Con] [String]                 -- { data Cxt x => T x = A x | B (T x) deriving (Z,W)}
  | NewtypeDec Cxt String [String] 
         Con [String]                   -- { newtype Cxt x => T x = A (B x) deriving (Z,W)}
  | TySynDec String [String] Typ           -- { type T x = (x,x) }
  | ClassDec Cxt String [String] [Dec]     -- { class Eq a => Ord a where ds }
  | InstanceDec Cxt Typ [Dec]              -- { instance Show w => Show [w] where ds }
  | SigDec String Typ                      -- { length :: [a] -> Int }
  | ForeignDec Foreign
  deriving( Show )

data Foreign = ImportForeign Callconv Safety String String Typ
             -- ExportForeign missing...
	     deriving( Show )

data Callconv = CCall | StdCall
	      deriving( Show )

data Safety = Unsafe | Safe | Threadsafe
	    deriving( Show )

type Cxt = [Typ]	-- (Eq a, Ord b)

data Strict = IsStrict | NotStrict
         deriving( Show )

data Con = NormalCon String [StrictTyp]
         | RecCon String [VarStrictTyp]
         | InfixCon StrictTyp String StrictTyp
         deriving( Show )

type StrictTyp = (Strict, Typ)
type StrictTypQ = Q StrictTyp
type VarStrictTyp = (String, Strict, Typ)
type VarStrictTypQ = Q VarStrictTyp

data Program = Program [ Dec ] 
             deriving( Show )

-- FIXME: Why this special status for "List" (even tuples might be handled
--	  differently)? -=chak
data Tag = TupleTag Int | ArrowTag | ListTag | ConNameTag String
         deriving (Eq, Show)

data Typ = ForallTyp [String] Cxt Typ  -- forall <vars>. <ctxt> -> <type>
         | VarTyp String               -- a
         | ConTyp Tag                  -- T or [] or (->) or (,,) etc
         | AppTyp Typ Typ              -- T a b
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
type RHSQ = Q RHS
type StmtQ = Q Stmt
type DotDotQ = Q DotDot

--runE :: ExpQ -> Exp
--runE x = runQ 0 x

--runP :: Pattern -> Pat
--runP x = x

--runD :: DecQ -> Dec
--runD d = runQ 0 d




-------------------- Lowercase pattern syntax functions ---

intPrimLit    :: Integer -> Lit
intPrimLit    = IntPrimLit
floatPrimLit  :: Rational -> Lit
floatPrimLit  = FloatPrimLit
doublePrimLit :: Rational -> Lit
doublePrimLit = DoublePrimLit
integerLit    :: Integer -> Lit
integerLit    = IntegerLit
charLit       :: Char -> Lit
charLit       = CharLit
stringLit     :: String -> Lit
stringLit     = StringLit
rationalLit   :: Rational -> Lit
rationalLit   = RationalLit

litPat :: Lit -> Pat
litPat = LitPat
varPat :: String -> Pat
varPat = VarPat
tupPat :: [Pat] -> Pat
tupPat = TupPat
conPat :: String -> [Pat] -> Pat
conPat = ConPat
tildePat :: Pat -> Pat
tildePat = TildePat
asPat :: String -> Pat -> Pat
asPat = AsPat
wildPat :: Pat
wildPat = WildPat
recPat :: String -> [FieldPat] -> Pat
recPat = RecPat

fieldPat :: String -> Pat -> (String, Pat)
fieldPat = (,)


--------------------------------------------------------------------------------
-- 	Stmt

bindStmt :: Pat -> ExpQ -> StmtQ
bindStmt p e = liftM (BindStmt p) e

letStmt :: [DecQ] -> StmtQ
letStmt ds = do { ds1 <- sequence ds; return (LetStmt ds1) }

noBindStmt :: ExpQ -> StmtQ
noBindStmt e = do { e1 <- e; return (NoBindStmt e1) }

parStmt :: [[StmtQ]] -> StmtQ
parStmt _ = fail "No parallel comprehensions yet"

--------------------------------------------------------------------------------
-- 	RHS

normalRHS :: ExpQ -> RHSQ
normalRHS e = do { e1 <- e; return (NormalRHS e1) }

guardedRHS :: [(ExpQ,ExpQ)] -> RHSQ
guardedRHS gs = do { gs1 <- mapM f gs; return (GuardedRHS gs1) }
	   where
		f (g,e) = do { g1 <- g; e1 <- e; return (g1,e1) }

--------------------------------------------------------------------------------
-- 	Match and Clause

match :: Pat -> RHSQ -> [DecQ] -> MatchQ
match p rhs ds = do { r' <- rhs;
                      ds' <- sequence ds;
                      return (Match p r' ds') }

clause :: [Pat] -> RHSQ -> [DecQ] -> ClauseQ
clause ps r ds = do { r' <- r;
                      ds' <- sequence ds;
                      return (Clause ps r' ds') }


---------------------------------------------------------------------------
-- 	Exp

global :: String -> ExpQ
global s = return (VarExp s)

varExp :: String -> ExpQ
varExp s = return (VarExp s)

conExp :: String -> ExpQ
conExp s =  return (ConExp s)

litExp :: Lit -> ExpQ
litExp c = return (LitExp c)

appExp :: ExpQ -> ExpQ -> ExpQ
appExp x y = do { a <- x; b <- y; return (AppExp a b)}

infixExp :: Maybe ExpQ -> ExpQ -> Maybe ExpQ -> ExpQ
infixExp (Just x) s (Just y) = do { a <- x; s' <- s; b <- y; return (InfixExp (Just a) s' (Just b))}
infixExp Nothing  s (Just y) = do { s' <- s; b <- y; return (InfixExp Nothing s' (Just b))}
infixExp (Just x) s Nothing  = do { a <- x; s' <- s; return (InfixExp (Just a) s' Nothing)}
infixExp Nothing  s Nothing  = do { s' <- s; return (InfixExp Nothing s' Nothing) }

infixApp :: ExpQ -> ExpQ -> ExpQ -> ExpQ
infixApp x y z = infixExp (Just x) y (Just z)
sectionL :: ExpQ -> ExpQ -> ExpQ
sectionL x y = infixExp (Just x) y Nothing
sectionR :: ExpQ -> ExpQ -> ExpQ
sectionR x y = infixExp Nothing x (Just y)

fromExp :: ExpQ -> ExpQ
fromExp x = do { a <- x; return (ArithSeqExp (FromDotDot a)) }  

fromThenExp :: ExpQ -> ExpQ -> ExpQ
fromThenExp x y = do { a <- x; b <- y; return (ArithSeqExp (FromThenDotDot a b)) }  

fromToExp :: ExpQ -> ExpQ -> ExpQ
fromToExp x y = do { a <- x; b <- y; return (ArithSeqExp (FromToDotDot a b)) }  

fromThenToExp :: ExpQ -> ExpQ -> ExpQ -> ExpQ
fromThenToExp x y z = do { a <- x; b <- y; c <- z; return (ArithSeqExp (FromThenToDotDot a b c)) }  

lamExp :: [Pat] -> ExpQ -> ExpQ
lamExp ps e = liftM (LamExp ps) e

lam1Exp :: Pat -> ExpQ -> ExpQ	-- Single-arg lambda
lam1Exp p e = lamExp [p] e

tupExp :: [ExpQ] -> ExpQ
tupExp es = do { es1 <- sequence es; return (TupExp es1)}

condExp :: ExpQ -> ExpQ -> ExpQ -> ExpQ
condExp x y z =  do { a <- x; b <- y; c <- z; return (CondExp a b c)}

letExp :: [DecQ] -> ExpQ -> ExpQ
letExp ds e = do { ds2 <- sequence ds; e2 <- e; return (LetExp ds2 e2) }

caseExp :: ExpQ -> [MatchQ] -> ExpQ
caseExp e ms = do { e1 <- e; ms1 <- sequence ms; return (CaseExp e1 ms1) } 

doExp :: [StmtQ] -> ExpQ
doExp ss = do { ss1 <- sequence ss; return (DoExp ss1) } 

compExp :: [StmtQ] -> ExpQ
compExp ss = do { ss1 <- sequence ss; return (CompExp ss1) } 

listExp :: [ExpQ] -> ExpQ
listExp es = do { es1 <- sequence es; return (ListExp es1) }

sigExp :: ExpQ -> TypQ -> ExpQ
sigExp e t = do { e1 <- e; t1 <- t; return (SigExp e1 t1) }

recConExp :: String -> [Q (String,Exp)] -> ExpQ
recConExp c fs = do { flds <- sequence fs; return (RecConExp c flds) }

recUpdExp :: ExpQ -> [Q (String,Exp)] -> ExpQ
recUpdExp e fs = do { e1 <- e; flds <- sequence fs; return (RecUpdExp e1 flds) }

stringExp :: String -> ExpQ
stringExp = litExp . stringLit

fieldExp :: String -> ExpQ -> Q (String, Exp)
fieldExp s e = do { e' <- e; return (s,e') }

--------------------------------------------------------------------------------
-- 	Dec

valDec :: Pat -> RHSQ -> [DecQ] -> DecQ
valDec p b ds = 
  do { ds' <- sequence ds
     ; b' <- b
     ; return (ValDec p b' ds')
     }

funDec :: String -> [ClauseQ] -> DecQ
funDec nm cs = 
 do { cs1 <- sequence cs
    ; return (FunDec nm cs1)
    }

tySynDec :: String -> [String] -> TypQ -> DecQ
tySynDec tc tvs rhs = do { rhs1 <- rhs; return (TySynDec tc tvs rhs1) }

dataDec :: CxtQ -> String -> [String] -> [ConQ] -> [String] -> DecQ
dataDec ctxt tc tvs cons derivs =
  do
    ctxt1 <- ctxt
    cons1 <- sequence cons
    return (DataDec ctxt1 tc tvs cons1 derivs)

newtypeDec :: CxtQ -> String -> [String] -> ConQ -> [String] -> DecQ
newtypeDec ctxt tc tvs con derivs =
  do
    ctxt1 <- ctxt
    con1 <- con
    return (NewtypeDec ctxt1 tc tvs con1 derivs)

classDec :: CxtQ -> String -> [String] -> [DecQ] -> DecQ
classDec ctxt cls tvs decs =
  do 
    decs1 <- sequence decs
    ctxt1 <- ctxt
    return $ ClassDec ctxt1 cls tvs decs1

instanceDec :: CxtQ -> TypQ -> [DecQ] -> DecQ
instanceDec ctxt ty decs =
  do 
    ctxt1 <- ctxt
    decs1 <- sequence decs
    ty1   <- ty
    return $ InstanceDec ctxt1 ty1 decs1

sigDec :: String -> TypQ -> DecQ
sigDec fun ty = liftM (SigDec fun) $ ty

cxt :: [TypQ] -> CxtQ
cxt = sequence

normalCon :: String -> [StrictTypQ] -> ConQ
normalCon con strtys = liftM (NormalCon con) $ sequence strtys

recCon :: String -> [VarStrictTypQ] -> ConQ
recCon con varstrtys = liftM (RecCon con) $ sequence varstrtys

infixCon :: Q (Strict, Typ) -> String -> Q (Strict, Typ) -> ConQ
infixCon st1 con st2 = do st1' <- st1
                          st2' <- st2
                          return $ InfixCon st1' con st2'


--------------------------------------------------------------------------------
-- 	Typ

forallTyp :: [String] -> CxtQ -> TypQ -> TypQ
forallTyp tvars ctxt ty = do
    ctxt1 <- ctxt
    ty1   <- ty
    return $ ForallTyp tvars ctxt1 ty1

varTyp :: String -> TypQ
varTyp = return . VarTyp

conTyp :: Tag -> TypQ
conTyp = return . ConTyp

appTyp :: TypQ -> TypQ -> TypQ
appTyp t1 t2 = do
	       t1' <- t1
	       t2' <- t2
	       return $ AppTyp t1' t2'

arrowTyp :: TypQ
arrowTyp = return $ ConTyp ArrowTag

listTyp :: TypQ
listTyp = return $ ConTyp ListTag

tupleTyp :: Int -> TypQ
tupleTyp i = return $ ConTyp (TupleTag i)

conNameTyp :: String -> TypQ
conNameTyp s = return $ ConTyp (ConNameTag s)

isStrict, notStrict :: Q Strict
isStrict = return $ IsStrict
notStrict = return $ NotStrict

strictTyp :: Q Strict -> TypQ -> StrictTypQ
strictTyp = liftM2 (,)

varStrictTyp :: String -> StrictTypQ -> VarStrictTypQ
varStrictTyp v st = do (s, t) <- st
                       return (v, s, t)

--------------------------------------------------------------
-- useful helper functions

combine :: [([(String, String)], Pat)] -> ([(String, String)], [Pat])
combine pairs = foldr f ([],[]) pairs
  where f (env,p) (es,ps) = (env++es,p:ps)

rename :: Pat -> Q ([(String, String)], Pat)
rename (LitPat c)  = return([],LitPat c)
rename (VarPat s)  = do { s1 <- gensym s; return([(s,s1)],VarPat s1) }
rename (TupPat pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,TupPat ps)
rename (ConPat nm pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,ConPat nm ps)
rename (TildePat p) = do { (env,p2) <- rename p; return(env,TildePat p2) }   
rename (AsPat s p) = 
   do { s1 <- gensym s; (env,p2) <- rename p; return((s,s1):env,AsPat s1 p2) }
rename WildPat = return([],WildPat)
rename (RecPat nm fs) = do { pairs <- mapM rename ps; g(combine pairs) }
    where g (env,ps') = return (env,RecPat nm (zip ss ps'))
          (ss,ps) = unzip fs

genpat :: Pat -> Q ((String -> ExpQ), Pat)
genpat p = do { (env,p2) <- rename p; return (alpha env,p2) }

alpha :: [(String, String)] -> String -> ExpQ
alpha env s = case lookup s env of
               Just x -> varExp x
               Nothing -> varExp s

--genPE s n = [ (pvar x, var x) | i <- [1..n], let x = s ++ show i ]

genPE :: String -> Integer -> ([Pat], [ExpQ])
genPE s n = let ns = [ s++(show i) | i <- [1..n]]
            in (map varPat ns, map varExp ns)

appsExp :: [ExpQ] -> ExpQ
appsExp [] = error "appsExp []"
appsExp [x] = x
appsExp (x:y:zs) = appsExp ( (appExp x y) : zs )

simpleMatch :: Pat -> Exp -> Match
simpleMatch p e = Match p (NormalRHS e) []


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
pprExpI _ (VarExp v)     = text v
pprExpI _ (ConExp c)     = text c
pprExpI i (LitExp l)     = pprLit i l
pprExpI i (AppExp e1 e2) = parensIf (i >= appPrec) $ pprExpI opPrec e1
                                                 <+> pprExpI appPrec e2
pprExpI i (InfixExp (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExpI opPrec e1
                          <+> pprExp op
                          <+> pprExpI opPrec e2
pprExpI _ (InfixExp me1 op me2) = parens $ pprMaybeExp noPrec me1
                                       <+> pprExp op
                                       <+> pprMaybeExp noPrec me2
pprExpI i (LamExp ps e) = parensIf (i > noPrec) $ char '\\'
                                               <> hsep (map pprPat ps)
                                              <+> text "->" <+> pprExp e
pprExpI _ (TupExp es) = parens $ sep $ punctuate comma $ map pprExp es
-- Nesting in Cond is to avoid potential problems in do statments
pprExpI i (CondExp guard true false)
 = parensIf (i > noPrec) $ sep [text "if" <+> pprExp guard,
                           nest 1 $ text "then" <+> pprExp true,
                           nest 1 $ text "else" <+> pprExp false]
pprExpI i (LetExp ds e)
    = parensIf (i > noPrec) $ text "let" <+> vcat (map pprDec ds)
                           $$ text " in" <+> pprExp e
pprExpI i (CaseExp e ms)
 = parensIf (i > noPrec) $ text "case" <+> pprExp e <+> text "of"
                        $$ nest nestDepth (vcat $ map pprMatch ms)
pprExpI i (DoExp ss) = parensIf (i > noPrec) $ text "do"
                                           <+> vcat (map pprStmt ss)
pprExpI _ (CompExp []) = error "Can't happen: pprExpI (CompExp [])"
-- This will probably break with fixity declarations - would need a ';'
pprExpI _ (CompExp ss) = text "[" <> pprStmt s
                     <+> text "|"
                     <+> (sep $ punctuate comma $ map pprStmt ss')
                      <> text "]"
  where s = last ss
        ss' = init ss
pprExpI _ (ArithSeqExp d) = pprDotDot d
pprExpI _ (ListExp es) = brackets $ sep $ punctuate comma $ map pprExp es
	-- 5 :: Int :: Int will break, but that's a silly thing to do anyway
pprExpI i (SigExp e t)
 = parensIf (i > noPrec) $ pprExp e <+> text "::" <+> pprTyp t
pprExpI _ (RecConExp nm fs) = text nm <> braces (pprFields fs)
pprExpI _ (RecUpdExp e fs) = pprExpI appPrec e <> braces (pprFields fs)

pprFields :: [(String,Exp)] -> Doc
pprFields = sep . punctuate comma
          . map (\(s,e) -> text s <+> equals <+> pprExp e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExpI i e

------------------------------
pprStmt :: Stmt -> Doc
pprStmt (BindStmt p e) = pprPat p <+> text "<-" <+> pprExp e
pprStmt (LetStmt ds) = text "let" <+> vcat (map pprDec ds)
pprStmt (NoBindStmt e) = pprExp e
pprStmt (ParStmt sss) = sep $ punctuate (text "|")
                      $ map (sep . punctuate comma . map pprStmt) sss

------------------------------
pprMatch :: Match -> Doc
pprMatch (Match p rhs ds) = pprPat p <+> pprRHS False rhs
                         $$ where_clause ds

------------------------------
pprRHS :: Bool -> RHS -> Doc
pprRHS eq (GuardedRHS xs) = nest nestDepth $ vcat $ map do_guard xs
  where eqd = if eq then text "=" else text "->"
        do_guard (lhs, rhs) = text "|" <+> pprExp lhs <+> eqd <+> pprExp rhs
pprRHS eq (NormalRHS e) = (if eq then text "=" else text "->")
                      <+> pprExp e

------------------------------
pprLit :: Precedence -> Lit -> Doc
pprLit i (IntPrimLit x)    = parensIf (i > noPrec && x < 0)
                                      (integer x <> char '#')
pprLit i (FloatPrimLit x)  = parensIf (i > noPrec && x < 0)
                                      (float (fromRational x) <> char '#')
pprLit i (DoublePrimLit x) = parensIf (i > noPrec && x < 0)
                                      (double (fromRational x) <> text "##")
pprLit i (IntegerLit x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (CharLit c)       = text (show c)
pprLit _ (StringLit s)     = text (show s)
pprLit i (RationalLit rat) = parensIf (i > noPrec) $ rational rat

------------------------------
pprPat :: Pat -> Doc
pprPat = pprPatI noPrec

pprPatI :: Precedence -> Pat -> Doc
pprPatI i (LitPat l)     = pprLit i l
pprPatI _ (VarPat v)     = text v
pprPatI _ (TupPat ps)    = parens $ sep $ punctuate comma $ map pprPat ps
pprPatI i (ConPat s ps)  = parensIf (i > noPrec) $ text s <+> sep (map (pprPatI appPrec) ps)
pprPatI i (TildePat p)   = parensIf (i > noPrec) $ pprPatI appPrec p
pprPatI i (AsPat v p) = parensIf (i > noPrec) $ text v <> text "@" <> pprPatI appPrec p
pprPatI _ WildPat = text "_"
pprPatI _ (RecPat nm fs)
 = parens $     text nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> text s <+> equals <+> pprPat p) fs)

------------------------------
pprDec :: Dec -> Doc
pprDec (FunDec f cs)   = vcat $ map (\c -> text f <+> pprClause c) cs
pprDec (ValDec p r ds) = pprPat p <+> pprRHS True r
                      $$ where_clause ds
pprDec (TySynDec t xs rhs) = text "type" <+> text t <+> hsep (map text xs) 
                         <+> text "=" <+> pprTyp rhs
pprDec (DataDec ctxt t xs cs decs)
    = text "data"
  <+> pprCxt ctxt
  <+> text t <+> hsep (map text xs)
  <+> sep (pref $ map pprCon cs)
   $$ if null decs
      then empty
      else nest nestDepth
         $ text "deriving"
       <+> parens (hsep $ punctuate comma $ map text decs)
    where pref :: [Doc] -> [Doc]
          pref [] = [char '='] -- Can't happen in H98
          pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds
pprDec (NewtypeDec ctxt t xs c decs)
    = text "newtype"
  <+> pprCxt ctxt
  <+> text t <+> hsep (map text xs)
  <+> char '=' <+> pprCon c
   $$ if null decs
      then empty
      else nest nestDepth
         $ text "deriving"
       <+> parens (hsep $ punctuate comma $ map text decs)
pprDec (ClassDec ctxt c xs ds) = text "class" <+> pprCxt ctxt
                             <+> text c <+> hsep (map text xs)
                              $$ where_clause ds
pprDec (InstanceDec ctxt i ds) = text "instance" <+> pprCxt ctxt <+> pprTyp i
                              $$ where_clause ds
pprDec (SigDec f t) = text f <+> text "::" <+> pprTyp t
pprDec (ForeignDec f) = pprForeign f

------------------------------
pprForeign :: Foreign -> Doc
pprForeign (ImportForeign callconv safety impent as typ)
    = text "foreign import"
  <+> showtextl callconv
  <+> showtextl safety
  <+> text (show impent)
  <+> text as
  <+> text "::" <+> pprTyp typ

------------------------------
pprClause :: Clause -> Doc
pprClause (Clause ps rhs ds) = hsep (map pprPat ps) <+> pprRHS True rhs
                            $$ where_clause ds

------------------------------
pprCon :: Con -> Doc
pprCon (NormalCon c sts) = text c <+> hsep (map pprStrictTyp sts)
pprCon (RecCon c vsts) = text c
                     <+> char '{'
                      <> hsep (punctuate comma $ map pprVarStrictTyp vsts)
                      <> char '}'
pprCon (InfixCon st1 c st2) = pprStrictTyp st1
                          <+> text c
                          <+> pprStrictTyp st2

------------------------------
pprVarStrictTyp :: (String, Strict, Typ) -> Doc
pprVarStrictTyp (v, str, t) = text v <+> text "::" <+> pprStrictTyp (str, t)

------------------------------
pprStrictTyp :: (Strict, Typ) -> Doc
pprStrictTyp (IsStrict, t) = char '!' <> pprTyp t
pprStrictTyp (NotStrict, t) = pprTyp t

------------------------------
pprParendTyp :: Typ -> Doc
pprParendTyp (VarTyp s) = text s
pprParendTyp (ConTyp t) = pprConTyp t
pprParendTyp other    = parens (pprTyp other)

pprTyp :: Typ -> Doc
pprTyp (ForallTyp tvars ctxt ty) = 
  text "forall" <+> hsep (map text tvars) <+> text "." <+> 
  ctxtDoc <+> pprTyp ty
  where
    ctxtDoc | null ctxt = empty
	    | otherwise = parens (sep (punctuate comma (map pprTyp ctxt))) <+>
			  text "=>"
pprTyp ty		       = pprTyApp (split ty)

pprTyApp :: (Typ, [Typ]) -> Doc
pprTyApp (ConTyp ArrowTag, [arg1,arg2])
  = sep [pprTyp arg1 <+> text "->", pprTyp arg2]

pprTyApp (ConTyp ListTag, [arg])
  = brackets (pprTyp arg)

pprTyApp (ConTyp (TupleTag n), args)
  | length args == n
  = parens (sep (punctuate comma (map pprTyp args)))

pprTyApp (fun, args)
  = pprParendTyp fun <+> sep (map pprParendTyp args)

pprConTyp :: Tag -> Doc
pprConTyp (TupleTag 0)   = text "()"
pprConTyp (TupleTag n)   = parens (hcat (replicate (n-1) comma))
pprConTyp ArrowTag       = parens (text "->")
pprConTyp ListTag        = text "[]"
pprConTyp (ConNameTag s) = text s

split :: Typ -> (Typ, [Typ])	-- Split into function and args
split t = go t []
	where
	  go (AppTyp t1 t2) args = go t1 (t2:args)
	  go ty             args = (ty, args)

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt [t] = pprTyp t <+> text "=>"
pprCxt ts = parens (hsep $ punctuate comma $ map pprTyp ts) <+> text "=>"

------------------------------
pprDotDot :: DotDot -> Doc
pprDotDot = brackets . pprDotDotI

pprDotDotI :: DotDot -> Doc
pprDotDotI (FromDotDot e) = pprExp e <> text ".."
pprDotDotI (FromThenDotDot e1 e2) = pprExp e1 <> text ","
                                 <> pprExp e2 <> text ".."
pprDotDotI (FromToDotDot e1 e2) = pprExp e1 <> text ".." <> pprExp e2
pprDotDotI (FromThenToDotDot e1 e2 e3) = pprExp e1 <> text ","
                                      <> pprExp e2 <> text ".."
                                      <> pprExp e3

------------------------------
where_clause :: [Dec] -> Doc
where_clause [] = empty
where_clause ds = text "where" <+> vcat (map pprDec ds)

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show

