-- THLib contains lots of useful helper functions for
-- generating and manipulating Template Haskell terms

module Language.Haskell.TH.THLib where
	-- All of the exports from this module should
	-- be "public" functions.  The main module TH
	-- re-exports them all.

import Text.PrettyPrint.HughesPJ
import Language.Haskell.TH.THSyntax
import Control.Monad( liftM, liftM2 )
import Data.Char	( toLower )

----------------------------------------------------------
--		Type synonyms
----------------------------------------------------------

type InfoQ   	    = Q Info
type ExpQ   	    = Q Exp
type DecQ   	    = Q Dec
type ConQ   	    = Q Con
type TypeQ  	    = Q Type
type CxtQ   	    = Q Cxt
type MatchQ 	    = Q Match
type ClauseQ	    = Q Clause
type BodyQ	    = Q Body
type StmtQ	    = Q Stmt
type RangeQ 	    = Q Range
type StrictTypeQ    = Q StrictType
type VarStrictTypeQ = Q VarStrictType

----------------------------------------------------------
--	 Lowercase pattern syntax functions
----------------------------------------------------------

intPrimL    :: Integer -> Lit
intPrimL    = IntPrimL
floatPrimL  :: Rational -> Lit
floatPrimL  = FloatPrimL
doublePrimL :: Rational -> Lit
doublePrimL = DoublePrimL
integerL    :: Integer -> Lit
integerL    = IntegerL
charL       :: Char -> Lit
charL       = CharL
stringL     :: String -> Lit
stringL     = StringL
rationalL   :: Rational -> Lit
rationalL   = RationalL

litP :: Lit -> Pat
litP = LitP
varP :: Name -> Pat
varP = VarP
tupP :: [Pat] -> Pat
tupP = TupP
conP :: Name -> [Pat] -> Pat
conP = ConP
tildeP :: Pat -> Pat
tildeP = TildeP
asP :: Name -> Pat -> Pat
asP = AsP
wildP :: Pat
wildP = WildP
recP :: Name -> [FieldPat] -> Pat
recP = RecP
listP :: [Pat] -> Pat
listP = ListP

fieldPat :: Name -> Pat -> (Name, Pat)
fieldPat = (,)


-------------------------------------------------------------------------------
--     Stmt

bindS :: Pat -> ExpQ -> StmtQ
bindS p e = liftM (BindS p) e

letS :: [DecQ] -> StmtQ
letS ds = do { ds1 <- sequence ds; return (LetS ds1) }

noBindS :: ExpQ -> StmtQ
noBindS e = do { e1 <- e; return (NoBindS e1) }

parS :: [[StmtQ]] -> StmtQ
parS _ = fail "No parallel comprehensions yet"

-------------------------------------------------------------------------------
--     Range

fromR :: ExpQ -> RangeQ
fromR x = do { a <- x; return (FromR a) }  

fromThenR :: ExpQ -> ExpQ -> RangeQ
fromThenR x y = do { a <- x; b <- y; return (FromThenR a b) }  

fromToR :: ExpQ -> ExpQ -> RangeQ
fromToR x y = do { a <- x; b <- y; return (FromToR a b) }  

fromThenToR :: ExpQ -> ExpQ -> ExpQ -> RangeQ
fromThenToR x y z = do { a <- x; b <- y; c <- z;
                         return (FromThenToR a b c) }  
-------------------------------------------------------------------------------
--     Body

normalB :: ExpQ -> BodyQ
normalB e = do { e1 <- e; return (NormalB e1) }

guardedB :: [(ExpQ,ExpQ)] -> BodyQ
guardedB ges = do { ges' <- mapM f ges; return (GuardedB ges') }
    where f (g, e) = do { g' <- g; e' <- e; return (g', e') }

-------------------------------------------------------------------------------
--     Match and Clause

match :: Pat -> BodyQ -> [DecQ] -> MatchQ
match p rhs ds = do { r' <- rhs;
                      ds' <- sequence ds;
                      return (Match p r' ds') }

clause :: [Pat] -> BodyQ -> [DecQ] -> ClauseQ
clause ps r ds = do { r' <- r;
                      ds' <- sequence ds;
                      return (Clause ps r' ds') }


---------------------------------------------------------------------------
--     Exp

global :: Name -> ExpQ
global s = return (VarE s)

varE :: Name -> ExpQ
varE s = return (VarE s)

conE :: Name -> ExpQ
conE s =  return (ConE s)

litE :: Lit -> ExpQ
litE c = return (LitE c)

appE :: ExpQ -> ExpQ -> ExpQ
appE x y = do { a <- x; b <- y; return (AppE a b)}

infixE :: Maybe ExpQ -> ExpQ -> Maybe ExpQ -> ExpQ
infixE (Just x) s (Just y) = do { a <- x; s' <- s; b <- y;
                                  return (InfixE (Just a) s' (Just b))}
infixE Nothing  s (Just y) = do { s' <- s; b <- y;
                                  return (InfixE Nothing s' (Just b))}
infixE (Just x) s Nothing  = do { a <- x; s' <- s;
                                  return (InfixE (Just a) s' Nothing)}
infixE Nothing  s Nothing  = do { s' <- s; return (InfixE Nothing s' Nothing) }

infixApp :: ExpQ -> ExpQ -> ExpQ -> ExpQ
infixApp x y z = infixE (Just x) y (Just z)
sectionL :: ExpQ -> ExpQ -> ExpQ
sectionL x y = infixE (Just x) y Nothing
sectionR :: ExpQ -> ExpQ -> ExpQ
sectionR x y = infixE Nothing x (Just y)

lamE :: [Pat] -> ExpQ -> ExpQ
lamE ps e = liftM (LamE ps) e

lam1E :: Pat -> ExpQ -> ExpQ    -- Single-arg lambda
lam1E p e = lamE [p] e

tupE :: [ExpQ] -> ExpQ
tupE es = do { es1 <- sequence es; return (TupE es1)}

condE :: ExpQ -> ExpQ -> ExpQ -> ExpQ
condE x y z =  do { a <- x; b <- y; c <- z; return (CondE a b c)}

letE :: [DecQ] -> ExpQ -> ExpQ
letE ds e = do { ds2 <- sequence ds; e2 <- e; return (LetE ds2 e2) }

caseE :: ExpQ -> [MatchQ] -> ExpQ
caseE e ms = do { e1 <- e; ms1 <- sequence ms; return (CaseE e1 ms1) } 

doE :: [StmtQ] -> ExpQ
doE ss = do { ss1 <- sequence ss; return (DoE ss1) } 

compE :: [StmtQ] -> ExpQ
compE ss = do { ss1 <- sequence ss; return (CompE ss1) } 

arithSeqE :: RangeQ -> ExpQ
arithSeqE r = do { r' <- r; return (ArithSeqE r') }  

-- arithSeqE Shortcuts
fromE :: ExpQ -> ExpQ
fromE x = do { a <- x; return (ArithSeqE (FromR a)) }  

fromThenE :: ExpQ -> ExpQ -> ExpQ
fromThenE x y = do { a <- x; b <- y; return (ArithSeqE (FromThenR a b)) }  

fromToE :: ExpQ -> ExpQ -> ExpQ
fromToE x y = do { a <- x; b <- y; return (ArithSeqE (FromToR a b)) }  

fromThenToE :: ExpQ -> ExpQ -> ExpQ -> ExpQ
fromThenToE x y z = do { a <- x; b <- y; c <- z;
                         return (ArithSeqE (FromThenToR a b c)) }  
-- End arithSeqE shortcuts

listE :: [ExpQ] -> ExpQ
listE es = do { es1 <- sequence es; return (ListE es1) }

sigE :: ExpQ -> TypeQ -> ExpQ
sigE e t = do { e1 <- e; t1 <- t; return (SigE e1 t1) }

recConE :: Name -> [Q (Name,Exp)] -> ExpQ
recConE c fs = do { flds <- sequence fs; return (RecConE c flds) }

recUpdE :: ExpQ -> [Q (Name,Exp)] -> ExpQ
recUpdE e fs = do { e1 <- e; flds <- sequence fs; return (RecUpdE e1 flds) }

stringE :: String -> ExpQ
stringE = litE . stringL

fieldExp :: Name -> ExpQ -> Q (Name, Exp)
fieldExp s e = do { e' <- e; return (s,e') }

-------------------------------------------------------------------------------
--     Dec

valD :: Pat -> BodyQ -> [DecQ] -> DecQ
valD p b ds = 
  do { ds' <- sequence ds
     ; b' <- b
     ; return (ValD p b' ds')
     }

funD :: Name -> [ClauseQ] -> DecQ
funD nm cs = 
 do { cs1 <- sequence cs
    ; return (FunD nm cs1)
    }

tySynD :: Name -> [Name] -> TypeQ -> DecQ
tySynD tc tvs rhs = do { rhs1 <- rhs; return (TySynD tc tvs rhs1) }

dataD :: CxtQ -> Name -> [Name] -> [ConQ] -> [Name] -> DecQ
dataD ctxt tc tvs cons derivs =
  do
    ctxt1 <- ctxt
    cons1 <- sequence cons
    return (DataD ctxt1 tc tvs cons1 derivs)

newtypeD :: CxtQ -> Name -> [Name] -> ConQ -> [Name] -> DecQ
newtypeD ctxt tc tvs con derivs =
  do
    ctxt1 <- ctxt
    con1 <- con
    return (NewtypeD ctxt1 tc tvs con1 derivs)

classD :: CxtQ -> Name -> [Name] -> [DecQ] -> DecQ
classD ctxt cls tvs decs =
  do 
    decs1 <- sequence decs
    ctxt1 <- ctxt
    return $ ClassD ctxt1 cls tvs decs1

instanceD :: CxtQ -> TypeQ -> [DecQ] -> DecQ
instanceD ctxt ty decs =
  do 
    ctxt1 <- ctxt
    decs1 <- sequence decs
    ty1   <- ty
    return $ InstanceD ctxt1 ty1 decs1

sigD :: Name -> TypeQ -> DecQ
sigD fun ty = liftM (SigD fun) $ ty

cxt :: [TypeQ] -> CxtQ
cxt = sequence

normalC :: Name -> [StrictTypeQ] -> ConQ
normalC con strtys = liftM (NormalC con) $ sequence strtys

recC :: Name -> [VarStrictTypeQ] -> ConQ
recC con varstrtys = liftM (RecC con) $ sequence varstrtys

infixC :: Q (Strict, Type) -> Name -> Q (Strict, Type) -> ConQ
infixC st1 con st2 = do st1' <- st1
                        st2' <- st2
                        return $ InfixC st1' con st2'


-------------------------------------------------------------------------------
--     Type

forallT :: [Name] -> CxtQ -> TypeQ -> TypeQ
forallT tvars ctxt ty = do
    ctxt1 <- ctxt
    ty1   <- ty
    return $ ForallT tvars ctxt1 ty1

varT :: Name -> TypeQ
varT = return . VarT

conT :: Name -> TypeQ
conT = return . ConT

appT :: TypeQ -> TypeQ -> TypeQ
appT t1 t2 = do
           t1' <- t1
           t2' <- t2
           return $ AppT t1' t2'

arrowT :: TypeQ
arrowT = return ArrowT

listT :: TypeQ
listT = return ListT

tupleT :: Int -> TypeQ
tupleT i = return (TupleT i)

isStrict, notStrict :: Q Strict
isStrict = return $ IsStrict
notStrict = return $ NotStrict

strictType :: Q Strict -> TypeQ -> StrictTypeQ
strictType = liftM2 (,)

varStrictType :: Name -> StrictTypeQ -> VarStrictTypeQ
varStrictType v st = do (s, t) <- st
                        return (v, s, t)

--------------------------------------------------------------
-- 	Useful helper functions

combine :: [([(Name, Name)], Pat)] -> ([(Name, Name)], [Pat])
combine pairs = foldr f ([],[]) pairs
  where f (env,p) (es,ps) = (env++es,p:ps)

rename :: Pat -> Q ([(Name, Name)], Pat)
rename (LitP c)  = return([],LitP c)
rename (VarP s)  = do { s1 <- newName (nameBase s); return([(s,s1)],VarP s1) }
rename (TupP pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,TupP ps)
rename (ConP nm pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,ConP nm ps)
rename (TildeP p) = do { (env,p2) <- rename p; return(env,TildeP p2) }   
rename (AsP s p) = 
   do { s1 <- newName (nameBase s); (env,p2) <- rename p; return((s,s1):env,AsP s1 p2) }
rename WildP = return([],WildP)
rename (RecP nm fs) = do { pairs <- mapM rename ps; g(combine pairs) }
    where g (env,ps') = return (env,RecP nm (zip ss ps'))
          (ss,ps) = unzip fs
rename (ListP pats) = do { pairs <- mapM rename pats; g(combine pairs) }
   where g (es,ps) = return (es,ListP ps)

genpat :: Pat -> Q ((Name -> ExpQ), Pat)
genpat p = do { (env,p2) <- rename p; return (alpha env,p2) }

alpha :: [(Name, Name)] -> Name -> ExpQ
alpha env s = case lookup s env of
               Just x -> varE x
               Nothing -> varE s

appsE :: [ExpQ] -> ExpQ
appsE [] = error "appsExp []"
appsE [x] = x
appsE (x:y:zs) = appsE ( (appE x y) : zs )

simpleMatch :: Pat -> Exp -> Match
simpleMatch p e = Match p (NormalB e) []


 

--------------------------------------------------------------
--         A pretty printer (due to Ian Lynagh)
--------------------------------------------------------------

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, opPrec, noPrec :: Precedence
appPrec = 2    -- Argument of a function application
opPrec  = 1    -- Argument of an infix operator
noPrec  = 0    -- Others

parensIf :: Bool -> Doc -> Doc
parensIf True d = parens d
parensIf False d = d

------------------------------
pprName :: Name -> Doc
pprName v = text (show v)

------------------------------
pprInfo :: Info -> Doc
pprInfo (ClassI d) = pprDec d
pprInfo (TyConI d) = pprDec d

pprInfo (ClassOpI v ty cls fix) 
  = text "Class op from" <+> pprName cls <> colon <+>
	vcat [ppr_sig v ty, pprFixity v fix]

pprInfo (DataConI v ty tc fix) 
  = text "Constructor from" <+> pprName tc <> colon <+>
	vcat [ppr_sig v ty, pprFixity v fix]

pprInfo (TyVarI v ty)
  = text "Type variable" <+> pprName v <+> equals <+> pprType ty

pprInfo (VarI v ty mb_d fix) 
  = vcat [ppr_sig v ty, pprFixity v fix, 
	  case mb_d of { Nothing -> empty; Just d -> pprDec d }]

ppr_sig v ty = pprName v <+> text "::" <+> pprType ty

pprFixity :: Name -> Fixity -> Doc
pprFixity v f | f == defaultFixity = empty
pprFixity v (Fixity i d) = ppr_fix d <+> int i <+> pprName v
			 where
			   ppr_fix InfixR = text "infixr"
			   ppr_fix InfixL = text "infixl"
			   ppr_fix InfixN = text "infix"


------------------------------
pprExp :: Exp -> Doc
pprExp = pprExpI noPrec

pprExpI :: Precedence -> Exp -> Doc
pprExpI _ (VarE v)     = pprName v
pprExpI _ (ConE c)     = pprName c
pprExpI i (LitE l)     = pprLit i l
pprExpI i (AppE e1 e2) = parensIf (i >= appPrec) $ pprExpI opPrec e1
                                               <+> pprExpI appPrec e2
pprExpI i (InfixE (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExpI opPrec e1
                        <+> pprExp op
                        <+> pprExpI opPrec e2
pprExpI _ (InfixE me1 op me2) = parens $ pprMaybeExp noPrec me1
                                     <+> pprExp op
                                     <+> pprMaybeExp noPrec me2
pprExpI i (LamE ps e) = parensIf (i > noPrec) $ char '\\'
                                             <> hsep (map pprPat ps)
                                            <+> text "->" <+> pprExp e
pprExpI _ (TupE es) = parens $ sep $ punctuate comma $ map pprExp es
-- Nesting in Cond is to avoid potential problems in do statments
pprExpI i (CondE guard true false)
 = parensIf (i > noPrec) $ sep [text "if" <+> pprExp guard,
                           nest 1 $ text "then" <+> pprExp true,
                           nest 1 $ text "else" <+> pprExp false]
pprExpI i (LetE ds e)
    = parensIf (i > noPrec) $ text "let" <+> vcat (map pprDec ds)
                           $$ text " in" <+> pprExp e
pprExpI i (CaseE e ms)
 = parensIf (i > noPrec) $ text "case" <+> pprExp e <+> text "of"
                        $$ nest nestDepth (vcat $ map pprMatch ms)
pprExpI i (DoE ss) = parensIf (i > noPrec) $ text "do"
                                         <+> vcat (map pprStmt ss)
pprExpI _ (CompE []) = error "Can't happen: pprExpI (CompExp [])"
-- This will probably break with fixity declarations - would need a ';'
pprExpI _ (CompE ss) = text "[" <> pprStmt s
                   <+> text "|"
                   <+> (sep $ punctuate comma $ map pprStmt ss')
                    <> text "]"
  where s = last ss
        ss' = init ss
pprExpI _ (ArithSeqE d) = pprRange d
pprExpI _ (ListE es) = brackets $ sep $ punctuate comma $ map pprExp es
    -- 5 :: Int :: Int will break, but that's a silly thing to do anyway
pprExpI i (SigE e t)
 = parensIf (i > noPrec) $ pprExp e <+> text "::" <+> pprType t
pprExpI _ (RecConE nm fs) = pprName nm <> braces (pprFields fs)
pprExpI _ (RecUpdE e fs) = pprExpI appPrec e <> braces (pprFields fs)

pprFields :: [(Name,Exp)] -> Doc
pprFields = sep . punctuate comma
          . map (\(s,e) -> pprName s <+> equals <+> pprExp e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExpI i e

------------------------------
pprStmt :: Stmt -> Doc
pprStmt (BindS p e) = pprPat p <+> text "<-" <+> pprExp e
pprStmt (LetS ds) = text "let" <+> vcat (map pprDec ds)
pprStmt (NoBindS e) = pprExp e
pprStmt (ParS sss) = sep $ punctuate (text "|")
                      $ map (sep . punctuate comma . map pprStmt) sss

------------------------------
pprMatch :: Match -> Doc
pprMatch (Match p rhs ds) = pprPat p <+> pprBody False rhs
                         $$ where_clause ds

------------------------------
pprBody :: Bool -> Body -> Doc
pprBody eq (GuardedB xs) = nest nestDepth $ vcat $ map do_guard xs
  where eqd = if eq then text "=" else text "->"
        do_guard (lhs, rhs) = text "|" <+> pprExp lhs <+> eqd <+> pprExp rhs
pprBody eq (NormalB e) = (if eq then text "=" else text "->")
                        <+> pprExp e

------------------------------
pprLit :: Precedence -> Lit -> Doc
pprLit i (IntPrimL x)    = parensIf (i > noPrec && x < 0)
                                    (integer x <> char '#')
pprLit i (FloatPrimL x)  = parensIf (i > noPrec && x < 0)
                                    (float (fromRational x) <> char '#')
pprLit i (DoublePrimL x) = parensIf (i > noPrec && x < 0)
                                    (double (fromRational x) <> text "##")
pprLit i (IntegerL x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (CharL c)       = text (show c)
pprLit _ (StringL s)     = text (show s)
pprLit i (RationalL rat) = parensIf (i > noPrec) $ rational rat

------------------------------
pprPat :: Pat -> Doc
pprPat = pprPatI noPrec

pprPatI :: Precedence -> Pat -> Doc
pprPatI i (LitP l)     = pprLit i l
pprPatI _ (VarP v)     = pprName v
pprPatI _ (TupP ps)    = parens $ sep $ punctuate comma $ map pprPat ps
pprPatI i (ConP s ps)  = parensIf (i > noPrec) $ pprName s
                                             <+> sep (map (pprPatI appPrec) ps)
pprPatI i (TildeP p)   = parensIf (i > noPrec) $ pprPatI appPrec p
pprPatI i (AsP v p)    = parensIf (i > noPrec) $ pprName v <> text "@"
                                                          <> pprPatI appPrec p
pprPatI _ WildP        = text "_"
pprPatI _ (RecP nm fs)
 = parens $     pprName nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> pprName s <+> equals <+> pprPat p) fs)
pprPatI _ (ListP ps) = brackets $ sep $ punctuate comma $ map pprPat ps

------------------------------
pprDec :: Dec -> Doc
pprDec (FunD f cs)   = vcat $ map (\c -> pprName f <+> pprClause c) cs
pprDec (ValD p r ds) = pprPat p <+> pprBody True r
                    $$ where_clause ds
pprDec (TySynD t xs rhs) = text "type" <+> pprName t <+> hsep (map pprName xs) 
                       <+> text "=" <+> pprType rhs
pprDec (DataD ctxt t xs cs decs)
    = text "data"
  <+> pprCxt ctxt
  <+> pprName t <+> hsep (map pprName xs)
  <+> sep (pref $ map pprCon cs)
   $$ if null decs
      then empty
      else nest nestDepth
         $ text "deriving"
       <+> parens (hsep $ punctuate comma $ map pprName decs)
    where pref :: [Doc] -> [Doc]
          pref [] = [char '='] -- Can't happen in H98
          pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds
pprDec (NewtypeD ctxt t xs c decs)
    = text "newtype"
  <+> pprCxt ctxt
  <+> pprName t <+> hsep (map pprName xs)
  <+> char '=' <+> pprCon c
   $$ if null decs
      then empty
      else nest nestDepth
         $ text "deriving"
       <+> parens (hsep $ punctuate comma $ map pprName decs)
pprDec (ClassD ctxt c xs ds) = text "class" <+> pprCxt ctxt
                           <+> pprName c <+> hsep (map pprName xs)
                            $$ where_clause ds
pprDec (InstanceD ctxt i ds) = text "instance" <+> pprCxt ctxt <+> pprType i
                            $$ where_clause ds
pprDec (SigD f t) = pprName f <+> text "::" <+> pprType t
pprDec (ForeignD f) = pprForeign f

------------------------------
pprForeign :: Foreign -> Doc
pprForeign (ImportF callconv safety impent as typ)
   = text "foreign import"
  <+> showtextl callconv
  <+> showtextl safety
  <+> text (show impent)
  <+> pprName as
  <+> text "::" <+> pprType typ
pprForeign (ExportF callconv        expent as typ)
    = text "foreign export"
  <+> showtextl callconv
  <+> text (show expent)
  <+> pprName as
  <+> text "::" <+> pprType typ

------------------------------
pprClause :: Clause -> Doc
pprClause (Clause ps rhs ds) = hsep (map pprPat ps) <+> pprBody True rhs
                            $$ where_clause ds

------------------------------
pprCon :: Con -> Doc
pprCon (NormalC c sts) = pprName c <+> hsep (map pprStrictType sts)
pprCon (RecC c vsts) = pprName c
                   <+> char '{'
                    <> hsep (punctuate comma $ map pprVarStrictType vsts)
                    <> char '}'
pprCon (InfixC st1 c st2) = pprStrictType st1
                        <+> pprName c
                        <+> pprStrictType st2

------------------------------
pprVarStrictType :: (Name, Strict, Type) -> Doc
pprVarStrictType (v, str, t) = pprName v <+> text "::" <+> pprStrictType (str, t)

------------------------------
pprStrictType :: (Strict, Type) -> Doc
pprStrictType (IsStrict, t) = char '!' <> pprType t
pprStrictType (NotStrict, t) = pprType t

------------------------------
pprParendType :: Type -> Doc
pprParendType (VarT v)   = pprName v
pprParendType (ConT c)   = pprName c
pprParendType (TupleT 0) = text "()"
pprParendType (TupleT n) = parens (hcat (replicate (n-1) comma))
pprParendType ArrowT     = parens (text "->")
pprParendType ListT      = text "[]"
pprParendType other      = parens (pprType other)

pprType :: Type -> Doc
pprType (ForallT tvars ctxt ty) = 
  text "forall" <+> hsep (map pprName tvars) <+> text "." <+> 
  ctxtDoc <+> pprType ty
  where
    ctxtDoc | null ctxt = empty
        | otherwise = parens (sep (punctuate comma (map pprType ctxt))) <+>
              text "=>"
pprType ty               = pprTyApp (split ty)

pprTyApp :: (Type, [Type]) -> Doc
pprTyApp (ArrowT, [arg1,arg2])
  = sep [pprType arg1 <+> text "->", pprType arg2]

pprTyApp (ListT, [arg]) = brackets (pprType arg)

pprTyApp (TupleT n, args)
 | length args == n
    = parens (sep (punctuate comma (map pprType args)))

pprTyApp (fun, args)
  = pprParendType fun <+> sep (map pprParendType args)

split :: Type -> (Type, [Type])    -- Split into function and args
split t = go t []
    where
      go (AppT t1 t2) args = go t1 (t2:args)
      go ty           args = (ty, args)

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt [t] = pprType t <+> text "=>"
pprCxt ts = parens (hsep $ punctuate comma $ map pprType ts) <+> text "=>"

------------------------------
pprRange :: Range -> Doc
pprRange = brackets . pprRangeI

pprRangeI :: Range -> Doc
pprRangeI (FromR e) = pprExp e <> text ".."
pprRangeI (FromThenR e1 e2) = pprExp e1 <> text ","
                           <> pprExp e2 <> text ".."
pprRangeI (FromToR e1 e2) = pprExp e1 <> text ".." <> pprExp e2
pprRangeI (FromThenToR e1 e2 e3) = pprExp e1 <> text ","
                                <> pprExp e2 <> text ".."
                                <> pprExp e3

------------------------------
where_clause :: [Dec] -> Doc
where_clause [] = empty
where_clause ds = text "where" <+> vcat (map pprDec ds)

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show


