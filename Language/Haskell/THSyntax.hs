module Language.Haskell.THSyntax where

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe( unsafePerformIO )

-------------------------------------------------------
-- The Q monad as IO

type Q a = IO a

returnQ :: a -> Q a
returnQ = return

bindQ :: Q a -> (a -> Q b) -> Q b
bindQ = (>>=)

counter :: IORef Int
counter = unsafePerformIO(newIORef 0)

gensym :: String -> Q String
gensym s = do { n <- readIORef counter
              ; writeIORef counter (n+1)
              ; return(s++"'"++(show n)) }

class Lift t where
  lift :: t -> Exp
  
instance Lift Int where
  lift = Lit . Int
instance Lift Char where
  lift = Lit . Char
------------------------------------------------------

data Lit = Int Int | Char Char | CrossStage String 

data Pat 
  = Plit Lit                      -- { 5 or 'c' }
  | Pvar String                   -- { x }
  | Ptup [Pat]                    -- { (p1,p2) }
  | Pcon String [Pat]             -- data T1 = C1 t1 t2; {C1 p1 p1} = e 
  | Ptilde Pat                    -- { ~p }
  | Paspat String Pat             -- { x @ p }
  | Pwild                         -- { _ }



type Match p e d  = ( p ,Body e,[d])   -- case e of { pat -> body where decs } 
type Clause p e d = ([p],Body e,[d])   -- f { p1 p2 = body where decs }
 
data Exp 
  = Var String                           -- { x }
  | Con String                           -- data T1 = C1 t1 t2; p = {C1} e1 e2  
  | Lit Lit                              -- { 5 or 'c'}
  | App Exp Exp                          -- { f x }
  | Infix (Maybe Exp) String (Maybe Exp) -- {x + y} or {(x+)} or {(+ x)} or {(+)}
  | Lam [Pat] Exp                        -- { \ p1 p2 -> e }
  | Tup [Exp]                            -- { (e1,e2) }  
  | Cond Exp Exp Exp                     -- { if e1 then e2 else e3 }
  | Let [Dec] Exp                        -- { let x=e1;   y=e2 in e3 }
  | Case Exp [Match Pat Exp Dec]         -- { case e of m1; m2 }
  | Do [Stmt Pat Exp Dec]                -- { do { p <- e1; e2 }  }
  | Comp [Stmt Pat Exp Dec]              -- { [ (x,y) | x <- xs, y <- ys ] }
  | ArithSeq (DotDot Exp)                -- { [ 1 ,2 .. 10 ] }
  | ListExp [ Exp ]                      -- { [1,2,3] }
  | Br Exp
  | Esc Exp

--left out things implicit parameters, sections

data Body e
  = Guarded [(e,e)]       -- f p { | e1 = e2 | e3 = e4 } where ds
  | Normal e              -- f p = { e } where ds

data Stmt p e d
  = BindSt p e
  | LetSt [ d ]
  | NoBindSt e
  | ParSt [[Stmt p e d]]

data DotDot e = From e | FromThen e e | FromTo e e | FromThenTo e e e 
  
data Dec 
  = Fun String [Clause Pat Exp Dec] -- { f p1 p2 = b where decs }
  | Val Pat (Body Exp) [Dec]        -- { p = b where decs }
  | Data String [String] 
         [Constr] [String]          -- { data T x = A x | B (T x) deriving (Z,W)}
  | Class [Typ] Typ [Dec]           -- { class Eq a => Eq [a] where ds }
  | Instance [Typ] Typ [Dec]        -- { instance Show w => Show [w] where ds }
  | Proto String Typ                -- { length :: [a] -> Int }

data Constr = Constr String [Typ]

data Program = Program [ Dec ] 

data Tag = Tuple Int | Arrow | List | Name String deriving Eq

data Typ = Tvar String           -- a
         | Tcon Tag              -- T or [] or (->) or (,,) etc
         | Tapp Typ Typ          -- T a b
 
---------------------------------------------------
-- Combinator based types


type Expr =  Q Exp
type Patt = Pat
type Decl = Q Dec
type Type = Typ

type Mat  = Match Pat Exp Dec
type Mtch = Match Patt Expr Decl

type Cls  = Clause Pat Exp Dec
type Clse = Clause Patt Expr Decl

type Stm  = Stmt Pat Exp Dec
type Statement = Stmt Patt Expr Decl

--runE :: Expr -> Exp
--runE x = runQ 0 x

--runP :: Pattern -> Pat
runP x = x

--runD :: Decl -> Dec
--runD d = runQ 0 d



-------------------- Lowercase pattern syntax functions ---

intL = Int
charL = Char

plit = Plit
pvar = Pvar
ptup = Ptup
pcon = Pcon
ptilde = Ptilde
paspat = Paspat
pwild = Pwild

alpha env s = case lookup s env of
               Just x -> return(Var x)
               Nothing -> return(Var s)

--rho :: Qenv -> String -> Expr
rho e s = 
  case lookup s e of 
    Just z -> return(Var z)
    Nothing -> return(Var s)
    
    

--------------------------------------------------------------------------------
-- Combinators for the "helper" datatypes and type synonymns

--stmtC :: Stmt Pattern Expr Decl -> Q(Stmt Pat Exp Dec)
stmtC (BindSt p e) = do { e1 <- e; return(BindSt p e1) }
stmtC (LetSt ds) = do { ds2 <- sequence ds; return(LetSt ds2) }
stmtC (NoBindSt e) = do { e1 <- e; return(NoBindSt e1) }
stmtC (ParSt zs) = fail "No parallel comprehensions yet"

stmtsC ss = sequence(map stmtC ss)

--bodyC :: Body Expr -> Q(Body Exp)
bodyC (Normal e) = do { e1 <- e; return(Normal e1) }
bodyC (Guarded ps) = do { ps1 <- mapM f ps; return(Guarded ps1) }
  where f (g,e) = do { g1 <- g; e1 <- e; return(g1,e1) }

--matchC :: Match Pattern Expr Decl -> Q(Match Pat Exp Dec)  
matchC (p,b,ds) = do { b1 <- bodyC b; ds1 <- sequence ds; return(p,b1,ds1)}
clauseC x = matchC x

dotdotC (From x) = do { a <- x; return(From a)}  
dotdotC (FromThen x y) = do { a <- x; b <- y; return(FromThen a b)}  
dotdotC (FromTo x y) = do { a <- x; b <- y; return(FromTo a b)}  
dotdotC (FromThenTo x y z) = do { a <- x; b <- y; c <- z; return(FromThenTo a b c)}  


---------------------------------------------------------------------------
-- Monadic combinators for constructing Expr

global :: String -> Expr
global s = return(Var s)


var :: String -> Expr
var s = return(Var s)

con :: String -> Expr
con s =  return(Con s)

lit :: Lit -> Expr
lit c = return(Lit c)

app :: Expr -> Expr -> Expr
app x y = do { a <- x; b <- y; return(App a b)}


infixE :: Maybe Expr -> String -> Maybe Expr -> Expr
infixE (Just x) s (Just y) = do { a <- x; b <- y; return(Infix (Just a) s (Just b))}
infixE Nothing  s (Just y) = do { b <- y; return(Infix Nothing s (Just b))}
infixE (Just x) s Nothing  = do { a <- x; return(Infix (Just a) s Nothing)}
infixE Nothing  s Nothing  = return(Infix Nothing s Nothing)

infixApp x y z = infixE (Just x) y (Just z)
sectionL x y = infixE (Just x) y Nothing
sectionR x y = infixE Nothing x (Just y)

lam :: [Patt] -> Expr -> Expr
lam ps e = do { e2 <- e
              ; return(Lam ps e2) }

lam1 p e = lam [p] e              

tup :: [Expr] -> Expr
tup es = do { es1 <- sequence es; return(Tup es1)}

cond :: Expr -> Expr -> Expr -> Expr
cond x y z =  do { a <- x; b <- y; c <- z; return(Cond a b c)}

letE :: [Decl] -> Expr -> Expr
letE ds e = do { ds2 <- sequence ds; e2 <- e; return(Let ds2 e2) }

caseE :: Expr -> [Match Patt Expr Decl] -> Expr
caseE e ms = do { e1 <- e; ms1 <- mapM matchC ms; return(Case e1 ms1) } 

doE :: [Stmt Patt Expr Decl] -> Expr
doE ss = do { ss1 <- stmtsC ss; return(Do ss1) } 

comp :: [Stmt Patt Expr Decl] -> Expr
comp ss = do { ss1 <- stmtsC ss; return(Comp ss1) } 

arithSeq :: (DotDot Expr) -> Expr
arithSeq xs = do { ys <- dotdotC xs; return(ArithSeq ys) }

listExp :: [ Expr ] -> Expr
listExp es = do { es1 <- sequence es; return(ListExp es1)}

br :: Expr -> Expr
br e = do { x <- e; return(Br x) }

esc :: Expr -> Expr
esc e = do { x <- e; return(Esc x) }


string s = listExp(map (lit . Char) s)

-----------------------------------------------------
-- Combinators for helper types Body, Stmt, DotDot

guarded = Guarded
normal  = Normal

bindSt   = BindSt
letSt    = LetSt
noBindSt = NoBindSt
parSt    = ParSt

from       = From
fromThen   = fromThen
fromTo     = fromTo
fromThenTo = FromThenTo


--------------------------------------------------------------------------------
-- Combinators for constructing Decl's

--val :: Pattern -> (Body Expr) -> [Decl] -> Decl
val p b ds = 
  do { ds1 <- sequence ds
     ; b1 <- bodyC b
     ; return(Val p b1 ds1)
     }

--fun :: Name -> [Clause Pattern Expr Decl] -> Decl     
fun nm cs = 
 do { cs1 <- mapM clauseC cs
    ; return(Fun nm cs1)
    }

--------------------------------------------------------------
-- useful helper functions

combine pairs = foldr f ([],[]) pairs
  where f (env,p) (es,ps) = (env++es,p:ps)

rename (Plit c) = return([],Plit c)
rename (Pvar s) = do { s1 <- gensym s; return([(s,s1)],Pvar s1) }
rename (Ptup ps) = do { pairs <- mapM rename ps; g(combine pairs) }
   where g (es,ps) = return (es,Ptup ps)
rename (Pcon nm ps) = do { pairs <- mapM rename ps; g(combine pairs) }
   where g (es,ps) = return (es,Pcon nm ps)
rename (Ptilde p) = do { (env,p2) <- rename p; return(env,Ptilde p2) }   
rename (Paspat s p) = 
   do { s1 <- gensym s; (env,p2) <- rename p; return((s,s1):env,Paspat s1 p2) }
rename Pwild = return([],Pwild)

genpat p = do { (env,p2) <- rename p; return(alpha env,p2) }

--genPE s n = [ (pvar x, var x) | i <- [1..n], let x = s ++ show i ]

genPE s n = let ns = [ s++(show i) | i <- [1..n]]
            in (map pvar ns,map var ns)

apps :: [Expr] -> Expr
apps [x] = x
apps (x:y:zs) = apps ( (app x y) : zs )

simpleM p e = (p,Normal e,[])    
