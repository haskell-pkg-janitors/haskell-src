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



type Match p e d  = ( p ,RightHandSide e,[d])   -- case e of { pat -> body where decs } 
type Clause p e d = ([p],RightHandSide e,[d])   -- f { p1 p2 = body where decs }
 
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
  | Do [Statement Pat Exp Dec]           -- { do { p <- e1; e2 }  }
  | Comp [Statement Pat Exp Dec]         -- { [ (x,y) | x <- xs, y <- ys ] }
  | ArithSeq (DotDot Exp)                -- { [ 1 ,2 .. 10 ] }
  | ListExp [ Exp ]                      -- { [1,2,3] }
  | Br Exp
  | Esc Exp

--left out things implicit parameters, sections

data RightHandSide e
  = Guarded [(e,e)]       -- f p { | e1 = e2 | e3 = e4 } where ds
  | Normal e              -- f p = { e } where ds

data Statement p e d
  = BindSt p e
  | LetSt [ d ]
  | NoBindSt e
  | ParSt [[Statement p e d]]

data DotDot e = From e | FromThen e e | FromTo e e | FromThenTo e e e 
  
data Dec 
  = Fun String [Clause Pat Exp Dec]      -- { f p1 p2 = b where decs }
  | Val Pat (RightHandSide Exp) [Dec]    -- { p = b where decs }
  | Data String [String] 
         [Constr] [String]               -- { data T x = A x | B (T x) deriving (Z,W)}
  | Class Context String [String] [Dec]  -- { class Eq a => Eq [a] where ds }
  | Instance Context Typ [Dec]   	 -- { instance Show w => Show [w] where ds }
  | Proto String Typ                     -- { length :: [a] -> Int }

type Context = [Typ]	-- (Eq a, Ord b)

data Constr = Constr String [Typ]

data Program = Program [ Dec ] 

data Tag = Tuple Int | Arrow | List | Name String deriving Eq

data Typ = Tvar String           -- a
         | Tcon Tag              -- T or [] or (->) or (,,) etc
         | Tapp Typ Typ          -- T a b
 
---------------------------------------------------
-- Combinator based types

type Expr = Q Exp
type Decl = Q Dec
type Type = Typ		-- No need for Q here
type Patt = Pat		-- Ditto

type Mat  = Match Pat Exp Dec
type Mtch = Q Mat

type Cls  = Clause Pat Exp Dec
type Clse = Q Cls

type Rhs  = RightHandSide Exp
type Rihs = Q Rhs

type Stm  = Statement Pat Exp Dec
type Stmt = Q Stm

type DDt  = DotDot Exp
type DDot = Q DDt

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
-- 	Stmt

bindSt :: Patt -> Expr -> Stmt
bindSt p e = do { e1 <- e; return (BindSt p e1) }

letSt :: [Decl] -> Stmt
letSt ds = do { ds1 <- sequence ds; return (LetSt ds1) }

noBindSt :: Expr -> Stmt
noBindSt e = do { e1 <- e; return (NoBindSt e1) }

parSt :: [[Stmt]] -> Stmt
parSt zs = fail "No parallel comprehensions yet"

--------------------------------------------------------------------------------
-- 	RightHandSide

normal :: Expr -> Rihs
normal e = do { e1 <- e; return (Normal e1) }

guarded :: [(Expr,Expr)] -> Rihs
guarded gs = do { gs1 <- mapM f gs; return (Guarded gs1) }
	   where
		f (g,e) = do { g1 <- g; e1 <- e; return (g1,e1) }

--------------------------------------------------------------------------------
-- 	Match and Clause

match :: Patt -> Rihs -> [Decl] -> Mtch
match p rhs ds = do { rhs' <- rhs; ds' <- sequence ds; return (p, rhs', ds') }

clause :: [Patt] -> Rihs -> [Decl] -> Clse
clause ps r ds = do { r' <- r; ds' <- sequence ds; return (ps, r', ds') }


---------------------------------------------------------------------------
-- 	Expr

global :: String -> Expr
global s = return(Var s)

var :: String -> Expr
var s = return(Var s)

con :: String -> Expr
con s =  return(Con s)

lit :: Lit -> Expr
lit c = return (Lit c)

app :: Expr -> Expr -> Expr
app x y = do { a <- x; b <- y; return (App a b)}


infixE :: Maybe Expr -> String -> Maybe Expr -> Expr
infixE (Just x) s (Just y) = do { a <- x; b <- y; return(Infix (Just a) s (Just b))}
infixE Nothing  s (Just y) = do { b <- y; return(Infix Nothing s (Just b))}
infixE (Just x) s Nothing  = do { a <- x; return(Infix (Just a) s Nothing)}
infixE Nothing  s Nothing  = return(Infix Nothing s Nothing)

infixApp x y z = infixE (Just x) y (Just z)
sectionL x y = infixE (Just x) y Nothing
sectionR x y = infixE Nothing x (Just y)

from :: Expr -> Expr
from x = do { a <- x; return (ArithSeq (From a)) }  

fromThen :: Expr -> Expr -> Expr
fromThen x y = do { a <- x; b <- y; return (ArithSeq (FromThen a b)) }  

fromTo :: Expr -> Expr -> Expr
fromTo x y = do { a <- x; b <- y; return (ArithSeq (FromTo a b)) }  

fromThenTo :: Expr -> Expr -> Expr -> Expr
fromThenTo x y z = do { a <- x; b <- y; c <- z; return (ArithSeq (FromThenTo a b c)) }  

lam :: [Patt] -> Expr -> Expr
lam ps e = do { e2 <- e ; return (Lam ps e2) }

lam1 :: Patt -> Expr -> Expr	-- Single-arg lambda
lam1 p e = lam [p] e              

tup :: [Expr] -> Expr
tup es = do { es1 <- sequence es; return (Tup es1)}

cond :: Expr -> Expr -> Expr -> Expr
cond x y z =  do { a <- x; b <- y; c <- z; return (Cond a b c)}

letE :: [Decl] -> Expr -> Expr
letE ds e = do { ds2 <- sequence ds; e2 <- e; return (Let ds2 e2) }

caseE :: Expr -> [Mtch] -> Expr
caseE e ms = do { e1 <- e; ms1 <- sequence ms; return (Case e1 ms1) } 

doE :: [Stmt] -> Expr
doE ss = do { ss1 <- sequence ss; return (Do ss1) } 

comp :: [Stmt] -> Expr
comp ss = do { ss1 <- sequence ss; return (Comp ss1) } 

listExp :: [Expr] -> Expr
listExp es = do { es1 <- sequence es; return (ListExp es1)}

br :: Expr -> Expr
br e = do { x <- e; return (Br x) }

esc :: Expr -> Expr
esc e = do { x <- e; return (Esc x) }

string s = listExp(map (lit . Char) s)

--------------------------------------------------------------------------------
-- 	Decl

val :: Patt -> Rihs -> [Decl] -> Decl
val p b ds = 
  do { ds1 <- sequence ds
     ; b1 <- b
     ; return(Val p b1 ds1)
     }

fun :: String -> [Clse] -> Decl     
fun nm cs = 
 do { cs1 <- sequence cs
    ; return (Fun nm cs1)
    }

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

genpat p = do { (env,p2) <- rename p; return(alpha env,p2) }

--genPE s n = [ (pvar x, var x) | i <- [1..n], let x = s ++ show i ]

genPE s n = let ns = [ s++(show i) | i <- [1..n]]
            in (map pvar ns,map var ns)

apps :: [Expr] -> Expr
apps [x] = x
apps (x:y:zs) = apps ( (app x y) : zs )

simpleM p e = (p,Normal e,[])    
