----------- THInternals -----------------
--
-- The stuff in this module is part of the implementation
-- of Template Haskell, and should not be exposed to the
-- programmer

module THInternals(

-----------------------------------------------------
--		Names and uniques 
-----------------------------------------------------

type ModName = PackedString	-- Module name
mkModName :: String -> ModName
mkModName s = packString s

modString :: ModName -> String
modString m = unpack m

type OccName = PackedString	-- Occurrence name; no explicit name space indication
mkOccName :: String -> OccName
mkOccName s = packString s

occString :: OccName -> String
occString occ = unpack occ

type Uniq = Int

-----------------------------------------------------
--		Bndrs and Vars
-----------------------------------------------------

data Bndr = BndrU OccName Uniq		-- A unique local name
	  | BndrS OccName		-- A particular string

data Var = Var Bndr
	 | Orig ModName	OccName		-- An original name
					-- These can only show up as occurrences,
					-- not binders

bndrVar :: Bndr -> Var
bndrVar b = Var b

baseName :: Bndr -> String
baseName (BndrG occ _) = occString occ
baseName (BndrU occ _) = occString occ
baseName (BndrS occ)   = occString occ

mkBndr :: String -> Bndr
mkBndr s = BndrS (mkOccName s)

newBndr :: String -> Q Bndr
newBndr s = Q (do { uniq <- newUniq
		  ; return (BndrU (mkOccName s) uniq) })

mkOrig :: String -> String -> Var	-- Used for 'x etc, but not available
mkOrig mod occ 				-- to the programmer
  = Orig (mkModName mod) (mkOccName occ)

instance Eq Bndr where
  b1 == b2 = cmpEq (b1 `compare` b2)

instance Ord Bndr where
  (BndrU _ u1) `compare` (BndrU _ u2) = u1 `compare` u2
  (BndrU _ u1) `compare` other	      = LT
  (BndrS s1)   `compare` (BndrS s2)   = s1 `compare` s2
  (BndrS s1)   `compare` other	      = GT

instance Show Bndr where
  show (BndrU occ u) = occString occ ++ "_" ++ show u
  show (BndrS occ)   = occString occ

instance Eq Var where
  v1 == v2 = cmpEq (v1 `compare` v2)

instance Ord Var where
  (Var b1) `compare` (Var b2) 	      = b1 `compare` b2
  (Var b1) `compare` other    	      = LT    
  (Orig m1 o1) `compare` (Orig m2 o2) = (m1 `compare` m2) `thenCmp` 
					(o1 `compare` o2)
  (Orig m1 o1) `compare` other        = GT

instance Show Var where
  show (Var b) = show b
  show (Orig m o) = modString m ++ "." ++ occString o

-----------------------------------------------------
--		Info
-----------------------------------------------------

data Info = Info	-- Fill in later

-----------------------------------------------------
--		The Quasi monad
-----------------------------------------------------

class Monad m => Quasi m where
  newUniq :: m Uniq

  failWith :: String -> m ()	-- Report an error and give up
  warnWith :: String -> m ()	-- Display a warning
  recover  :: m a -> m a -> m a	-- The first arg is the error handler
 
  reify :: Var -> m Info
  -- and more ...

newtype Q a = Q (forall m. Quasi m => m a)


-----------------------------------------------------
--		Internal helper functions
-----------------------------------------------------

cmpEq :: Ordering -> Bool
cmpEq EQ = True
cmpEq _  = False

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 o2 = o1
