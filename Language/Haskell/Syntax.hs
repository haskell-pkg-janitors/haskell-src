-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A suite of datatypes describing the abstract syntax of Haskell 98
-- <http://www.haskell.org/definition/> plus a few extensions:
--
--   * multi-parameter type classes
--
--   * parameters of type class assertions are unrestricted
--
-----------------------------------------------------------------------------

module Language.Haskell.Syntax (
    -- * Modules
    HsModule(..), HsExportSpec(..),
    HsImportDecl(..), HsImportSpec(..), HsAssoc(..),
    -- * Declarations
    HsDecl(..), HsConDecl(..), HsBangType(..),
    HsMatch(..), HsRhs(..), HsGuardedRhs(..),
    -- * Class Assertions and Contexts
    HsQualType(..), HsContext, HsAsst,
    -- * Types
    HsType(..),
    -- * Expressions
    HsExp(..), HsStmt(..), HsFieldUpdate(..),
    HsAlt(..), HsGuardedAlts(..), HsGuardedAlt(..),
    -- * Patterns
    HsPat(..), HsPatField(..),
    -- * Literals
    HsLiteral(..),
    -- * Variables, Constructors and Operators
    Module(..), HsQName(..), HsName(..), HsQOp(..), HsOp(..),

    -- * Builtin names

    -- ** Modules
    prelude_mod, main_mod,
    -- ** Constructors
    unit_con_name, tuple_con_name,
    unit_con, tuple_con,
    -- ** Special identifiers
    as_name, qualified_name, hiding_name, minus_name, pling_name,
    -- ** Type constructors
    unit_tycon_name, fun_tycon_name, list_tycon_name, tuple_tycon_name,
    unit_tycon, fun_tycon, list_tycon, tuple_tycon,

    -- * Source coordinates
    SrcLoc(..),
  ) where

-- | A position in the source.
data SrcLoc = SrcLoc {
		srcFilename :: String,
		srcLine :: Int,
		srcColumn :: Int
		}
  deriving (Eq,Ord,Show)

newtype Module = Module String
  deriving (Eq,Ord,Show)

-- |This type is used to represent qualified variables, and also
-- qualified constructors.
data HsQName
	= Qual Module HsName
	| UnQual HsName
  deriving (Eq,Ord)

instance Show HsQName where
   showsPrec _ (Qual (Module m) s) =
	showString m . showString "." . shows s
   showsPrec _ (UnQual s) = shows s

-- |This type is used to represent variables, and also constructors.
data HsName
	= HsIdent String	-- ^ /varid/ or /conid/.
	| HsSymbol String	-- ^ /varsym/ or /consym/
	| HsSpecial String
  deriving (Eq,Ord)

instance Show HsName where
   showsPrec _ (HsIdent s) = showString s
   showsPrec _ (HsSymbol s) = showString s
   showsPrec _ (HsSpecial s) = showString s

-- | Qualified operators.
data HsQOp
	= HsQVarOp HsQName
	| HsQConOp HsQName
  deriving (Eq,Ord)

instance Show HsQOp where
   showsPrec p (HsQVarOp n) = showsPrec p n
   showsPrec p (HsQConOp n) = showsPrec p n

-- | Operators.
data HsOp
	= HsVarOp HsName
	| HsConOp HsName
  deriving (Eq,Ord)

instance Show HsOp where
   showsPrec p (HsVarOp n) = showsPrec p n
   showsPrec p (HsConOp n) = showsPrec p n

data HsModule = HsModule SrcLoc Module (Maybe [HsExportSpec])
                         [HsImportDecl] [HsDecl]
  deriving Show

-- | Export specification.
data HsExportSpec
	 = HsEVar HsQName			-- ^ variable
	 | HsEAbs HsQName			-- ^ @T@:
			-- a class or datatype exported abstractly,
			-- or a type synonym.
	 | HsEThingAll HsQName			-- ^ @T(..)@:
			-- a class exported with all of its methods, or
			-- a datatype exported with all of its constructors.
	 | HsEThingWith HsQName [HsName]	-- ^ @T(C_1,...,C_n)@:
			-- a class exported with some of its methods, or
			-- a datatype exported with some of its constructors.
	 | HsEModuleContents Module		-- ^ @module M@:
			-- re-export a module.
  deriving (Eq,Show)

-- | Import declaration.
data HsImportDecl
	 = HsImportDecl SrcLoc Module Bool (Maybe Module)
	                (Maybe (Bool,[HsImportSpec]))
  deriving (Eq,Show)

-- | Import specification.
data HsImportSpec
	 = HsIVar HsName			-- ^ variable
	 | HsIAbs HsName			-- ^ @T@:
			-- the name of a class, datatype or type synonym.
	 | HsIThingAll HsName			-- ^ @T(..)@:
			-- a class imported with all of its methods, or
			-- a datatype imported with all of its constructors.
	 | HsIThingWith HsName [HsName]		-- ^ @T(C_1,...,C_n)@:
			-- a class imported with some of its methods, or
			-- a datatype imported with some of its constructors.
  deriving (Eq,Show)

data HsAssoc
	 = HsAssocNone
	 | HsAssocLeft
	 | HsAssocRight
  deriving (Eq,Show)

data HsDecl
	 = HsTypeDecl	 SrcLoc HsName [HsName] HsType
	 | HsDataDecl	 SrcLoc HsContext HsName [HsName] [HsConDecl] [HsQName]
	 | HsInfixDecl   SrcLoc HsAssoc Int [HsOp]
	 | HsNewTypeDecl SrcLoc HsContext HsName [HsName] HsConDecl [HsQName]
	 | HsClassDecl	 SrcLoc HsQualType [HsDecl]
	 | HsInstDecl	 SrcLoc HsQualType [HsDecl]
	 | HsDefaultDecl SrcLoc [HsType]
	 | HsTypeSig	 SrcLoc [HsName] HsQualType
	 | HsFunBind     [HsMatch]
	 | HsPatBind	 SrcLoc HsPat HsRhs {-where-} [HsDecl]
  deriving (Eq,Show)

data HsMatch
	 = HsMatch SrcLoc HsName [HsPat] HsRhs {-where-} [HsDecl]
  deriving (Eq,Show)

data HsConDecl
	 = HsConDecl SrcLoc HsName [HsBangType]
	 | HsRecDecl SrcLoc HsName [([HsName],HsBangType)]
  deriving (Eq,Show)

data HsBangType
	 = HsBangedTy   HsType
	 | HsUnBangedTy HsType
  deriving (Eq,Show)

data HsRhs
	 = HsUnGuardedRhs HsExp
	 | HsGuardedRhss  [HsGuardedRhs]
  deriving (Eq,Show)

data HsGuardedRhs
	 = HsGuardedRhs SrcLoc HsExp HsExp
  deriving (Eq,Show)

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data HsQualType
	 = HsQualType HsContext HsType
  deriving (Eq,Show)

data HsType
	 = HsTyFun   HsType HsType
	 | HsTyTuple [HsType]
	 | HsTyApp   HsType HsType
	 | HsTyVar   HsName
	 | HsTyCon   HsQName
  deriving (Eq,Show)

type HsContext = [HsAsst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
type HsAsst    = (HsQName,[HsType])

-- | /literal/
data HsLiteral
	= HsInt		Integer
	| HsChar	Char
	| HsString	String
	| HsFrac	Rational
	| HsCharPrim	Char		-- ^ GHC unboxed character literal
	| HsStringPrim	String		-- ^ GHC unboxed string literal
	| HsIntPrim	Integer		-- ^ GHC unboxed integer literal
	| HsFloatPrim	Rational	-- ^ GHC unboxed float literal
	| HsDoublePrim	Rational	-- ^ GHC unboxed double literal
  deriving (Eq, Show)

-- | Haskell expressions.
-- Because it is difficult for parsers to distinguish patterns from
-- expressions, they typically parse them in the same way and then check
-- that they have the appropriate form.  Hence the expression type
-- includes some forms that are found only in patterns.  After these
-- checks, these constructors should not be used.

data HsExp
	= HsVar HsQName
	| HsCon HsQName
	| HsLit HsLiteral
	| HsInfixApp HsExp HsQOp HsExp
	| HsApp HsExp HsExp
	| HsNegApp HsExp
	| HsLambda [HsPat] HsExp
	| HsLet [HsDecl] HsExp
	| HsIf HsExp HsExp HsExp
	| HsCase HsExp [HsAlt]
	| HsDo [HsStmt]			-- ^ Do expression:
					-- The last statement in the list
					-- should be an expression.
	| HsTuple [HsExp]
	| HsList [HsExp]
	| HsParen HsExp
	| HsLeftSection HsExp HsQOp
	| HsRightSection HsQOp HsExp
	| HsRecConstr HsQName [HsFieldUpdate]
	| HsRecUpdate HsExp [HsFieldUpdate]
	| HsEnumFrom HsExp
	| HsEnumFromTo HsExp HsExp
	| HsEnumFromThen HsExp HsExp
	| HsEnumFromThenTo HsExp HsExp HsExp
	| HsListComp HsExp [HsStmt]
	| HsExpTypeSig SrcLoc HsExp HsQualType
	| HsAsPat HsName HsExp		-- ^ patterns only
	| HsWildCard			-- ^ patterns only
	| HsIrrPat HsExp		-- ^ patterns only
 deriving (Eq,Show)

data HsPat
	= HsPVar HsName
	| HsPLit HsLiteral
	| HsPNeg HsPat
	| HsPInfixApp HsPat HsQName HsPat
	| HsPApp HsQName [HsPat]
	| HsPTuple [HsPat]
	| HsPList [HsPat]
	| HsPParen HsPat
	| HsPRec HsQName [HsPatField]
	| HsPAsPat HsName HsPat
	| HsPWildCard
	| HsPIrrPat HsPat
 deriving (Eq,Show)

data HsPatField
	= HsPFieldPat HsQName HsPat
 deriving (Eq,Show)

-- | This type represents both /stmt/ in a @do@-expression,
--   and /qual/ in a list comprehension.
data HsStmt
	= HsGenerator HsPat HsExp
	| HsQualifier HsExp
	| HsLetStmt [HsDecl]
 deriving (Eq,Show)

-- | An /fbind/ in a labeled construction or update.
data HsFieldUpdate
	= HsFieldUpdate HsQName HsExp
  deriving (Eq,Show)

data HsAlt
	= HsAlt SrcLoc HsPat HsGuardedAlts [HsDecl]
  deriving (Eq,Show)

data HsGuardedAlts
	= HsUnGuardedAlt HsExp
	| HsGuardedAlts  [HsGuardedAlt]
  deriving (Eq,Show)

data HsGuardedAlt
	= HsGuardedAlt SrcLoc HsExp HsExp
  deriving (Eq,Show)

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: Module
prelude_mod	      = Module "Prelude"
main_mod	      = Module "Main"

unit_con_name :: HsQName
unit_con_name	      = Qual prelude_mod (HsSpecial "()")

tuple_con_name :: Int -> HsQName
tuple_con_name i      = Qual prelude_mod (HsSpecial ("("++replicate i ','++")"))

unit_con :: HsExp
unit_con	      = HsCon unit_con_name

tuple_con :: Int -> HsExp
tuple_con i	      = HsCon (tuple_con_name i)

as_name, qualified_name, hiding_name, minus_name, pling_name :: HsName
as_name	              = HsIdent "as"
qualified_name        = HsIdent "qualified"
hiding_name	      = HsIdent "hiding"
minus_name	      = HsSymbol "-"
pling_name	      = HsSymbol "!"

unit_tycon_name, fun_tycon_name, list_tycon_name :: HsQName
unit_tycon_name       = unit_con_name
fun_tycon_name        = Qual prelude_mod (HsSymbol "->")
list_tycon_name       = Qual prelude_mod (HsIdent "[]")

tuple_tycon_name :: Int -> HsQName
tuple_tycon_name i    = tuple_con_name i

unit_tycon, fun_tycon, list_tycon :: HsType
unit_tycon	      = HsTyCon unit_tycon_name
fun_tycon	      = HsTyCon fun_tycon_name
list_tycon	      = HsTyCon list_tycon_name

tuple_tycon :: Int -> HsType
tuple_tycon i	      = HsTyCon (tuple_tycon_name i)
