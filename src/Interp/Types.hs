{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module Interp.Types where

import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Data.Set (Set)

import GHC.Generics (Generic)
import Text.Megaparsec (Parsec, SourcePos)

data P'
    = PVar Text
    | PWild
    | PInt Integer
    | PBool Bool
    | PStr Text
    | PNil
    | PCons P' P'
    | PTuple [P']
    | PCtor Text [P']
    deriving (Show, Eq)

data E'
    = ILit Integer
    | BLit Bool
    | SLit Text
    | Var Text
    | ListLit [E']
    | TupleLit [E']
    | Let Text E' E'
    | If E' E' E'
    | BOpr Opr E' E'
    | Lambda Text E'
    | App E' E'
    | Match E' [(P', E')]
    | AnnT E' T'
    | At Span E'
    deriving (Show, Eq)

data Opr    = OArith LitOpr | OCmp CmpOpr deriving (Show, Eq)
data LitOpr = OpAdd | OpMul | OpSub | OpDiv | OpPow deriving (Show, Eq)
data CmpOpr = OpEq | OpNe | OpLt | OpGt | OpLe | OpGe deriving (Show, Eq)
data Assoc  = AssocL | AssocR | AssocN deriving (Show, Eq)
data Span   = Span SourcePos SourcePos deriving (Show, Eq)

type Lev = Int

data Fixity = Fixity { fixLev :: Lev, fixAssoc :: Assoc } deriving (Show, Eq)
type FixTab = Map Text Fixity

data FieldInfo = FieldInfo
    { fiCtor  :: Text
    , fiData  :: Text
    , fiIdx   :: Int
    , fiArity :: Int
    , fiSolo  :: Bool
    } deriving (Show, Eq)

type FieldTab = Map Text FieldInfo

data Tabs = Tabs
    { tbFix :: FixTab
    , tbFld :: FieldTab
    } deriving (Show, Eq)

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/^=<>!%&~?" :: String)

isOpName :: Text -> Bool
isOpName n = not (T.null n) && T.all isOpChar n

data V'
    = VInt Integer
    | VBool Bool
    | VStr Text
    | VList [V']
    | VTuple [V']
    | VClosure Text E' Env
    | VPrim Text Int [V']
    | VCtor Text Int [V']
    deriving (Show, Eq)

data EvalError
    = UnboundVariable Text
    | OprArgIsNotNum Opr V'
    | IsNotFunction V'
    | DividedByZero Integer
    | NegativeExponent Integer
    | IfNeedsBool V'
    | RecursionLimited Depth
    | RecursiveVarDef Text
    | NonExhaustiveMatch V'
    | ConsNeedsList V'
    | OprArgIsNotComparable V' V'
    | NotACodepoint Integer
    | PrimArgMismatch Text [V']
    | NoSuchField Text V'
    deriving (Show, Eq)

newtype Depth = Depth Int deriving (Show, Eq, Ord, Generic)
data Located e = Located (Maybe Span) e deriving (Show, Eq)

data T' 
    = TInt | TBool | TString 
    | TVar TypeVar 
    | TList T'
    | TTuple [T']
    | TFunc T' T'
    | TCon Text [T'] 
    deriving (Show, Eq)

data StmtTy 
    = TyDef Text S'
    | TyDefFailed (Located TypeError)
    | TyDefSkipped [Text]
    | TyExpr (Either (Located TypeError) T') 
    deriving (Show, Eq)

type TypeVar = Int
type Counter = Int

data TypeError
    = UnboundVar Text
    | TypeMismatch T' T'
    | DuplicateDef Text
    | DuplicatePatVar Text
    | NonExhaustivePat T'
    | UnknownCtor Text
    | CtorArityMisMatch Text Int Int
    | DuplicateCtor Text
    | DuplicateData Text
    | UnknownTypeCtor Text
    | TypeCtorArityMisMatch Text Int Int
    | OccurCheck
    | CalleeNote Text S' TypeError
    | WithNote Text TypeError
    | CalleeNotChecked Text
    deriving (Show, Eq)

type Env  = Map Text V'
type TEnv = Map Text S'
type TSub = Map TypeVar T'

type SynTable = Map Text ([Text], T')
type Parser a = Parsec Void Text a

data S' = Forall (Set TypeVar) T' deriving (Show, Eq)
data Decl = Def Text [Text] E' deriving (Show, Eq)

data Statement 
    = StmtDef Decl 
    | StmtExpr E' 
    | StmtData DataDecl
    | StmtType Text [Text] T'
    | StmtInfix Text Fixity
    deriving (Show, Eq)

type Program = [Statement]
type Binding = (Text, E')
type BatchName = Text

data DataDecl = DataDecl
    { dName   :: Text
    , dParams :: [Text]
    , dCtors  :: [(Text, [T'])]
    , dFields :: Map Text (Text, Int)
    } deriving (Show, Eq)

data Session = Session
    { sTEnv    :: TEnv
    , sEnv     :: Env
    , sNext    :: Counter
    , sBatch   :: BatchName
    , sDEnv    :: DEnv
    , sAliases :: Map Text FilePath
    , sSyns    :: SynTable
    , sFixities :: FixTab
    , sFields  :: FieldTab
    } deriving (Show, Eq)

data CtorInfo = CtorInfo
    { ciTvs  :: Set TypeVar
    , ciRes  :: T'
    , ciArgs :: [T']
    } deriving (Show, Eq)

data DEnv = DEnv
    { denvCtors :: Map Text CtorInfo
    , denvDatas :: Map Text [Text]
    } deriving (Show, Eq)

data Tops = Tops 
    { tVals  :: Set Text
    , tTypes :: Set Text
    , tCtors :: Set Text
    }

opTable :: [(Text, Opr)]
opTable = 
    [ ("+", OArith OpAdd)
    , ("-", OArith OpSub)
    , ("*", OArith OpMul)
    , ("/", OArith OpDiv)
    , ("^", OArith OpPow)
    , ("==", OCmp OpEq)
    , ("!=", OCmp OpNe)
    , ("<=", OCmp OpLe)
    , (">=", OCmp OpGe)
    , ("<", OCmp OpLt)
    , (">", OCmp OpGt)
    ]

instance Num Depth where
    fromInteger :: Integer -> Depth
    fromInteger = Depth . fromInteger

    (+) :: Depth -> Depth -> Depth
    (+) (Depth a) (Depth b) = Depth (a + b)

    (-) :: Depth -> Depth -> Depth
    (-) (Depth a) (Depth b) = Depth (a - b)

    (*) :: Depth -> Depth -> Depth
    (*) (Depth a) (Depth b) = Depth (a * b)

    abs :: Depth -> Depth
    abs (Depth a) = Depth (abs a)

    signum :: Depth -> Depth
    signum (Depth a) = Depth (signum a)