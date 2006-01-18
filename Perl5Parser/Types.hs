module Perl5Parser.Types
    ( Env_lexical(..), Env(..), Prototypes, State(..)
    , NodeName(..), SpaceCommentT(..), BalancedOrNot(..), LiteralT, SubstituteT
    , QuoteT(..), QuoteLikeT(..), RegexpOptionT, RegexpT(..), NumberT(..), SeparatorT(..)
    , IdentT(..), TokenT(..), Node(..)
    , Perl5Parser
    ) where

import qualified Data.Map as Map

import Text.ParserCombinators.Parsec (CharParser)

data Env_lexical = Env_lexical
    { current_package :: String     
    }

data Env = Env 
    { env_lexical :: Env_lexical
    }

type Prototypes = Map.Map (String, String) String

data State = State 
    { prototypes :: Prototypes
    , env :: Env
    , next_line_is_here_doc :: Maybe String
    }

type Perl5Parser a = CharParser State a


newtype NodeName = NodeName String deriving Eq

instance Show NodeName where
    show (NodeName s) = s

data SpaceCommentT = 
    Whitespace String
  | Comment String
  | HereDocValue String
    deriving Show

data BalancedOrNot a = 
    NonBalanced Char
  | Balanced Char a
    deriving Show

-- | [TokenT] is the optional comment
type LiteralT = ([SpaceCommentT], BalancedOrNot Char)
type SubstituteT = ([SpaceCommentT], BalancedOrNot (Char, LiteralT))

data QuoteT =
    Double
  | Single
  | Literal     LiteralT
  | Interpolate LiteralT
    deriving Show

data QuoteLikeT =
    Glob
  | Readline
  | Backstick
  | Words LiteralT
  | Command LiteralT
    deriving Show

type RegexpOptionT = String

data RegexpT =
    Match (Maybe LiteralT) String RegexpOptionT
  | Qr LiteralT String RegexpOptionT
  | Substitute SubstituteT String String RegexpOptionT
  | Transliterate (String, SubstituteT) String String RegexpOptionT
    deriving Show

data NumberT = 
    NormalNumber
  | VersionNumber
    deriving Show

data SeparatorT = 
    Separator_Data
  | Separator_End
    deriving Show

data IdentT =
    LocalIdent
  | FqIdent { fq_canonical :: String, fq_verbatim :: String }
    deriving Show

data TokenT =
    Quote QuoteT String
  | QuoteLike QuoteLikeT String
  | Regexp RegexpT
  | Number NumberT String
  | Word String
  | Separator SeparatorT [String] String
  | HereDoc [SpaceCommentT] TokenT
  | PictureFormat String
  | Prototype String
  | Symbol String
  | Ident IdentT String
  | Operator String
  | Pod String
  | Label String [SpaceCommentT]
  | Attribute String (Maybe String)
    deriving Show

data Node = 
    Node(NodeName, [Node])
  | Call(NodeName, [Node])
  | Token (TokenT, [SpaceCommentT])
    deriving Show
