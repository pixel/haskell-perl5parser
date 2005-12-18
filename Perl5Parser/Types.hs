module Perl5Parser.Types
    ( State(..)
    , NodeName(..), CommentO, BalancedOrNot(..), LiteralT, SubstituteT
    , QuoteT(..), QuoteLikeT(..), RegexpOptionT, RegexpT(..), NumberT(..), TokenT(..), Node(..)
    , Perl5Parser
    ) where

import qualified Data.Map as Map

import Text.ParserCombinators.Parsec (CharParser)

data State = State 
    { prototypes :: Map.Map String String
    , next_line_is_here_doc :: Maybe String
    }

type Perl5Parser a = CharParser State a


newtype NodeName = NodeName String deriving Eq

instance Show NodeName where
    show (NodeName s) = s

type CommentO = Maybe String

data BalancedOrNot a = 
    NonBalanced Char
  | Balanced Char a
    deriving Show

type LiteralT = ([TokenT], BalancedOrNot Char)
type SubstituteT = ([TokenT], BalancedOrNot (Char, LiteralT))

data QuoteT =
    Double
  | Single
  | Literal     LiteralT
  | Interpolate LiteralT
    deriving Show

data QuoteLikeT =
    Glob
  | Readline
  | Words LiteralT
  | Qr LiteralT
    deriving Show

type RegexpOptionT = String

data RegexpT =
    Match LiteralT String RegexpOptionT
  | Substitute SubstituteT String String RegexpOptionT
  | Transliterate (String, SubstituteT) String String RegexpOptionT
    deriving Show

data NumberT = 
    NormalNumber
  | VersionNumber
    deriving Show

data TokenT =
    Quote QuoteT String
  | QuoteLike QuoteLikeT String
  | Regexp RegexpT
  | Number NumberT String
  | Word String
  | Whitespace String
  | Comment String
  | HereDoc [TokenT] TokenT
  | HereDocValue String
  | PictureFormat String
  | Prototype String
  | Symbol String
  | Operator String
  | Pod String
  | Label String [String]
    deriving Show

data Node = 
    Node(NodeName, [Node])
  | Call(NodeName, [Node])
  | Tokens [TokenT]
    deriving Show
