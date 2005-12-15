module Perl5Parser.Types
    ( State(..)
    , NodeName(..), Node(..)
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

data Node = Node(NodeName, [Node]) | Token String



instance Show NodeName where
    show (NodeName s) = s

instance Show Node where
    show (Node(NodeName "Token::Number", Token n : _)) = n
    show (Node(s, l)) = show s ++ "[ " ++ unwords (map show l) ++ " ]"
    show (Token s) = show s
