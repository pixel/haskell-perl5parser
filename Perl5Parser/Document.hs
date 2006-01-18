module Perl5Parser.Document
    ( prog, initial_state
    ) where

import Perl5Parser.Types
import Perl5Parser.Prototype
import Perl5Parser.ParserHelper
import Perl5Parser.Lines

import qualified Data.Map as Map

initial_state = State { prototypes = initial_prototypes, env = initial_env, next_line_is_here_doc = Nothing }
    where initial_prototypes = Map.fromList $ map (\(f, proto) -> (("CORE", f), proto)) Perl5Parser.Prototype.builtin_prototypes
          initial_env = Env { env_lexical = Env_lexical { current_package = "main" } }


prog :: Perl5Parser Node

prog = newNode "prog" $ do 
         l1 <- onList (\l -> Token(Pod "", l)) spaces_comments
         l2 <- lines_
         eof <?> ""
         return$ l1 ++ l2
