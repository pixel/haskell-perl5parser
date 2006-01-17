module Perl5Parser.Document
    ( prog, initial_state
    ) where

import Perl5Parser.Types
import Perl5Parser.Prototype
import Perl5Parser.ParserHelper
import Perl5Parser.Lines

initial_state = State initial_prototypes Nothing
    where initial_prototypes = Perl5Parser.Prototype.builtin_prototypes

prog :: Perl5Parser Node

prog = newNode "prog" $ do 
         l1 <- toNodes spaces_comments_token
         l2 <- lines_
         eof <?> ""
         return$ l1 ++ l2
