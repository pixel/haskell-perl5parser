{-# OPTIONS_GHC -fglasgow-exts #-}

import Prelude hiding (lines)
import Data.Maybe (fromMaybe, isNothing, fromJust)

import Perl5Parser.Types
import Perl5Parser.Prototype
import Perl5Parser.ParserHelper
import Perl5Parser.Lines


--------------------------------------------------------------------------------
prog :: Perl5Parser Node

prog = newNode "prog" $ do 
         l1 <- toTokens spaces_comments
         l2 <- lines
         eof <?> ""
         return$ l1 ++ l2

--------------------------------------------------------------------------------

test_file = "/tmp/t.pl"
--test_file = "/home/pixel/cooker/soft/perl-MDK-Common/MDK/Common/File.pm"

test file = readFile file >>= parseTest prog initial_state

main = test_lexemes >> test test_file

_t = parse_prototype
