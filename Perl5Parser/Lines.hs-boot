module Perl5Parser.Lines
    ( anonymous_sub, lines_, block
    ) where

import Perl5Parser.Types

anonymous_sub :: Perl5Parser Node
lines_ :: Perl5Parser [Node]
block :: Perl5Parser [Node]
