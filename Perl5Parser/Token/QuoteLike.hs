module Perl5Parser.Token.QuoteLike
    ( p_Readline
    , p_Glob
    , p_Words
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Token.Quote (user_delimited_string, inside_string)


p_Readline :: Perl5Parser (String, String)
p_Readline =
    -- | try needed for token_QuoteLike_Glob
    try$ do c <- char '<'
            s <- pcons (char '$') word_raw <|> word_raw <|> return ""
            c2 <- char '>'
            return (s, (c : s) ++ [c2])

--------------------------------------------------------------------------------

p_Glob :: Perl5Parser (String, String)
p_Glob = do c1 <- char '<'
            (inside_s, s) <- inside_string '<'
            return (inside_s, c1 : s)

p_Words = seQ [user_delimited_string "qw", spaces_comments]
