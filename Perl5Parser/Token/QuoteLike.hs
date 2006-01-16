module Perl5Parser.Token.QuoteLike
    ( p_Readline
    , p_Glob
    , p_Words
    , p_Backstick
    , p_Qx
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Token.Quote (user_delimited_string, inside_string)


p_Readline :: Perl5Parser (QuoteLikeT, String)
p_Readline =
    -- | try needed for token_QuoteLike_Glob
    try$ do char '<'
            s <- pcons (char '$') word_raw <|> word_raw <|> return ""
            char '>'
            return (Readline, s)

--------------------------------------------------------------------------------

p_Glob :: Perl5Parser (QuoteLikeT, String)
p_Glob = do char '<'
            (_, s) <- inside_string '<'
            return (Glob, s)

p_Words :: Perl5Parser (QuoteLikeT, String)
p_Words = do (structure, s) <- user_delimited_string "qw"
             return (Words structure, s)

p_Backstick :: Perl5Parser (QuoteLikeT, String)
p_Backstick = do char '`'
                 (_, s) <- inside_string '`'
                 return (Backstick, s)

p_Qx :: Perl5Parser (QuoteLikeT, String)
p_Qx = do (structure, s) <- user_delimited_string "qx"
          return (Command structure, s)
