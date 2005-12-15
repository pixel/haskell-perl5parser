module Perl5Parser.Token.HereDoc
    ( p_HereDoc
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Token.Quote


-- IMPORTANT: most of the work is done in spaces_comments (which uses spaces_comments_with_here_doc)

p_HereDoc :: Perl5Parser [String]
p_HereDoc = do r <- try_string "<<"
               (s, l) <- here_doc_next
               updateState (\state -> state { next_line_is_here_doc = Just s })
               return (r : l)

here_doc_next :: Perl5Parser (String, [String])
here_doc_next = do s <- many1 wordAny
                   return (s, [s])
            <|> do l <- spaces
                   (inside_s, s) <- Perl5Parser.Token.Quote.p_Single <|> Perl5Parser.Token.Quote.p_Double
                   return (inside_s, l ++ [s])
