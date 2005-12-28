module Perl5Parser.Token.HereDoc
    ( p_HereDoc
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Token.Quote


-- IMPORTANT: most of the work is done in spaces_comments (which uses spaces_comments_with_here_doc)

p_HereDoc :: Perl5Parser TokenT
p_HereDoc = do try_string "<<"
               (space, tok, s) <- here_doc_next
               updateState (\state -> state { next_line_is_here_doc = Just s })
               return$ HereDoc space tok

here_doc_next :: Perl5Parser ([TokenT], TokenT, String)
here_doc_next = do s <- many1 wordAny  -- ^ and not word_raw because <<1 is allowed
                   return ([], Word s, s)
            <|> do l <- spaces_token
                   (quote, s) <- Perl5Parser.Token.Quote.p_Single_raw <|> Perl5Parser.Token.Quote.p_Double_raw
                   return (l, Quote quote s, s)
