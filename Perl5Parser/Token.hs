module Perl5Parser.Token
    ( p_Token
    , p_Pod
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Token.Number
import qualified Perl5Parser.Token.QuoteLike
import qualified Perl5Parser.Token.Quote
import qualified Perl5Parser.Token.Regexp
import qualified Perl5Parser.Token.HereDoc


p_Pod :: Perl5Parser Node
p_Pod = newNode"Token::Pod" (toTokens $ pcons p_Pod_raw spaces_comments)
p_Pod_raw = seQ 
         [ seQ [ lineBegin (charl '='), toList (satisfy isAlpha) ]
         , anyTill (try_string "\n=cut")
         , anyTill (charl '\n')
         ]


fname s = fmap (\v -> (NodeName s, v))

p_Token :: Perl5Parser Node
p_Token = do (name, l) <- p
             l2 <- spaces_comments
             return$ Node(name, map Token (l ++ l2))
    where p = 
                  fmap (\(n,s) -> (n,[s])) p1 
              <|> p2
              <|> fmap (\(n,(_,s)) -> (n,[s])) p3

          p1 = 
                  fname"Token::Number" Perl5Parser.Token.Number.p_Number
              <|> fname"Token::Number" Perl5Parser.Token.Number.p_VersionNumber

          p2 = 
                  fname"Token::Regexp::Match" Perl5Parser.Token.Regexp.p_Match
              <|> fname"Token::Regexp::Substitute" Perl5Parser.Token.Regexp.p_Substitute
              <|> fname"Token::Regexp::Transliterate" Perl5Parser.Token.Regexp.p_Transliterate
              <|> fname"Token::Quote::Interpolate" Perl5Parser.Token.Quote.p_Interpolate
              <|> fname"Token::Quote::Literal" Perl5Parser.Token.Quote.p_Literal
              <|> fname"Token::QuoteLike::Words" Perl5Parser.Token.QuoteLike.p_Words

                  -- !! HereDoc before Readline and Glob !!
              <|> fname"Token::QuoteLike::HereDoc" Perl5Parser.Token.HereDoc.p_HereDoc

          p3 = 
                  -- !! Readline before Glob !!
                  fname"Token::QuoteLike::Readline" Perl5Parser.Token.QuoteLike.p_Readline
              <|> fname"Token::QuoteLike::Glob" Perl5Parser.Token.QuoteLike.p_Glob

              <|> fname"Token::Quote::Single" Perl5Parser.Token.Quote.p_Single
              <|> fname"Token::Quote::Double" Perl5Parser.Token.Quote.p_Double
