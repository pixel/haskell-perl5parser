module Perl5Parser.Token
    ( p_Token
    , p_Pod
    , p_Label
    , p_Attributes
    , p_Ident, p_Ident_sure, p_Ident_raw, p_Filetest_raw
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Prototype
import qualified Perl5Parser.Token.Number
import qualified Perl5Parser.Token.QuoteLike
import qualified Perl5Parser.Token.Quote
import qualified Perl5Parser.Token.Regexp
import qualified Perl5Parser.Token.HereDoc


p_Pod :: Perl5Parser [TokenT]
p_Pod = pcons (fmap Pod p_Pod_raw) spaces_comments
p_Pod_raw = seQ 
         [ seQ [ lineBegin (charl '='), toList (satisfy isAlpha) ]
         , anyTill (try_string "\n=cut")
         , anyTill (charl '\n')
         ]

p_Label :: Perl5Parser [TokenT]
p_Label = pcons p_Label_raw spaces_comments
p_Label_raw = try$ do s <- word_raw
                      sp <- fmap (map Whitespace) spaces_no_nl
                      l <- operator ":"
                      notFollowedBy (char ':') -- for pkg::f()
                      return$ Label s (sp ++ l)

-- | :: a ::b  c:: ::d:: e::f e'f
p_Ident :: Perl5Parser [TokenT]
p_Ident = pcons (fmap Word p_Ident_raw) spaces_comments

-- | same as p_Ident with also 'b (::b)
p_Ident_sure :: Perl5Parser [TokenT]
p_Ident_sure = pcons (fmap Word p_Ident_raw_cont) spaces_comments

p_Ident_raw :: Perl5Parser String
p_Ident_raw = seQ [ try_string "::", option "" p_Ident_raw_cont ]
              <|> seQ [ word_raw, option "" p_Ident_raw_cont ]

p_Ident_raw_cont = seQ [ try_string "'", word_raw, option "" p_Ident_raw_cont ]
                   <|> p_Ident_raw

-- | file test functions (eg: -x '/sbin/halt')
p_Filetest_raw = try $ do char '-'
                          c <- endWord (oneOf Perl5Parser.Prototype.filetest_functions)
                          return$ "-" ++ [c]

p_Attributes :: Perl5Parser [TokenT]
p_Attributes = fmap concat $ many1 $ seQ [ operator ":"
                                         , manY (pcons attribute spaces_comments)
                                         ]
    where attribute = do w <- word_raw
                         para <- toMaybe parameters
                         return (Attribute w para)
          parameters = do char '('
                          (_, s) <- Perl5Parser.Token.Quote.inside_string '('
                          return s



p_Token :: Perl5Parser [TokenT]
p_Token = do pcons p spaces_comments
    where p = 
                  fmap to_Quote Perl5Parser.Token.Quote.p_Interpolate
              <|> fmap to_Quote Perl5Parser.Token.Quote.p_Literal
              <|> Perl5Parser.Token.Quote.p_Single
              <|> Perl5Parser.Token.Quote.p_Double

              <|> fmap (Number NormalNumber) Perl5Parser.Token.Number.p_Number
              <|> fmap (Number VersionNumber) Perl5Parser.Token.Number.p_VersionNumber

              <|> fmap Regexp Perl5Parser.Token.Regexp.p_Match
              <|> fmap Regexp Perl5Parser.Token.Regexp.p_Substitute
              <|> fmap Regexp Perl5Parser.Token.Regexp.p_Transliterate
              <|> fmap Regexp Perl5Parser.Token.Regexp.p_Qr
              <|> fmap to_QuoteLike Perl5Parser.Token.QuoteLike.p_Backstick
              <|> fmap to_QuoteLike Perl5Parser.Token.QuoteLike.p_Words

                  -- !! HereDoc before Readline and Glob !!
              <|> Perl5Parser.Token.HereDoc.p_HereDoc

                  -- !! Readline before Glob !!
              <|> fmap to_QuoteLike Perl5Parser.Token.QuoteLike.p_Readline
              <|> fmap to_QuoteLike Perl5Parser.Token.QuoteLike.p_Glob

              <|> p_EndData

          to_Quote (cc, s) = Quote cc s
          to_QuoteLike (cc, s) = QuoteLike cc s


p_EndData :: Perl5Parser TokenT
p_EndData = do sep <- choice (map (endWord . string) [ "__END__", "__DATA__" ])
               comment <- manyl (satisfy (/= '\n'))
               l <- option [] (toList $ string "\n")
               s <- many anyChar
               return$ Separator (if sep == "__END__" then Separator_End else Separator_Data) (comment ++ l) s
