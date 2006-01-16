module Perl5Parser.Token.Quote
    ( p_Single, p_Single_raw, p_Double, p_Double_raw
    , p_Literal, p_Interpolate
    --
    , user_delimited_string, user_delimited_string_p, inside_string
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper

p_Single :: Perl5Parser TokenT
p_Single = fmap (\(cc, s) -> Quote cc s) p_Single_raw

p_Single_raw :: Perl5Parser (QuoteT, String)
p_Single_raw = do char '\''
                  (_, s) <- inside_string '\''
                  return (Single, s)

p_Double :: Perl5Parser TokenT
p_Double = fmap (\(cc, s) -> Quote cc s) p_Double_raw

p_Double_raw :: Perl5Parser (QuoteT, String)
p_Double_raw = do char '"'
                  (_, s) <- inside_string '"'
                  return (Double, s)

p_Literal :: Perl5Parser (QuoteT, String)
p_Literal = do (structure, s) <- user_delimited_string "q"
               return (Literal structure, s)

p_Interpolate :: Perl5Parser (QuoteT, String)
p_Interpolate = do (structure, s) <- user_delimited_string "qq"
                   return (Interpolate structure, s)

user_delimited_string_p :: Perl5Parser Char -> Perl5Parser (LiteralT, String)
user_delimited_string_p p =
    do (l, c) <- p >>= find_next_if_space
       (structure, s) <- inside_string c
       return ((l, structure), s)

user_delimited_string :: String -> Perl5Parser (LiteralT, String)
user_delimited_string s = user_delimited_string_p$ try (string s >> notWord)


find_next_if_space :: Char -> Perl5Parser ([TokenT], Char)
find_next_if_space c =
    if isSpace c then 
        do l <- spaces_comments
           c2 <- notWord
           return (Whitespace [c] : l, c2)
    else 
        return ([], c)


inside_string :: Char -> Perl5Parser (BalancedOrNot Char, String)
inside_string c = fmap (\s -> (cc, s)) (f "" 1)
    where
      (beg, end, cc) = case balancedDelim c of
                         Nothing -> (pzero, char c, NonBalanced c)
                         Just end -> (char c, char end, Balanced c end)
      f accu n = 
            do c <- char '\\'
               c2 <- anyChar
               f (c2 : c : accu) n
        <|> do c <- beg
               f (c : accu) (n+1)
        <|> do c <- end
               if n == 1 then return (reverse accu) else f (c : accu) (n-1)
        <|> do c <- anyChar
               f (c : accu) n
