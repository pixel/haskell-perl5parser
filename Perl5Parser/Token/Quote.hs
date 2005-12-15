module Perl5Parser.Token.Quote
    ( p_Single, p_Double
    , p_Literal, p_Interpolate
    --
    , user_delimited_string, user_delimited_string_p, inside_string
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper


p_Single :: Perl5Parser (String, String)
p_Single = do c1 <- char '\''
              (inside_s, s) <- inside_string '\''
              return (inside_s, c1 : s)

p_Double :: Perl5Parser (String, String)
p_Double = do c1 <- char '"'
              (inside_s, s) <- inside_string '"'
              return (inside_s, c1 : s)

p_Literal = seQ [user_delimited_string "q", spaces_comments]
p_Interpolate = seQ [user_delimited_string "qq", spaces_comments]



user_delimited_string_p p =
    do (l, c) <- p >>= find_next_if_space
       (inside_s, s2) <- inside_string c
       return (c, inside_s, l ++ [c : s2])

user_delimited_string = fmap snd . user_delimited_string_

user_delimited_string_ :: String -> Perl5Parser (String, [String])
user_delimited_string_ s = 
    do (_c, inside_s, l) <- user_delimited_string_p$ try (string s >> notWord)
       return (inside_s, [s] ++ l)



find_next_if_space c =
    if isSpace c then 
        do l <- spaces_comments
           c <- notWord
           return (l, c)
    else 
        return ([], c)


inside_string c = f "" 1
    where
      (beg, end) = case balancedDelim c of
                     Nothing -> (pzero, char c)
                     Just end -> (char c, char end)
      f accu n = 
            do c <- char '\\'
               c2 <- anyChar
               f (c2 : c : accu) n
        <|> do c <- beg
               f (c : accu) (n+1)
        <|> do c <- end
               if n == 1 then return (reverse accu, reverse (c : accu)) else f (c : accu) (n-1)
        <|> do c <- anyChar
               f (c : accu) n
