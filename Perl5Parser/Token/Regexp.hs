module Perl5Parser.Token.Regexp
    ( p_Match
    , p_Substitute
    , p_Transliterate
    ) where

import Perl5Parser.Common
import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Token.Quote (user_delimited_string, user_delimited_string_p, inside_string)


p_Match = seQ [user_delimited_string "m", regexp_options]

p_Substitute = seQ [user_delimited_ter "s", regexp_options]
p_Transliterate = seQ [user_delimited_ter "tr", regexp_options]
              <|> seQ [user_delimited_ter "y", regexp_options]


regexp_options = manyl (oneOf "egimosx")

{- one must handle this:
print(q # foo
      /bar/
      );#);

s/\s/AA/g;
s /\s/BB/g;
s #\s#CC#g;
  /\s/DD/g;
s (\s) {EE}g;
s (\s)#EE#g;

-}

user_delimited_ter = fmap (\(_,_,l) -> l) . user_delimited_ter_

user_delimited_ter_ :: String -> Perl5Parser (String, String, [String])
user_delimited_ter_ s =
    do (c, inside_s, l) <- user_delimited_string_p$ try (string s >> notWord)
       (inside_s2, l2) <- if isNothing (balancedDelim c) then non_balanced c else balanced
       return (inside_s, inside_s2, [s] ++ l ++ l2)
    where non_balanced c = fmap (\(s, l) -> (s, [l])) (inside_string c)
          balanced = fmap (\(_c, s, l) -> (s, l)) (user_delimited_string_p notWord)

