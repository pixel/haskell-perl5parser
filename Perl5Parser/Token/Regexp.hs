module Perl5Parser.Token.Regexp
    ( p_Match
    , p_Substitute
    , p_Transliterate
    ) where

import Perl5Parser.Common
import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Token.Quote (user_delimited_string, user_delimited_string_p, inside_string)


p_Match :: Perl5Parser RegexpT
p_Match = do (structure, s) <- user_delimited_string "m"
             options <- regexp_options
             return$ Match structure s options

p_Substitute :: Perl5Parser RegexpT
p_Substitute = do (structure, s1, s2) <- user_delimited_ter "s"
                  options <- regexp_options
                  return$ Substitute structure s1 s2 options

p_Transliterate :: Perl5Parser RegexpT
p_Transliterate = p "tr" <|> p "y"
    where p name = do (structure, s1, s2) <- user_delimited_ter name
                      options <- regexp_options
                      return$ Transliterate (name, structure) s1 s2 options


regexp_options = many (oneOf "egimosx")

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

user_delimited_ter :: String -> Perl5Parser (SubstituteT, String, String)
user_delimited_ter s =
    do ((co, cc), s) <- user_delimited_string_p$ try (string s >> notWord)
       (cc2, s2) <- case cc of
                      NonBalanced c -> non_balanced c
                      Balanced c1 c2 -> balanced c1 c2
       return ((co, cc2), s, s2) 

    where 
      non_balanced :: Char -> Perl5Parser (BalancedOrNot (Char, LiteralT), String)
      non_balanced c =
              do (cc, s) <- inside_string c
                 let cc' = case cc of
                          NonBalanced c -> NonBalanced c
                          Balanced _ _ -> internal_error "user_delimited_ter/non_balanced"
                 return (cc', s)
                 

      balanced :: Char -> Char -> Perl5Parser (BalancedOrNot (Char, LiteralT), String)
      balanced c1 c2 = 
              do (structure, s) <- user_delimited_string_p notWord
                 return (Balanced c1 (c2, structure), s)

