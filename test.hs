import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Document
import qualified Perl5Parser.Serialize
import qualified Perl5Parser.Token

ok_numbers = "0 1 1. 1.2 1E3 1.E3 0XFFF 0xFf1 0755"

ok_string = "'' 'a' 'a\\b' '\\'' '\\a' 'a\\bc' 'foo'"

ok_version_numbers = "v5 v5.6 5 5. 5.6 5.6.1"

test_tokens = 
    do parse_and_verif ok_numbers
       parse_and_verif ok_string
       parse_and_verif ok_version_numbers
    where
         parse_and_verif s = 
             if s /= s' then putStrLn ("NOT OK: " ++ s' ++ " instead of " ++ s) else return ()
             where
               s' :: String
               s' = Perl5Parser.Serialize.to_s $ parse parser' initial_state s
               parser' = manY $ Perl5Parser.Token.p_Token
--------------------------------------------------------------------------------
ok_exprs = ("1+2", "1+2")

test_exprs = test ok_exprs
    where test (input, _wanted) =
              let s = parse prog initial_state input in
              putStrLn $ Perl5Parser.Serialize.to_s (s :: Node)

-- 1 + 2 * 3 + 4 || 5 * 6
-- 1?2:3
-- !2
-- 1-2-3
-- ~ 1 + not 2 + 3, 4 and 5;  # ((~1 + not((2 + 3), 4)) and 5)
-- 1 ? g 5 : 6;
-- 1 ? 2 : 3 ? 4 : 5 ;    # ?[ 1 "?" " " :[ 2 ":" " " ?[ 3 "?" " " :[ 4 ":" " " 5 ] ] ] ]
-- 1 ? 2 ? 3 : 4 : 5 ;    # ?[ 1 "?" " " ?[ 2 "?" " " :[ 3 ":" " " :[ 4 ":" " " 5 ] ] ] ]
-- $foo ? @l : join '', @l;
-- 
-- f($a ? g $b, $c ? $d : $e : $f);





_test = 
    do s <- readFile test_file
       let ast = parse prog initial_state s
       print ast
    where
      test_file = "/tmp/t.pl"
      --test_file = "/home/pixel/cooker/soft/perl-MDK-Common/MDK/Common/File.pm"


main = test_tokens 
       >> test_exprs
       -- >> test
