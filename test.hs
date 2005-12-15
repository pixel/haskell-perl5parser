import Perl5Parser.ParserHelper
import Perl5Parser.Document
import qualified Perl5Parser.Token.Number
import qualified Perl5Parser.Token.Quote

ok_numbers = "0 1 1. 1.2 1E3 1.E3 0XFFF 0xFf1 0755"
ok_numbers_parser = Perl5Parser.Token.Number.p_Number

ok_string = "'' 'a' 'a\\b' '\\'' '\\a' 'a\\bc' 'foo'"
ok_string_parser = fmap snd Perl5Parser.Token.Quote.p_Single

ok_version_numbers = "v5 v5.6 5 5. 5.6 5.6.1"
ok_version_numbers_parser = Perl5Parser.Token.Number.p_VersionNumber

test_lexemes = 
    do parse_and_verif ok_numbers_parser ok_numbers
       parse_and_verif ok_string_parser ok_string
       parse_and_verif ok_version_numbers_parser ok_version_numbers
    where
         parse_and_verif parser s = 
             if words s /= s' then putStrLn ("NOT OK: " ++ show s' ++ " instead of " ++ show (words s)) else return ()
             where
               s' :: [String]
               s' = parse parser' initial_state s
               parser' = many $ do { n <- parser; spaces_comments; return n }
--------------------------------------------------------------------------------

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




test_file = "/tmp/t.pl"
--test_file = "/home/pixel/cooker/soft/perl-MDK-Common/MDK/Common/File.pm"

test file = do s <- readFile file
               let ast = parse prog initial_state s
               print ast

main = test_lexemes >> test test_file
