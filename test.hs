import System (getArgs)
--import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Document
import Perl5Parser.Serialize
import qualified Perl5Parser.Token

must_be_same s s' = if s /= s' then "NOT OK: " ++ s' ++ " instead of " ++ s ++ "\n" else ""

error_if "" = ""
error_if s = error s


ok_numbers = "0 1 1. 1.2 1E3 1.E3 0XFFF 0xFf1 0755"

ok_string = "'' 'a' 'a\\b' '\\'' '\\a' 'a\\bc' 'foo'"

ok_version_numbers = "v5 v5.6 5 5. 5.6 5.6.1"

test_tokens = 
    parse_and_verif ok_numbers ++ 
    parse_and_verif ok_string ++
    parse_and_verif ok_version_numbers
    where
         parse_and_verif s = must_be_same s s'
             where
               s' = verbatim $ parse parser' initial_state "" s
               parser' = manY $ Perl5Parser.Token.p_Token
--------------------------------------------------------------------------------
ok_exprs = [ ("1+2", "1+2")
           , ("1-2-3", "(1-2)-3")
           , ("1**2**3", "1**(2**3)")
           , ("1+2*3", "1+(2*3)")
           , ("1 + 2 * 3 + 4 || 5 * 6", "((1 + (2 * 3 ))+ 4 )|| (5 * 6)")
           , ("~ 1 + not 2 + 3, 4 and 5", "((~ 1 )+ (not ((2 + 3), 4 )))and 5")
           , ("1?2:3", "1?2:3")
           , ("1 ? 2 : 3 ? 4 : 5", "1 ? 2 : (3 ? 4 : 5)")
           , ("1 ? 2 ? 3 : 4 : 5", "1 ? (2 ? 3 : 4 ): 5")
           , ("f(1)", "f(1)")
           , ("f 1", "f 1")
           , ("f 1, 2", "f (1, 2)")
           , ("f 1+2", "f (1+2)")
           , ("f 1 and 2", "(f 1 )and 2")
           , ("f 1+2 and 3", "(f (1+2 ))and 3")
           , ("1 ? f 2 : 3", "1 ? (f 2 ): 3")
           , ("f 1 ? 2 : 3", "f (1 ? 2 : 3)")
           , ("1 ? 2 : f 3, 4", "1 ? 2 : (f (3, 4))")
           , ("1 ? f 2, 3 : 4", "1 ? (f (2, 3 )): 4")
           , ("1 ? f 2, 3 ? 4 : 5 : 6", "1 ? (f (2, (3 ? 4 : 5 ))): 6") -- perl doesn't parse it correctly, should we also fail?
           -- identifiers
           , ("::f 1", "::f 1")
           , ("$:: = 1", "$:: = 1")
           , ("$a:: = 1", "$a:: = 1")
           , ("$::a = 1", "$::a = 1")
           , ("$:::: = 1", "$:::: = 1")
           , ("$::::a:::: = 1", "$::::a:::: = 1")
           -- explicit deref
           , ("$a->meth", "$a->meth")
           , ("$a->'meth'", "$a->'meth'")
           , ("$a->\"meth\"", "$a->\"meth\"")
           , ("$a->$m", "$a->$m")
           , ("a->meth", "a->meth")
           , ("a-> meth", "a-> meth")
           , ("a -> meth", "a -> meth")
           , ("a ->meth(0)", "a ->meth((0))")
           , ("$a->[0]", "$a->[(0)]")
           , ("$a->(0)", "$a->((0))")
           , ("$a->{aa}", "$a->{aa}")
           , ("$a->{connect}", "$a->{connect}")
           , ("$a->{'aa'}", "$a->{('aa')}")
           -- ff {0} and ff [0] are invalid perl, must be disallowed somehow
           , ("$ a", "$ a")
           , ("$ $$$a", "$ $$$a")
           , ("@$a", "@$a")
           , ("@ $a", "@ $a")
           , ("@{a}", "@{a}")
           , ("@ {a}", "@ {a}")
           , ("&$a", "(&$a)")
           , ("$1=1", "$1=1")
           , ("$ 1 = 1", "$ 1 = 1")
           , ("$111 = 1", "$111 = 1")
           , ("$\\ = 1", "$\\ = 1")
           , ("$ \\ = 1", "$ \\ = 1")
           , ("$:=1", "$:=1")
           , ("$ : = 1", "$ : = 1")
           , ("map {1}&a", "map {(1)}((&a))")
           , ("eval {1}&a", "(eval {(1)})&(a)")
           , ("eval {1}&&a", "(eval {(1)})&&(a)")
           , ("fork/2", "(fork)/2")
           , ("scalar/2/", "scalar/2/")
           , ("foo/2", "(foo)/2")
           , ("{ aa => 1, connect => 2, x => 3 }", "{ ((((aa => 1), connect )=> 2), x )=> 3 }")
           , ("`foo`", "`foo`")
           , ("$if = 1", "$if = 1")
           -- complex ?: calls
           , ("($v ? N() . '; ' . N() . ': ' . $w : N())", "($v ? ((((N() ). '; ' ). ((N() ). ': ' )). $w ): (N()))")
           , ("$a = 0 || !1 ? 2 : 3", "$a = ((0 || (!1 ))? 2 : 3)")
           , ("my @l = ref($needs) ? @$needs : $needs;", "(my @l )= ((ref($needs) )? @$needs : $needs);")
           , ("0 ? 1 : f() ? 2 : 3", "0 ? 1 : ((f() )? 2 : 3)")
           , ("1 ? map { 1 } f() : map { 2 } g()", "1 ? (map { (1 )} (f() )): (map { (2 )} (g()))")
           ]

test_exprs = concat $ map test ok_exprs
    where test (input, wanted) =
              let ast = parse prog initial_state "" input in
              let s' = verbatim ast in
              let s_prio = with_parentheses ast in
              must_be_same input s' ++ must_be_same wanted s_prio

test :: String -> IO ()
test test_file =
    do s <- readFile test_file
       let ast = parse prog initial_state test_file s
       writeFile (test_file ++ ".new") (verbatim ast)
       putStrLn (with_parentheses ast) >> print ast


main = 
    do getArgs >>= mapM_ test
       seq (error_if test_tokens) $ seq (error_if test_exprs) $ return ()
