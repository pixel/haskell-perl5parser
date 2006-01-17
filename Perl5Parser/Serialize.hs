module Perl5Parser.Serialize
    ( Serialize
    , verbatim
    , with_parentheses
    )
    where

import Perl5Parser.Types

class Serialize a where
    verbatim :: a -> String
    with_parentheses :: a -> String

    with_parentheses = verbatim

instance Serialize a => Serialize [a] where
    verbatim = concat . map verbatim

    with_parentheses = concat . map with_parentheses

instance Serialize Node where
    verbatim (Node(_, l)) = verbatim l
    verbatim (Call(_, l)) = verbatim l
    verbatim (Tokens l) = verbatim l

    with_parentheses (Call(_, l)) = concat $ map may_add_para l
        where may_add_para e = if add_para e then "(" ++ s ++ ")" else s
                  where s = with_parentheses e
                        add_para (Tokens _) = False
                        add_para (Node(NodeName n, _)) = not $ elem n ["paren_option_expr", "$", "@", "%", "{}", "[]" ]
                        add_para _ = True
    with_parentheses (Node(_, l)) = with_parentheses l
    with_parentheses (Tokens l) = with_parentheses l

instance Serialize SpaceCommentT where
    verbatim (Whitespace s) = s
    verbatim (Comment s) = s
    verbatim (HereDocValue s) = s

instance Serialize TokenT where
    verbatim (Quote t s) = to_s_Quote t s
    verbatim (QuoteLike t s) = to_s_QuoteLike t s
    verbatim (Regexp t) = to_s_Regexp t
    verbatim (Separator sep l s) = to_s_Separator sep ++ concat l ++ s
    verbatim (HereDoc co name) = "<<" ++ verbatim co ++ verbatim name
    verbatim (Label label co) = label ++ verbatim co ++ ":"
    verbatim (Number _ s) = s
    verbatim (Word s) = s
    verbatim (SpaceComment l) = verbatim l
    verbatim (PictureFormat s) = s
    verbatim (Prototype s) = s
    verbatim (Ident l i) = concatMap (\(i,delim) -> i ++ delim) l ++ i
    verbatim (Symbol s) = s
    verbatim (Operator s) = s
    verbatim (Pod s) = s
    verbatim (Attribute s Nothing) = s
    verbatim (Attribute s (Just para)) = s ++ "(" ++ para ++ ")"

--    with_parentheses (Comment _) = ""
--    with_parentheses e = verbatim e

to_s_Quote Double s = "\"" ++ s ++ "\""
to_s_Quote Single s = "'" ++ s ++ "'"
to_s_Quote (Literal structure) s = "q" ++ to_s_structure structure s
to_s_Quote (Interpolate structure) s = "qq" ++ to_s_structure structure s

to_s_QuoteLike Glob s = "<" ++ s ++ ">"
to_s_QuoteLike Readline s = "<" ++ s ++ ">"
to_s_QuoteLike Backstick s = "`" ++ s ++ "`"
to_s_QuoteLike (Words t) s = "qw" ++ to_s_structure t s
to_s_QuoteLike (Command t) s = "qx" ++ to_s_structure t s

to_s_Regexp (Match Nothing s opt) = "/" ++ s ++ "/" ++ opt
to_s_Regexp (Match (Just t) s opt) = "m" ++ to_s_structure t s ++ opt
to_s_Regexp (Qr t s opt) = "qr" ++ to_s_structure t s ++ opt
to_s_Regexp (Substitute t s1 s2 opt) = "s" ++ to_s_subst t s1 s2 ++ opt
to_s_Regexp (Transliterate (name, t) s1 s2 opt) = name ++ to_s_subst t s1 s2 ++ opt

to_s_Separator Separator_Data = "__DATA__"
to_s_Separator Separator_End = "__END__"

to_s_structure :: LiteralT -> String -> String
to_s_structure (co, cc) s = verbatim co ++ [c1] ++ s ++ [c2]
    where (c1, c2) = from_balanced cc

to_s_subst :: SubstituteT -> String -> String -> String
to_s_subst (co, cc) s1 s2 = verbatim co ++ to cc
    where to (NonBalanced c) = [c] ++ s1 ++ [c] ++ s2 ++ [c]
          to (Balanced c1 (c2, t)) = [c1] ++ s1 ++ [c2] ++ to_s_structure t s2
                  


from_balanced (Balanced c1 c2) = (c1, c2)
from_balanced (NonBalanced c) = (c, c)
