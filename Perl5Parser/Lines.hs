module Perl5Parser.Lines
    ( anonymous_sub, lines_, block
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Term
import Perl5Parser.Expr
import qualified Perl5Parser.Env as Env
import qualified Perl5Parser.Token
import qualified Perl5Parser.Token.Number

op = toList . operator_node

local_ident s = toList $ fmap Token $ with_spaces_comments (fmap (Ident LocalIdent) $ endWord (string s))
symbol_ = toList . symbol_node

-- | A collection of "lines" in the program
lines_ :: Perl5Parser [Node]
lines_ = pcons line lines_
        <|> pcons sideff (option [] (pcons semi_colon lines_))
        <|> return []

line :: Perl5Parser Node
line = format
       <|> scheduled_declaration
       <|> sub_declaration
       <|> if_then
       <|> loop
       <|> foreach
       <|> label
       <|> newNode"block" block
       <|> pod
       <|> semi_colon -- unneeded ";"

format = newNode"format"$ seQ 
         [ local_ident "format"
         , toList (fmap Token word)
         , op "="
         , toList (fmap Token $ with_spaces_comments (fmap PictureFormat $ anyTill (try_string "\n.\n")))
         ]

sub_declaration	= newNode"Statement::Sub" p
    where p = do (ident, l1) <- try sub_xxx -- ^ try needed for anonymous_sub
                 (proto, l2) <- option_prototype
                 case proto of
                   Nothing -> return ()
                   Just proto -> Env.set_prototype ident proto
                 l3 <- subattrlist
                 l4 <- block <|> op ";"
                 return (l1 ++ l2 ++ l3 ++ l4)
          sub_xxx = do l1 <- symbol_ "sub"
                       (fq, i) <- Perl5Parser.Token.p_Ident_sure_raw
                       l2 <- spaces_comments
                       return ((fq, i), l1 ++ [Token (Ident fq i, l2)])


scheduled_declaration = newNode"Statement::Scheduled"$ seQ
          [ choice $ map local_ident [ "BEGIN", "CHECK", "INIT", "END", "AUTOLOAD" ] -- ^ cf AutoLoader.pm for such an AUTOLOAD example
          , fmap snd option_prototype
          , subattrlist
          , block <|> op ";"
          ]

anonymous_sub = newNode"anonymous_sub"$ seQ 
                [ notFollowedBy_ (string "=>") (symbol_ "sub")
                , fmap snd option_prototype
                , subattrlist
                , block
                ]

option_prototype = option (Nothing, []) prototype
prototype :: Perl5Parser (Maybe String, [Node])
prototype = do char '('
               proto <- many (satisfy (/= ')'))
               char ')'
               l <- spaces_comments
               return (Just proto, [Token (Prototype proto, l)])

subattrlist = option [] Perl5Parser.Token.p_Attributes

package = newNode"package"$ p
    where p = do l1 <- symbol_node "package"
                 (fq, i) <- Perl5Parser.Token.p_Ident_raw
                 let pkg = case fq of LocalIdent -> i ; _ -> fq_canonical fq ++ "::" ++ i
                 Env.set_package pkg
                 l2 <- spaces_comments
                 return$ l1 : [Token (Word pkg, l2)]

-- | Real conditional expressions
if_then = newNode"if_then"$ Env.with_new_lexical_block$ seQ l
    where l = [ symbol_ "if" <|> symbol_ "unless"
              , paren_expr
              , block_allow_pod
              , option [] (elsif <|> else_)
              ]
elsif = seQ [ symbol_ "elsif"
            , paren_expr
            , block_allow_pod
            , option [] (elsif <|> else_)
            ]
else_ = seQ [ symbol_ "else"
            , block
            ]

loop = newNode"loop"$ Env.with_new_lexical_block$ seQ l
    where l = [ symbol_ "while" <|> symbol_ "until"
              , paren_option_expr
              , block_allow_pod
              , option [] continue_block
              ]
foreach  = newNode"for"$ Env.with_new_lexical_block$ seQ l
    where l = [ symbol_ "for" <|> symbol_ "foreach"
              , foreach_novar <|> foreach_var
              , block_allow_pod
              , option [] continue_block
              ]
foreach_novar = try paren_expr <|> seQ 
              [ op "("
              , option_expr
              , op ";"
              , option_expr
              , op ";"
              , option_expr
              , op ")"
              ]

-- using "scalar" even if it allows ${xxx} whereas it is not valid perl
foreach_var = seQ 
              [ option [] (toList scalar <|> pcons var_declarator (toList scalar))
              , paren_expr
              ]

pod = newNode"pod" (toList $ fmap Token Perl5Parser.Token.p_Pod)

block :: Perl5Parser [Node]
block = Env.with_new_lexical_block$ seQ [ op "{", lines_, op "}" ]
continue_block = seQ [ symbol_ "continue", block ]
block_allow_pod = seQ [ block, many pod ]

var_declarator = any_symbol_node [ "my", "our" ]
semi_colon = newNode"Token::Structure"$ op ";"
label = fmap Token Perl5Parser.Token.p_Label


-- | An expression which may have a side-effect
sideff :: Perl5Parser Node
sideff = use <|> package <|> infix_cmd

infix_cmd :: Perl5Parser Node
infix_cmd = do e <- expr
               option e $ newNode"infix_cmd"$ fmap (e :) infix_cmd_optional
infix_cmd_optional = seQ [ choice (map symbol_ infix_cmds) <?> ""
                         , lexpr
                         ]

use :: Perl5Parser Node
use = newNode"use"$ try$ seQ [ symbol_ "use"
                             , toList version_number <|> use_module
                             , lexpr
                             ]
    where
      version_number = with_spaces_comments_ (fmap (Number VersionNumber) Perl5Parser.Token.Number.p_VersionNumber)
      use_module = pcons (fmap Token Perl5Parser.Token.p_Ident) (option [] (toList version_number))

