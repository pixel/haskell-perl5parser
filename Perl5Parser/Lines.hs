module Perl5Parser.Lines
    ( anonymous_sub, lines_, block
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Term
import Perl5Parser.Expr
import qualified Perl5Parser.Token
import qualified Perl5Parser.Token.Number

op = toList . operator_node

local_ident s = toList $ fmap Tokens $ pcons (fmap (Ident LocalIdent) $ endWord (string s)) spaces_comments_token
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
         , toNodes word
         , op "="
         , toNodes $ pcons (fmap PictureFormat $ anyTill (try_string "\n.\n"))
                           spaces_comments_token
         ]

sub_declaration	= newNode"Statement::Sub"$ seQ
          [ try$ seQ [ symbol_ "sub", toNodes$ Perl5Parser.Token.p_Ident_sure ] -- ^ try needed for anonymous_sub
          , prototype
          , subattrlist
          , block <|> op ";"
          ]

scheduled_declaration = newNode"Statement::Scheduled"$ seQ
          [ choice $ map local_ident [ "BEGIN", "CHECK", "INIT", "END", "AUTOLOAD" ] -- ^ cf AutoLoader.pm for such an AUTOLOAD example
          , prototype
          , subattrlist
          , block <|> op ";"
          ]

anonymous_sub = newNode"anonymous_sub"$ seQ 
                [ notFollowedBy_ (string "=>") (symbol_ "sub")
                , prototype
                , subattrlist
                , block
                ]

prototype = option [] (toNodes $ pcons prototype_ spaces_comments_token)
    where prototype_ = fmap Prototype $ seQ [ charl '(' , anyTill (charl ')') ]

subattrlist = option [] (toNodes Perl5Parser.Token.p_Attributes)
package = newNode"package"$ pcons (symbol_node "package") (toNodes Perl5Parser.Token.p_Ident)

-- | Real conditional expressions
if_then = newNode"if_then"$ seQ l
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

loop = newNode"loop"$ seQ l
    where l = [ symbol_ "while" <|> symbol_ "until"
              , paren_option_expr
              , block_allow_pod
              , option [] continue_block
              ]
foreach  = newNode"for"$ seQ l
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

pod = newNode"pod" (toList $ fmap Tokens Perl5Parser.Token.p_Pod)

block :: Perl5Parser [Node]
block = seQ [ op "{", lines_, op "}" ]
continue_block = seQ [ symbol_ "continue", block ]
block_allow_pod = seQ [ block, many pod ]

var_declarator = any_symbol_node [ "my", "our" ]
semi_colon = newNode"Token::Structure"$ op ";"
label = fmap Tokens Perl5Parser.Token.p_Label


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
                             , toNodes $ version_number <|> use_module
                             , lexpr
                             ]
    where
      version_number = pcons (fmap (Number VersionNumber) Perl5Parser.Token.Number.p_VersionNumber) spaces_comments_token
      use_module = seQ [ Perl5Parser.Token.p_Ident
                       , option [] version_number
                       ]
