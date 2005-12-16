module Perl5Parser.Lines
    ( anonymous_sub, lines_, block
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Expr
import qualified Perl5Parser.Token
import qualified Perl5Parser.Token.Number

op = toList . operator_node

symbol_ = toList . symbol_node

-- | A collection of "lines" in the program
lines_ :: Perl5Parser [Node]
lines_ = pcons line lines_
        <|> pcons sideff (option [] (pcons semi_colon lines_))
        <|> return []

line :: Perl5Parser Node
line = format
       <|> sub_declaration
       <|> if_then
       <|> loop
       <|> foreach
       <|> label
       <|> newNode"block" block
       <|> newNode"pod" (toList $ fmap Tokens Perl5Parser.Token.p_Pod)
       <|> semi_colon -- unneeded ";"

format = newNode"format"$ seQ 
         [ symbol_ "format"
         , toNodes word
         , op "="
         , toNodes $ pcons (fmap PictureFormat $ anyTill (try_string "\n.\n"))
                           spaces_comments
         ]

sub_declaration	= newNode"Statement::Sub"$ seQ
          [ symbol_ "sub"
          , toNodes$ word
          , prototype
          , subattrlist
          , block <|> op ";"
          ]

anonymous_sub = newNode"anonymous_sub"$ seQ 
                [ symbol_ "sub"
                , prototype
                , subattrlist
                , block
                ]

prototype = option [] (toNodes $ toList prototype_)
    where prototype_ = fmap Prototype $ seQ [ charl '(' , anyTill (charl ')') ]

subattrlist = toNodes $ manY (seQ [ operator ":", word ])

package = newNode"package"$ pcons (symbol_node "package") (option [] $ toNodes ident)

-- | Real conditional expressions
if_then = newNode"if_then"$ seQ l
    where l = [ symbol_ "if" <|> symbol_ "unless"
              , paren_expr
              , block
              ]
loop = newNode"loop"$ seQ l
    where l = [ symbol_ "while" <|> symbol_ "until"
              , paren_option_expr
              , block
              , option [] continue_block
              ]
foreach  = newNode"for"$ seQ l
    where l = [ symbol_ "for" <|> symbol_ "foreach"
              , foreach_novar <|> foreach_var
              , block
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

foreach_var = seQ 
              [ option [] (toList scalar_variable <|> pcons var_declarator (toList scalar_variable))
              , paren_expr
              ]

block :: Perl5Parser [Node]
block = seQ [ op "{", lines_, op "}" ]
continue_block = seQ [ symbol_ "continue", block ]

scalar_variable = newNode"$" $ toList $ fmap Tokens $ seQ [ operator "$", word ]
var_declarator = any_symbol_node [ "my", "our" ]
semi_colon = newNode"Token::Structure"$ op ";"
label = fmap Tokens Perl5Parser.Token.p_Label


-- | An expression which may have a side-effect
sideff :: Perl5Parser Node
sideff = use <|> package <|> infix_cmd

infix_cmd :: Perl5Parser Node
infix_cmd = do e <- expr
               option e $ newNode"infix_cmd"$ fmap (e :) infix_cmd_optional
infix_cmd_optional = seQ [ choice (map symbol_ [ "unless", "while", "until", "for" ]) <?> ""
                         , lexpr
                         ]

use :: Perl5Parser Node
use = newNode"use"$ try$ seQ [ symbol_ "use"
                             , toNodes $ seQ [ toList version_number <|> use_module
                                             , spaces_comments ]
                             ]
    where
      version_number = fmap (Number VersionNumber) Perl5Parser.Token.Number.p_VersionNumber
      use_module = ident
