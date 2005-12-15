module Perl5Parser.Lines
    ( anonymous_sub, lines_, block
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import Perl5Parser.Expr
import qualified Perl5Parser.Token
import qualified Perl5Parser.Token.Number

line :: Perl5Parser Node
sideff :: Perl5Parser Node
infix_cmd :: Perl5Parser Node
use :: Perl5Parser Node

-- | A collection of "lines" in the program
lines_ = pcons line lines_
        <|> pcons sideff (option [] (pcons semi_colon lines_))
        <|> return []

line = format
       <|> sub_declaration
       <|> if_then
       <|> loop
       <|> foreach
       <|> label
       <|> newNode"block" block
       <|> Perl5Parser.Token.p_Pod
       <|> semi_colon -- unneeded ";"

format = newNode"format"$ seQ 
         [ symbol "format"
         , word
         , operator "="
         , toListToken $ anyTill (try_string "\n.\n")
         , toTokens spaces_comments
         ]

sub_declaration	= newNode"sub_declaration"$ seQ
          [ symbol "sub"
          , word
          , prototype
          , subattrlist
          , block <|> operator ";"
          ]

anonymous_sub = newNode"anonymous_sub"$ seQ 
                [ symbol "sub"
                , prototype
                , subattrlist
                , block
                ]

prototype = option [] $ seq_toTokens [ charl '(' , anyTill (charl ')') ]
subattrlist = manY (seQ [ operator ":", word ])

package = newNode"package"$ seQ [ symbol "package" , option [] ident ]

-- | Real conditional expressions
if_then = newNode"if_then"$ seQ l
    where l = [ symbol "if" <|> symbol "unless"
              , paren_expr
              , block
              ]
loop = newNode"loop"$ seQ l
    where l = [ symbol "while" <|> symbol "until"
              , paren_option_expr
              , block
              , option [] continue_block
              ]
foreach  = newNode"for"$ seQ l
    where l = [ symbol "for" <|> symbol "foreach"
              , foreach_novar <|> foreach_var
              , block
              , option [] continue_block
              ]
foreach_novar = try paren_expr <|> seQ 
              [ operator "("
              , option_expr
              , operator ";"
              , option_expr
              , operator ";"
              , option_expr
              , operator ")"
              ]

foreach_var = seQ 
              [ option [] (scalar_variable <|> seQ [ var_declarator, scalar_variable ])
              , paren_expr
              ]

block :: Perl5Parser [Node]
block = seQ [ operator "{", lines_, operator "}" ]
continue_block = seQ [ symbol "continue", block ]

scalar_variable = seQ [ operator "$", word ]
var_declarator = any_symbol [ "my", "our" ]
semi_colon = newNode"Token::Structure"$ operator ";"
label = newNode"Token::label"$ try$ seQ [ word, operator ":" ]


-- | An expression which may have a side-effect
sideff = use <|> package <|> infix_cmd

infix_cmd = do e <- expr
               option e $ newNode"infix_cmd"$ fmap (e :) infix_cmd_optional
infix_cmd_optional = seQ [ choice (map symbol [ "unless", "while", "until", "for" ]) <?> ""
                         , lexpr
                         ]

use = newNode"use"$ try$ seQ [ symbol "use"
                             , toListToken Perl5Parser.Token.Number.p_VersionNumber <|> use_module
                             , toTokens spaces_comments
                             ]

use_module = word
