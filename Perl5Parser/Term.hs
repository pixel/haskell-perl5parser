module Perl5Parser.Term
    ( term, func, scalar
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Token
import {-# SOURCE #-} Perl5Parser.Expr
import {-# SOURCE #-} Perl5Parser.Lines


op = toList . operator_node

term :: Perl5Parser Node
term = anonymous
       <|> declvar
       <|> grouped

       <|> arraylen -- before scalar
       <|> scalar
       <|> star
       <|> hash
       <|> array_maybe_slice
       <|> fmap Tokens Perl5Parser.Token.p_Token


grouped = newNode"grouped" (seQ [ paren_option_expr, option [] paren_next_slice ])

-- | Constructors for anonymous data
anonymous =     newNode"[]" (seQ [ op "[", option_expr, op "]" ])
            <|> newNode"{}" (seQ [ op "{", option_expr, op "}" ])
            <|> anonymous_sub

declvar = newNode"declvar"$ pcons (any_symbol_node [ "my", "our", "local" ]) lexpr


paren_next_slice = seQ [ op "[", option_expr, op "]" ]

array_maybe_slice = do a <- array
                       option a (array_slice a)
    where
      array_slice a = newNode"slice"$ fmap (a :) p
      p = seQ [ op "[", option_expr, op "]" ]
          <|> seQ [ op "{", option_expr, op "}" ]


----------------------------------------
arraylen = var_context "$#"
scalar   = var_context "$"
star     = var_context "*"
hash     = var_context "%"
array    = var_context "@"
func     = var_context "&"

var_context :: String -> Perl5Parser Node
var_context s = newNode s $ pcons
                              (try$ operator_node s)
                              (toNodes Perl5Parser.Token.p_Ident <|> seQ [ op "{", option_expr, op "}" ])
