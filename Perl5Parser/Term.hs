module Perl5Parser.Term
    ( term, func
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
       <|> newNode"grouped" (seQ [ paren_option_expr, option [] paren_next_slice ])

       <|> arraylen -- before scalar
       <|> scalar
       <|> star
       <|> hash
       <|> array_maybe_slice
       <|> fmap Tokens Perl5Parser.Token.p_Token


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

{-
func_maybe_para = do f <- func
                     option f (para f)
    where
      para f = newNode"call"$ fmap (f :) paren_option_expr

word_maybe_para = do s <- word
                     para s <|> newNode"bareword" (return s)
    where
      para f = newNode"call"$ fmap (f ++) paren_option_expr
-}
----------------------------------------
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
                              (toList word_node <|> seQ [ op "{", option_expr, op "}" ])
