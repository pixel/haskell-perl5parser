module Perl5Parser.Term
    ( term, func
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Token
import {-# SOURCE #-} Perl5Parser.Expr
import {-# SOURCE #-} Perl5Parser.Lines


term = anonymous
       <|> declvar
       <|> newNode"grouped" (seQ [ paren_option_expr, option [] paren_next_slice ])

       <|> arraylen -- before scalar
       <|> scalar
       <|> star
       <|> hash
       <|> array_maybe_slice
       <|> Perl5Parser.Token.p_Token


-- | Constructors for anonymous data
anonymous =     newNode"[]" (seQ [ operator "[", option_expr, operator "]" ])
            <|> newNode"{}" (seQ [ operator "{", option_expr, operator "}" ])
            <|> anonymous_sub

declvar = newNode"declvar"$ seQ 
          [ any_symbol [ "my", "our", "local" ]
          , lexpr
          ]

paren_next_slice = seQ [ operator "[", option_expr, operator "]" ]

array_maybe_slice = do a <- array
                       option a (array_slice a)
    where
      array_slice a = newNode"slice"$ fmap (a :) p
      p = seQ [ operator "[", option_expr, operator "]" ]
          <|> seQ [ operator "{", option_expr, operator "}" ]

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
var_context s = newNode s $ seQ 
                [ try$ operator s
                , word <|> seQ [ operator "{", option_expr, operator "}" ]
                ]
