module Perl5Parser.Term
    ( term, func, scalar, after_deref
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Token
import qualified Perl5Parser.Token.Quote
import {-# SOURCE #-} Perl5Parser.Expr
import {-# SOURCE #-} Perl5Parser.Lines


term :: Perl5Parser Node
term = anonymous
       <|> declvar
       <|> grouped

       <|> arraylen -- before scalar
       <|> star
       <|> hash
       <|> scalar_maybe_subscript
       <|> array_maybe_slice
       <|> fmap Tokens Perl5Parser.Token.p_Token


grouped = newNode"grouped" (seQ [ paren_option_expr, option [] paren_next_slice ])

-- | (...)[...]
paren_next_slice = squareB_option_expr


-- | $z $z[...] $z{...}
scalar_maybe_subscript = do a <- scalar
                            option a (scalar_subscript a)
    where
      scalar_subscript a = newNode"subscript"$ fmap (a :) p
      p = squareB_option_expr <|> curlyB_option_expr

-- | @z @z[...] @z{...}
array_maybe_slice = do a <- array
                       option a (array_slice a)
    where
      array_slice a = newNode"slice"$ fmap (a :) p
      p = squareB_option_expr <|> curlyB_option_expr


-- | Constructors for anonymous data
anonymous =     newNode"[]" squareB_option_expr
            <|> newNode"{}" curlyB_option_expr
            <|> anonymous_sub

declvar = newNode"declvar"$ pcons (any_symbol_node [ "my", "our", "local" ]) lexpr


----------------------------------------
after_deref :: Perl5Parser [Node]
after_deref = squareB_option_expr 
              <|> curlyB_option_expr
              <|> toList grouped
              <|> pcons method (option [] paren_option_expr)
    where method = scalar
                   <|> fmap Tokens Perl5Parser.Token.p_Ident 
                   <|> fmap Tokens (toList Perl5Parser.Token.Quote.p_Double)
                   <|> fmap Tokens (toList Perl5Parser.Token.Quote.p_Single)

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
                              (toNodes Perl5Parser.Token.p_Ident <|> curlyB_option_expr)
