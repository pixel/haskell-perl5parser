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
      p = seQ [ squareB_option_expr <|> curlyB_option_expr
              , manY simple_subscript
              ]

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

simple_subscript = squareB_option_expr 
                   <|> curlyB_option_expr
                   <|> toList grouped

----------------------------------------
after_deref :: Perl5Parser [Node]
after_deref = fmap concat (many1 simple_subscript)
              <|> pcons method (option [] paren_option_expr)
    where method = scalar
                   <|> fmap Tokens Perl5Parser.Token.p_Ident 
                   <|> fmap Tokens (toList Perl5Parser.Token.Quote.p_Double)
                   <|> fmap Tokens (toList Perl5Parser.Token.Quote.p_Single)

----------------------------------------
-- E  =  [@%$&*] space* R <|> $# R
-- R  =  $* (ident <|> { expr })

arraylen = var_context "$#" (return []) []
scalar   = var_context "$" spaces_comments magic_scalars
star     = var_context "*" spaces_comments []
hash     = var_context "%" spaces_comments magic_hashes
array    = var_context "@" spaces_comments magic_arrays
func     = var_context_ "&" (pcons (try one_ampersand_only) (toNodes spaces_comments)) []
    -- | ugly special case to handle "eval {} && ...", so here we accept only one ampersand
    where one_ampersand_only = try$ do s <- operator_node "&"
                                       notFollowedBy (char '&')
                                       return s

var_context :: String -> Perl5Parser [TokenT] -> [String] -> Perl5Parser Node
var_context s between = var_context_ s (pcons (try$ operator_node s) (toNodes between))
    
var_context_ :: String -> Perl5Parser [Node] -> [String] -> Perl5Parser Node
var_context_ s p l_magics = newNode s $ seQ [ p, var_context_after <|> magics ] -- ^ do magics after var_context_after to handle $:: vs $:
    where
      magics = do magic <- choice (map try_string l_magics)
                  l <- spaces_comments
                  return [Tokens $ Word magic : l]


var_context_after :: Perl5Parser [Node]
var_context_after = seQ [ many (operator_node "$"), after_end ]
    where after_end = curlyB_option_expr 
                      <|> toNodes Perl5Parser.Token.p_Ident 
                      <|> toNodes (pcons (fmap Word $ many1 digit) spaces_comments)

magic_scalars = [ "&", "`", "'", "+", "*", ".", "/", "|", "\\", "\"", ";", "%", "=", "-", ")", "#", "~", "^", ":", "?", "!", "@", "$", "<", ">", "(", "0", "[", "]", "}", ",", "#+", "#-", "^L", "^A", "^E", "^C", "^D", "^F", "^H", "^I", "^M", "^N", "^O", "^P", "^R", "^S", "^T", "^V", "^W", "^X" ]
magic_arrays = [ "+", "-", "*" ]
magic_hashes = [ "!" ]
