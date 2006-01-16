module Perl5Parser.Term
    ( term, func, scalar, after_deref, subattrlist, decl_variable
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
       <|> star_maybe_subscript
       <|> hash
       <|> scalar_maybe_subscript
       <|> array_maybe_slice
       <|> fmap Tokens Perl5Parser.Token.p_Token


grouped = newNode"grouped" (seQ [ paren_option_expr, option [] paren_next_slice ])

-- | (...)[...]
paren_next_slice = squareB_option_expr


-- | $z $z[...] $z{...}
scalar_maybe_subscript = do a <- scalar
                            option a (newNode"subscript"$ fmap (a :) subscript)

-- | $z $z[...] $z{...}
star_maybe_subscript = do a <- star
                          option a (newNode"subscript"$ fmap (a :) subscript)

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

subscript = seQ [ squareB_option_expr <|> curlyB_option_expr
                , manY simple_subscript
                ]

simple_subscript = squareB_option_expr 
                   <|> curlyB_option_expr
                   <|> toList grouped


----------------------------------------
decl_variable :: Perl5Parser [Node]
decl_variable = pcons (decl_grouped <|> var) subattrlist 

subattrlist = toNodes $ manY (seQ [ operator ":", word ])

var = star <|> hash <|> scalar <|> array <|> fmap (\e -> Tokens [Word e]) (try_string "undef")

op = toList . operator_node
decl_grouped = newNode "grouped"$ seQ [ op "(", list, op ")" ]
    where list = pcons var (manY (seQ [ op ",", option [] (toList var) ]))          

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

op_no_space s = try_string s >> return (Tokens [ Operator s ])

arraylen = var_context "$#" (return []) []
scalar   = var_context "$" spaces_comments magic_scalars
star     = var_context "*" spaces_comments magic_scalars
hash     = var_context "%" spaces_comments magic_hashes
array    = var_context "@" spaces_comments magic_arrays
func     = var_context_ "&" (try one_ampersand_only) spaces_comments []
    -- | ugly special case to handle "eval {} && ...", so here we accept only one ampersand
    where one_ampersand_only = try$ do s <- op_no_space "&"
                                       notFollowedBy (char '&')
                                       return s

var_context :: String -> Perl5Parser [TokenT] -> [String] -> Perl5Parser Node
var_context s between = var_context_ s (op_no_space s) between
    
var_context_ :: String -> Perl5Parser Node -> Perl5Parser [TokenT] -> [String] -> Perl5Parser Node
var_context_ s p between l_magics = 
    do pval <- p
       bval <- between
       l <- var_context_after s <|> if has_comment bval then pzero else magics
       newNode s $ return (pval : Tokens bval : l) -- ^ do magics after var_context_after to handle $:: vs $:
    where
      magics = do magic <- choice (map try_string l_magics)
                  l <- spaces_comments
                  return [Tokens $ Word magic : l]

      has_comment = any is_comment
      is_comment (Comment _) = True
      is_comment _ = False


var_context_after :: String -> Perl5Parser [Node]
var_context_after s = do dollars <- many (op_no_space "$")
                         fmap (\l -> dollars ++ l) after_end <|> catch_magic_PID s dollars
    where after_end = curlyB_option_expr_special
                      <|> toNodes Perl5Parser.Token.p_Ident_sure
                      <|> toNodes (pcons (fmap Word $ many1 digit) spaces_comments)
          catch_magic_PID s dollars = 
              if (s == "$" || s == "*") && length dollars > 0 then
                  do sp <- spaces_comments
                     return$ tail dollars ++ [Tokens (Word "$" : sp)]
              else pzero

magic_scalars = [ "&", "`", "'", "+", "*", ".", "/", "|", "\\", "\"", ";", "%", "=", "-", ")", "#", "~", ":", "?", "!", "@", "$", "<", ">", "(", "0", "[", "]", "}", ",", "#+", "#-", "^L", "^A", "^E", "^C", "^D", "^F", "^H", "^I", "^M", "^N", "^O", "^P", "^R", "^S", "^T", "^V", "^W", "^X", "^" ]
magic_arrays = [ "+", "-", "*" ]
magic_hashes = [ "!" ]
