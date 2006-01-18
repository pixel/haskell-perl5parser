module Perl5Parser.Expr
    ( lexpr, expr, paren_expr, paren_option_expr, option_expr
    , curlyB_option_expr, curlyB_option_expr_special, squareB_option_expr
    ) where

import List (partition, concatMap, sortBy)
import Data.Maybe (fromJust)

import Perl5Parser.Common
import Perl5Parser.Types
import Perl5Parser.Serialize
import Perl5Parser.ParserHelper
import qualified Perl5Parser.Env as Env
import qualified Perl5Parser.Token
import Perl5Parser.Term
import Perl5Parser.Prototype
import {-# SOURCE #-} Perl5Parser.Lines

data AssocType = AssocRight | AssocLeft | AssocNone deriving (Eq, Show)
data OpType = Infix AssocType | Prefix | Postfix deriving (Eq, Show)

fixity_to_associativity Prefix = AssocRight
fixity_to_associativity Postfix = AssocLeft
fixity_to_associativity (Infix associativity) = associativity

infixRight = Infix AssocRight
infixLeft  = Infix AssocLeft
infixNone  = Infix AssocNone

prio_max = 1
prio_named_unary = 10
prio_normal_call = 21

operators :: [ (OpType, Integer, [String]) ]
operators = 
-- term
 [ (infixRight,  2, [ "->" ])
 , (Prefix    ,  3, [ "++", "--" ])
 , (Postfix   ,  3, [ "++", "--" ])
 , (infixRight,  4, [ "**" ])
 , (Prefix    ,  5, [ "!", "~", "\\", "+", "-" ])
 , (infixLeft ,  6, [ "=~", "!~" ])
 , (infixLeft ,  7, [ "*", "/", "x", "%" ])
 , (infixLeft ,  8, [ "+", "-", "." ])
 , (infixLeft ,  9, [ "<<", ">>" ])
-- named unary op    
 , (infixNone , 11, [ "<", ">", "<=", ">=", "lt", "gt", "le", "ge" ])
 , (infixNone , 12, [ "==", "!=", "<=>", "eq", "ne", "cmp" ])
 , (infixLeft , 13, [ "&" ])
 , (infixLeft , 14, [ "|", "^" ])
 , (infixLeft , 15, [ "&&" ])
 , (infixLeft , 16, [ "||" ])
 , (infixNone , 17, [ "..", "..." ])
 , (infixRight, 18, [ "?" ])
 , (infixRight, 19, [ "=", "+=", "-=", "*=", "/=", ".=", "|=", "&=", "^=", "||=", "&&=", "//=", "**=", "%=", "x=", "<<=", ">>=", ">=", "<=" ])
 , (infixLeft , 20, [ ",", "=>" ])
-- normal function call
 , (Prefix    , 22, [ "not" ])
 , (infixLeft , 23, [ "and" ])
 , (infixLeft , 24, [ "or", "xor" ])
 ]
-- "-A", "-B", "-C", "-M", "-O", "-R", "-S", "-T", "-W", "-X", "-b", "-c", "-d", "-e", "-f", "-g", "-k", "-l", "-o", "-p", "-r", "-s", "-t", "-u", "-w", "-x", "-z"

fmap_maybe _ Nothing = return Nothing
fmap_maybe f (Just e) = fmap Just (f e)

elem_local (LocalIdent, i) l = elem i l
elem_local _ _ = False

op = toList . operator_node
operator' s = if s == "x" then fmap Tokens (pcons (fmap Symbol (string s)) spaces_comments_token)
              else if isWordAny (last s) then symbol_node s else try $ operator_node s
operator_to_parser (i, prio, op) = fmap (\s -> (i, prio, (s,op))) (operator' op)

data ZZ = ZZ { z_op :: NodeName
             , z_left :: Maybe ZZ
             , z_middle :: [Node]
             , z_right :: Maybe ZZ
             , z_priority :: Integer
             , z_associativity :: AssocType
             , z_question_opened :: Integer
             }

show_long_ZZ = False
show_debug = False
debug s e = if show_debug then show4debug s e else e

instance Show ZZ where
    show (ZZ (NodeName"") Nothing middle Nothing _ _ _) = verbatim middle
    show (ZZ op left middle right prio asso question_opened) =
        "ZZ{ op = " ++ show op ++ ", left = " ++ show left ++ ", middle = " ++ verbatim middle ++ ", right = " ++ show right ++ 
                    if show_long_ZZ then ", priority = " ++ show prio ++ ", asso = " ++ show asso ++ ", question_opened = " ++ show question_opened ++ "}"
                       else "}"

preParsers :: [ Perl5Parser (OpType, Integer, (Node, String)) ]
(preParsers, postParsers) = 
    map_t2 (map operator_to_parser) $ partition (\(i, _, _) -> i == Prefix) $ long_first $ flatten operators
    where      
      long_first = sortBy (\ (_, _, op1) (_, _, op2) -> compare (length op2) (length op1))
      flatten = concatMap (\(i, prio, ops) -> map (\op -> (i, prio, op)) ops)


paren_expr = seQ [ op "(", lexpr, op ")" ]
paren_option_expr = seQ [ op "(", option_expr, op ")" ]

squareB_option_expr = seQ [ op "[", option_expr, op "]" ]

-- | we need to handle ->{aa} especially so that aa is not understood as being a function
curlyB_option_expr = do open <- op "{"
                        l <- try (pcons word_node (op "}")) <|> seQ [ option_expr, op "}" ]
                        return (open ++ l)
 
-- | mostly similar to curlyB_option_expr, but we need to handle {^XXX}, 
curlyB_option_expr_special =
    do open <- op "{"
       l <- pcons (fmap (\e -> Tokens [ Word e ]) (pcons (char '^') word_raw)) (op "}")
            <|> try (pcons word_node (op "}")) 
            <|> seQ [ option_expr, op "}" ]
       return (open ++ l)

option_expr :: Perl5Parser [Node]
option_expr = option [] lexpr

lexpr :: Perl5Parser [Node]
lexpr = toList expr

expr :: Perl5Parser Node
expr = newNode"expr"$ expr_ >>= reduce
    where
      expr_ :: Perl5Parser ZZ
      expr_ = do e <- term_with_pre
                 fold_many middle e

      term_with_pre = 
                  filetest_call -- do it before term_with_pre_op other "-" is matching unary op
              <|> term_with_pre_op
              <|> ampersand_call
              <|> fmap toZZ (toList term)
              <|> bareword_call

      term_with_pre_op = do l <- choice preParsers
                            t <- term_with_pre
                            get_middle (add_pre (toZZ_ l) t)

      ampersand_call = do f <- func
                          call_paren [f] <|> to_call_no_para [f]

      filetest_call = do f <- Perl5Parser.Token.p_Filetest_raw
                         s <- spaces_comments_token
                         let e = Tokens (Ident LocalIdent f : s)
                         bareword_call_proto (LocalIdent, f) [e]

      get_bareword = try$ do f@(l, i) <- Perl5Parser.Token.p_Ident_raw
                             s <- spaces_comments_token
                             let e = Tokens (Ident l i : s)
                             dont_keep_bareword <- fmap isNothing $ toMaybe $ lookAhead (try_string "->" <|> try_string "=>")
                             if dont_keep_bareword && elem_local f keywords 
                               then pzero 
                               else return (f, e, dont_keep_bareword)

      bareword_call = do (f, e, dont_keep_bareword) <- get_bareword
                         if not dont_keep_bareword 
                           then return (toZZ [e]) -- ^ simply return this word (useful for class->new and (xxx => ...)
                           else call_var_decl f [e] <|> call_print f [e] <|> may_call_paren f [e] <|> bareword_call_proto f [e]
          where may_call_paren f e = if elem_local f ["return"] then pzero else call_paren e
                -- ^ allow: return ($v)[$w]

      call_var_decl f e = if elem_local f ["my", "our"] then call_var_decl_ else pzero
          where call_var_decl_ = do v <- decl_variable
                                    to_call e prio_named_unary (toZZ v)

      call_paren :: [Node] -> Perl5Parser ZZ
      call_paren e = do l <- newNode"paren_option_expr"$ paren_option_expr
                        to_call e prio_max (toZZ [l])

      call_print f@(_, i) e = if elem_local f ["print", "printf"]
                           then call_print_paren <|> call_print_ 
                           else pzero
          where call_print_paren = 
                    do l <- seQ [ op "(", call_print_paren_get_fh, option_expr, op ")" ]
                       to_call e prio_max (toZZ l)
                call_print_paren_get_fh = 
                    do z <- lookAhead option_expr
                       has_file_handle' z >>= get_filehandle
                                      
                call_print_ =
                    do z <- lookAhead (bareword_call_proto f e)
                       has <- has_file_handle z
                       o_fh <- get_filehandle has
                       bareword_call_proto f (e ++ o_fh)

                get_filehandle True = toNodes Perl5Parser.Token.p_Ident <|> toList scalar
                get_filehandle False = return []

                has_file_handle (ZZ (NodeName"") Nothing [Call (NodeName"call", Tokens (Ident LocalIdent i' : _) : para)] Nothing _ _ _) | i == i' = is_filehandle para
                has_file_handle (ZZ (NodeName"call") Nothing [Tokens [Ident LocalIdent i']] Nothing _ _ _) | i == i' = return False
                has_file_handle z = show4debug "call_print, weird" z `seq` return False

                has_file_handle' [Node(NodeName"expr", e)] = is_filehandle e
                has_file_handle' _ = return False

                is_filehandle (Node(NodeName"$", _) : _) = return True
                is_filehandle (Call (NodeName"<<", (Node(NodeName"$", _) : _)) : _) = return True
                is_filehandle (Call (NodeName"call", (Tokens(Ident l s : _) : _)) : _) = fmap isNothing (Env.get_prototype (l, s))
                is_filehandle _ = return False

      bareword_call_proto :: (IdentT, String) -> [Node] -> Perl5Parser ZZ
      bareword_call_proto f e = 
          do proto <- Env.get_prototype f
             special_ proto <|> normal_choices proto
          where

            -- | in case a bareword is not known to be a function, we don't allow it to take one of [ /re/, <fh> ] as argument (but still allow <<EOF)
            special_ proto =
                do if isNothing proto then lookAhead (char '/' <|> notFollowedBy_ (char '<') (char '<')) else pzero
                   no_para proto

            no_para proto = if isNothing proto then return (toZZ e) else to_call_no_para e

            normal_choices proto = choice ((if max > 0 then [with_para] else [])
                                           ++ (if min == 0 then [no_para proto] else []))
                where
                  (min, max) = fromMaybe (0, 99) (proto >>= parse_prototype)
                  prio = if max == 1 then prio_named_unary else prio_normal_call

                  with_para = do b <- block
                                 if max == 1 then to_call e prio (toZZ b) else with_block_para b
                              <|> do t <- term_with_pre
                                     to_call e prio t

                  with_block_para b = do t <- term_with_pre -- ^ map { ... } @foo
                                         to_call (e ++ b) prio t
                                  <|> do to_call e prio (toZZ b) -- ^ for functions we don't have the prototype, which can be either "f { ...} expr"  or "f { ... }, expr"

      to_call_no_para f =
          let call = ZZ (NodeName"call") Nothing f Nothing prio_max AssocNone 0 in
          return call

      to_call f prio child = 
          let call = ZZ (NodeName"call") Nothing f Nothing prio AssocNone 0 in
          get_middle (add_pre call child)

      toZZ l = ZZ (NodeName"") Nothing l Nothing prio_max AssocNone 0
      toZZ_ (fixity, prio, (l,s)) = ZZ (NodeName s) Nothing [l] Nothing prio (fixity_to_associativity fixity) 0

      get_middle z = do z' <- middle z
                        seq (debug"get_middle" (z, z')) $ if z_priority z' == z_priority z then get_middle z' else reduce_inside z'
                     <|> reduce_local z
          where reduce_local z = fmap toZZ (reduce z)
                reduce_inside z = do left2 <- fmap_maybe reduce_local (z_left z)
                                     return z { z_left = left2 }
                 
      middle e = 
          let postParsers' = if z_question_opened e > 0 || z_priority e == 19 then postParsers ++ [ operator_to_parser (infixRight, 18, ":") ] else postParsers in
          do op@(fixity, _prio, (_l,s)) <- choice postParsers'
             return$ debug"middle found" s
             t <- if fixity == Postfix then return Nothing 
                  else if s == "->" then fmap (Just . toZZ) after_deref
                  else fmap Just term_with_pre <|> 
                      (if s == "," || s == "=>" then return Nothing else pzero)

             e2 <- if s == ":" then reduce_assign_for_ternary e else return e
             let e3 = add_maybe e2 (toZZ_ op) t
             let question_opened = z_question_opened e3 + (case s of { "?" -> 1; ":" -> -1; _ -> 0 })
             return e3 { z_question_opened = question_opened }

      reduce_assign_for_ternary assign | z_priority assign == 19 =
          case z_left assign of
            Just question@(ZZ (NodeName "?") _ _ _ _ _ _) -> 
                do assign' <- reduce (assign { z_left = z_right question })
                   return $ debug "reduce_assign_for_ternary" $ question { z_right = Just (toZZ assign') }
            _ -> return assign
      reduce_assign_for_ternary e | z_priority e == prio_normal_call =
          do right' <- fmap_maybe reduce_assign_for_ternary (z_right e)
             return e { z_right = right' }
      reduce_assign_for_ternary e = return e

      reduce :: ZZ -> Perl5Parser [Node]
      reduce e = reduce_ e
          where
            maybe_reduce = maybe (return []) reduce

            reduce_ :: ZZ -> Perl5Parser [Node]
            reduce_ (ZZ (NodeName"") Nothing middle Nothing _ _ _) = return middle
            reduce_ z@(ZZ (NodeName "?") _ _ _ _ _ _) = group z >>= reduce_ 
                where
                  group z@(ZZ (NodeName "?") _ op1 (Just right) _ _ _) = 
                      do right2 <- group right
                         case right2 of
                           ZZ (NodeName ":") middle_para op2 (Just ri@(ZZ (NodeName ":") _ _ _ _ _ _)) _ _ _ ->
                            do right2 <- fmap_maybe group (z_left ri)
                               middle <- maybe_reduce middle_para
                               return$ ri { z_left = Just$ z { z_op = NodeName "?:"
                                                             , z_middle = op1 ++ middle ++ op2
                                                             , z_right = right2
                                                             } }
                           ZZ (NodeName ":") middle_para op2 right _ _ _ ->
                               do right2 <- fmap_maybe group right
                                  middle <- maybe_reduce middle_para
                                  return$ z { 
                                       z_op = NodeName "?:"
                                      , z_middle = op1 ++ middle ++ op2
                                      , z_right = right2
                                      }
                           _ -> fail "missing \":\""
                  group z =
                      do left <- fmap_maybe group (z_left z)
                         right <- fmap_maybe group (z_right z)
                         return z { z_left = left , z_right = right }

            reduce_ (ZZ op left middle right _ _ _) = 
                do left2 <- maybe_reduce left
                   right2 <- maybe_reduce right
                   return [ Call(op, left2 ++ middle ++ right2) ]

      add_maybe :: ZZ -> ZZ -> Maybe ZZ -> ZZ
      add_maybe left op Nothing = add_post left op
      add_maybe left op (Just right) = add left op right

      add :: ZZ -> ZZ -> ZZ -> ZZ
      add left op right = seq (debug "add" (left, op, right)) $ debug "add returns" $
          if z_priority left == z_priority op && z_associativity op == AssocNone then
              error$ show (z_op left) ++ " is non associative"
          else if z_priority left < z_priority op || z_priority left == z_priority op && z_associativity op == AssocLeft then
              add_pre (op { z_left = Just left }) right
          else if z_priority right < z_priority op || z_priority right == z_priority op && z_associativity op == AssocRight then
              add_post left (op { z_right = Just right, z_question_opened = z_question_opened right })
          else
              -- | here we know for sure that op is no better than left and right
              -- we must find out wether we prefer left or right
              if z_priority left < z_priority right || z_priority left == z_priority right && z_associativity right == AssocLeft then
                  right { z_left = Just(add left op (fromJust$ z_left right)) }
              else
                  left { z_right = Just(add (fromJust$ z_right left) op right), z_question_opened = question_opened left right }

      add_pre op right = -- ^ here we know that (z_right op) is Nothing
        seq (debug"add_pre"(op,right)) $
          if z_priority op < z_priority right then
              case z_left right of
                Nothing -> op { z_right = Just right } -- eg: op = "1 +" and right is "not 2"
                Just right' -> right { z_left = Just (add_pre op right') }
          else 
              op { z_right = Just right, z_question_opened = question_opened op right }

      add_post left op = -- ^ here we know that (z_left op) is Nothing
        seq (debug"add_post"(left,op)) $
          if z_priority op < z_priority left || z_priority op == z_priority left && z_associativity op == AssocRight then
              left { z_right = Just (add_post (fromJust$ z_right left) op)
                   , z_question_opened = question_opened left op }
          else 
              op { z_left = Just left }

question_opened e1 e2 = z_question_opened e1 + z_question_opened e2
