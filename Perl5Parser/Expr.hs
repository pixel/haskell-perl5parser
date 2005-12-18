module Perl5Parser.Expr
    ( lexpr, expr, paren_expr, paren_option_expr, option_expr
    ) where

import List (partition, concatMap, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Perl5Parser.Common
import Perl5Parser.Types
import Perl5Parser.Serialize
import Perl5Parser.ParserHelper
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
 , (infixNone , 19, [ "=", "+=", "-=", "*=", ".=", "|=", "&=", "^=", "||=", "&&=", "//=", "**=", "%=", "x=", "<<=", ">>=", ">=", "<=" ])
 , (infixLeft , 20, [ ",", "=>" ])
-- normal function call
 , (Prefix    , 22, [ "not" ])
 , (infixLeft , 23, [ "and" ])
 , (infixLeft , 24, [ "or", "xor" ])
 ]
-- "-A", "-B", "-C", "-M", "-O", "-R", "-S", "-T", "-W", "-X", "-b", "-c", "-d", "-e", "-f", "-g", "-k", "-l", "-o", "-p", "-r", "-s", "-t", "-u", "-w", "-x", "-z"


op = toList . operator_node
operator' s = if isWordAny (last s) then symbol_node s else try $ operator_node s
operator_to_parser (i, prio, op) = fmap (\s -> (i, prio, (s,op))) (operator' op)

data ZZ = ZZ { z_op :: NodeName
             , z_left :: Maybe ZZ
             , _z_middle :: [Node]
             , z_right :: Maybe ZZ
             , z_priority :: Integer
             , z_associativity :: AssocType
             , z_question_opened :: Integer
             }

show_long_ZZ = False

instance Show ZZ where
    show (ZZ (NodeName"") Nothing middle Nothing _ _ _) = verbatim middle
    show (ZZ op left middle right prio asso question_opened) =
        "ZZ{ op = " ++ show op ++ ", left = " ++ show left ++ ", middle = " ++ verbatim middle ++ ", right = " ++ show right ++ 
                    if show_long_ZZ then ", priority = " ++ show prio ++ ", asso = " ++ show asso ++ ", question_opened = " ++ show question_opened ++ "}"
                       else "}"

get_prototype :: String -> Perl5Parser (Maybe String)
get_prototype f = do state <- getState
                     return$ Map.lookup f (prototypes state)

preParsers :: [ Perl5Parser (OpType, Integer, (Node, String)) ]
(preParsers, postParsers) = 
    map_t2 (map operator_to_parser) $ partition (\(i, _, _) -> i == Prefix) $ long_first $ flatten operators
    where      
      long_first = sortBy (\ (_, _, op1) (_, _, op2) -> compare (length op2) (length op1))
      flatten = concatMap (\(i, prio, ops) -> map (\op -> (i, prio, op)) ops)


paren_expr = seQ [ op "(", lexpr, op ")" ]
paren_option_expr = seQ [ op "(", option_expr, op ")" ]

option_expr :: Perl5Parser [Node]
option_expr = option [] lexpr

lexpr :: Perl5Parser [Node]
lexpr = toList expr

expr :: Perl5Parser Node
expr = newNode"expr"$ fmap reduce expr_
    where
      expr_ :: Perl5Parser ZZ
      expr_ = do e <- term_with_pre
                 fold_many middle e

      term_with_pre = 
                  term_with_pre_op
              <|> ampersand_call
              <|> fmap toZZ (toList term)
              <|> bareword_call

      term_with_pre_op = do l <- choice preParsers
                            t <- term_with_pre
                            get_middle (add_pre (toZZ_ l) t)

      ampersand_call = do f <- func
                          call_paren f <|> to_call_no_para f

      bareword_call = do f <- word_raw
                         s <- spaces_comments
                         let e = Tokens (Word f : s)
                         call_paren e <|> bareword_call_proto f e

      call_paren :: Node -> Perl5Parser ZZ
      call_paren f = do l <- newNode"paren_option_expr"$ paren_option_expr
                        to_call f prio_max (toZZ [l])

      bareword_call_proto :: String -> Node -> Perl5Parser ZZ
      bareword_call_proto f e = 
          do proto <- get_prototype f
             choice (choices proto)
          where
            choices proto = (if max > 0 then [with_para] else [])
                            ++ (if min == 0 then [no_para] else [])
                where
                  (min, max) = fromMaybe (0, 99) (proto >>= parse_prototype)
                  prio = if max == 1 then prio_named_unary else prio_normal_call

                  no_para = to_call_no_para e

                  with_para = do b <- block
                                 with_block_para b
                              <|> do t <- term_with_pre
                                     to_call e prio t

                  with_block_para b = do lookAhead (satisfy (/= ','))
                                         t <- term_with_pre -- ^ map { ... } @foo
                                         return$ ZZ (NodeName"call") Nothing (e : b) (Just t) prio AssocNone 0
                                  <|> do to_call e prio (toZZ b) -- ^ END { ...}  or  f { a => 1 }, ...

      to_call_no_para f =
          let call = ZZ (NodeName"call") Nothing [f] Nothing prio_max AssocNone 0 in
          return call

      to_call f prio child = 
          let call = ZZ (NodeName"call") Nothing [f] Nothing prio AssocNone 0 in
          get_middle (add_pre call child)

      toZZ l = ZZ (NodeName"") Nothing l Nothing prio_max AssocNone 0
      toZZ_ (fixity, prio, (l,s)) = ZZ (NodeName s) Nothing [l] Nothing prio (fixity_to_associativity fixity) 0

      get_middle z =
          do z' <- middle z
             seq (show4debug"get_middle" (z, z')) $ if z_priority z' == z_priority z then get_middle z' else return z'
          <|> return z
                 
      middle e = 
          let postParsers' = if z_question_opened e > 0 then postParsers ++ [ operator_to_parser (infixRight, 18, ":") ] else postParsers in
          do op@(fixity, _prio, (_l,s)) <- choice postParsers'
             return$ show4debug"middle found" s
             t <- if fixity == Postfix then return Nothing else fmap Just term_with_pre
             let question_opened' = z_question_opened e + (case s of { "?" -> 1; ":" -> -1; _ -> 0 })
             return (add_maybe e (toZZ_ op) t) { z_question_opened = question_opened' }

      reduce :: ZZ -> [Node]
      reduce e = seq (show4debug"to_l" e) $ show4debug"to_l gives"$ to_l e
          where
            maybe_reduce = maybe [] reduce

            to_l :: ZZ -> [Node]
            to_l (ZZ _ Nothing middle Nothing _ _ _) = middle
            to_l (ZZ (NodeName ":") (Just (ZZ (NodeName "?") left op1 middle_para _ _ _)) op2 right _ _ _) = 
                [ Call(NodeName "?:", maybe [] reduce left ++ op1 ++ maybe_reduce middle_para ++ op2 ++ maybe_reduce right) ]

{-
                -- here we have a left-hand comb with either "?" or ":"
                -- ":" + "?" must be combinated and given to deeper "?"
                where combinate (ZZ (NodeName ":") (ZZ (NodeName "?") left op1 middle_para _ _ _) op2 right _ _ _) =
                          
                case left of
                  Just (ZZ (NodeName "?") left middle2 middle_right _ _ _) ->
                      case to_l left of
                      to_l (left' { z_op = NodeName "?:", z_left = (middle' ++ left_right ++ middle) (to_l right)
-}
            to_l (ZZ op left middle right _ _ _) = [ Call(op, maybe_reduce left ++ middle ++ maybe_reduce right) ]
--                maybe [] to_l left ++ [(op, middle ++ maybe [] reduce right)]

--            reduce_l left l@((NodeName"?", _) : _) = reduce_l left (group left l)
--                where group left ((NodeName"?", l) : sub) =
--                          case show4debug"XXX" $ group sub of 
--                            ((NodeName"?", l2) : (NodeName ":", l3) : sub3) -> 
--                                let n = [ Call(NodeName"?:", l ++ l2 ++ l3) ] in
--                                (NodeName":", n) : sub3
--                            ((NodeName":", l2) : sub3) -> 
--                                (NodeName"?:", l ++ l2) : sub3
--                            _ -> error "missing \":\""
--                      group [ (NodeName"?", _) ] = error "missing \":\""
--                      group z = z
--            reduce_l accu [] = accu
--            reduce_l [] ((NodeName"", l) : sub) = reduce_l l sub
--            reduce_l left ((op, l) : sub) = reduce_l [Call(op, left ++ l)] sub

      add_maybe :: ZZ -> ZZ -> Maybe ZZ -> ZZ
      add_maybe left op Nothing = add_post left op
      add_maybe left op (Just right) = add left op right

      add :: ZZ -> ZZ -> ZZ -> ZZ
      add left op right = seq (show4debug "add" (left, op, right)) $ show4debug "add returns" $
          if z_priority left == z_priority op && z_associativity op == AssocNone then
              error$ show (z_op left) ++ " is non associative"
          else if z_priority left < z_priority op || z_priority left == z_priority op && z_associativity op == AssocLeft then
              add_pre (op { z_left = Just left }) right
          else if z_priority right < z_priority op || z_priority right == z_priority op && z_associativity op == AssocRight then
              add_post left (op { z_right = Just right })
          else
              -- | here we know for sure that op is no better than left and right
              -- we must find out wether we prefer left or right
              if z_priority left < z_priority right || z_priority left == z_priority right && z_associativity right == AssocLeft then
                  right { z_left = Just(add left op (fromJust$ z_left right)) }
              else
                  left { z_right = Just(add (fromJust$ z_right left) op right) }

      add_pre op right = -- ^ here we know that (z_right op) is Nothing
        seq (show4debug"add_pre"(op,right)) $
          if z_priority op < z_priority right then
              case z_left right of
                Nothing -> op { z_right = Just right } -- eg: op = "1 +" and right is "not 2"
                Just right' -> right { z_left = Just (add_pre op right') }
          else 
              op { z_right = Just right }

      add_post left op = -- ^ here we know that (z_left op) is Nothing
        seq (show4debug"add_post"(left,op)) $
          if z_priority op < z_priority left || z_priority op == z_priority left && z_associativity op == AssocRight then
              left { z_right = Just (add_post (fromJust$ z_right left) op) }
          else 
              op { z_left = Just left }
