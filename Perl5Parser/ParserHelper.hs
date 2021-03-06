module Perl5Parser.ParserHelper
    ( many, many1, eof, (<|>), (<?>), option, try, char, string, digit, pzero
    , anyChar, oneOf, updateState, satisfy, isAlpha, getState, choice, lookAhead, notFollowedBy_
    -- ^ above are re-exported
    --
    , show4debug_pretty
    , toList, onList, pcons, p_t2, seQ, manY, manyl, fold_many, lineBegin, toMaybe
    , anyTill, parse
    , isWordAny, isDigit_, isAlpha_, isSpace, balancedDelim, infix_cmds, keywords
    , charl, oneOfl, try_string
    , notWord, wordAny, digit_
    , word_raw, comment, spaces_no_nl, spaces, spaces_comments_normal, endWord
    --
    , spaces_token, word_raw_token, spaces_comments, spaces_comments_with_here_doc, with_spaces_comments, with_spaces_comments_
    , word, symbol, any_symbol, operator
    --
    , operator_node, symbol_node, any_symbol_node, word_node, newNode
    ) where

import Data.Char (isAlphaNum, isDigit, isAlpha, isSpace)
import Text.ParserCombinators.Parsec (oneOf, GenParser, CharParser, (<?>), many, many1,
    , satisfy, space, try, choice, (<|>), anyChar, eof, lookAhead, unexpected
    , option, getState, updateState, runParser, char, string, digit, getPosition, pzero, newline)
import Text.ParserCombinators.Parsec.Pos (sourceColumn)

import System.IO.Unsafe (unsafePerformIO)
import qualified Perl5Parser.Serialize

import Perl5Parser.Common
import Perl5Parser.Types

show_debug = True
debug s e = if show_debug then show4debug s e else e

show4debug_pretty :: Perl5Parser.Serialize.Serialize a => String -> a -> a
show4debug_pretty s e = seq (unsafePerformIO $ putStrLn (s ++ ": " ++ Perl5Parser.Serialize.with_parentheses e)) e


toList :: GenParser tok st a -> GenParser tok st [a]
toList = fmap (\c -> [c])

onList :: ([a] -> b) -> GenParser tok st [a] -> GenParser tok st [b]
onList f = fmap (map_non_empty_list f)

pcons :: GenParser tok st a -> GenParser tok st [a] -> GenParser tok st [a]
pcons li lis = do l <- li
                  ls <- lis
                  return$ l : ls

p_t2 :: GenParser tok st a -> GenParser tok st b -> GenParser tok st (a, b)
p_t2 p1 p2 = do v1 <- p1
                v2 <- p2
                return (v1, v2)

seQ :: [GenParser tok st [a]] -> GenParser tok st [a]
seQ = fmap concat . sequence

manY :: GenParser tok st [a] -> GenParser tok st [a]
manY = fmap concat . many
--manY1 = fmap concat . many1

manyl :: Eq a => GenParser tok st a -> GenParser tok st [[a]]
manyl = onList id . many

fold_many :: (a -> GenParser tok st a) -> a -> GenParser tok st a
fold_many p accu = option accu (p accu >>= fold_many p)

-- | only run the parser if we are at column 1
lineBegin :: GenParser tok st a -> GenParser tok st a
lineBegin p = do pos <- getPosition
                 if sourceColumn pos == 1 then p else pzero

toMaybe :: GenParser tok st a -> GenParser tok st (Maybe a)
toMaybe p = fmap Just p <|> return Nothing


notFollowedBy_ :: CharParser st b -> CharParser st a -> CharParser st a
notFollowedBy_ p q = try $ do r <- q
                              (p >> unexpected "") <|> return r

-- | must be able to handle BIG strings
anyTill :: CharParser st String -> CharParser st String
anyTill p = scan ""
    where
      scan s = fmap (\wanted -> reverse s ++ wanted) p
               <|>
               do{ x <- anyChar; scan (x : s) }


parse :: CharParser st a -> st -> String -> String -> a
parse p init_state file_name input = 
    case runParser p init_state file_name input of
      Left err -> error$ "parse error at " ++ show err
      Right x  -> x


-- boolean predicates
isWordAny   :: Char -> Bool
isWordAny x = (isAlphaNum x || x == '_')
isDigit_ x = (isDigit x || x == '_')
isAlpha_ x = (isAlpha x || x == '_')

balancedDelim :: Char -> Maybe Char
balancedDelim c = case c of
                    '(' -> Just ')'
                    ')' -> Just '('
                    '{' -> Just '}'
                    '}' -> Just '{'
                    '<' -> Just '>'
                    '>' -> Just '<'
                    '[' -> Just ']'
                    ']' -> Just '['
                    _   -> Nothing

infix_cmds = [ "if", "unless", "while", "until", "for", "foreach" ]
keywords = infix_cmds ++ [ "or", "and" ]


-- char parser constructors
charl :: Char -> CharParser st String
charl = toList . char

oneOfl :: [Char] -> CharParser st String
oneOfl = toList . oneOf

try_string :: String -> CharParser st String
try_string = try . string


-- char parsers
notWord :: CharParser st Char
notWord = satisfy (not . isWordAny)

wordAny = satisfy isWordAny
digit_ = satisfy isDigit_ <?> "digit"


-- string parsers
word_raw = do { d <- satisfy isAlpha_; l <- many wordAny; return$ d : l }

comment :: CharParser st String
comment = seQ [ charl '#', many (satisfy (/= '\n')) ]

spaces_no_nl :: CharParser st [String]
spaces_no_nl = manyl (oneOf " \t")

spaces :: CharParser st [String]
spaces = manyl space <?> ""


-- | fail if after running p, we are not at the end of a word
endWord :: CharParser st a -> CharParser st a
endWord = notFollowedBy_ (satisfy isWordAny)


word_raw_token = fmap Word word_raw
spaces_token = fmap (map Whitespace) spaces
comment_token = fmap Comment comment

spaces_comments_normal :: CharParser st [SpaceCommentT]
spaces_comments_normal = seQ [ spaces_token, manY $ seQ [ toList comment_token, spaces_token ] ]


spaces_comments :: Perl5Parser [SpaceCommentT]
spaces_comments = do state <- getState
                     case next_line_is_here_doc state of
                       Nothing -> spaces_comments_normal
                       Just limit -> spaces_comments_with_here_doc limit

spaces_comments_with_here_doc :: String -> Perl5Parser [SpaceCommentT]
spaces_comments_with_here_doc limit = do l <- fmap(map Whitespace) spaces_no_nl
                                         l2 <- option [] (toList comment_token)
                                         l3 <- option [] get_here_doc
                                         return$ l ++ l2 ++ l3
    where get_here_doc = do lookAhead newline
                            updateState (\s -> s { next_line_is_here_doc = Nothing })
                            here_doc <- anyTill (try_string ("\n" ++ limit ++ "\n"))
                            l2 <- spaces_comments_normal
                            return$ [HereDocValue here_doc] ++ l2

with_spaces_comments :: Perl5Parser TokenT -> Perl5Parser (TokenT, [SpaceCommentT])
with_spaces_comments p = p_t2 p spaces_comments
                         
word :: Perl5Parser (TokenT, [SpaceCommentT])
word = with_spaces_comments word_raw_token

symbol :: String -> Perl5Parser (TokenT, [SpaceCommentT])
symbol s = with_spaces_comments (fmap Symbol $ endWord (string s))

any_symbol :: [String] -> Perl5Parser (TokenT, [SpaceCommentT])
any_symbol = choice . map symbol

operator :: String -> Perl5Parser (TokenT, [SpaceCommentT])
operator s = with_spaces_comments (fmap Operator (try_string s))


--
--seq_toTokens :: [Perl5Parser String] -> Perl5Parser [Node]
--seq_toTokens = fmap (map Token) . sequence

with_spaces_comments_ = fmap Token . with_spaces_comments

operator_node s = fmap Token $ operator s
symbol_node s = fmap Token $ symbol s
any_symbol_node l = fmap Token $ any_symbol l
word_node = fmap Token word

newNode :: String -> Perl5Parser [Node] -> Perl5Parser Node
newNode s r = fmap (\l -> debug "newNode: " (Node(NodeName s, l))) r <?> s
