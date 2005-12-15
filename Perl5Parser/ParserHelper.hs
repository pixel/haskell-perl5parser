module Perl5Parser.ParserHelper
    ( many, many1, eof, (<|>), (<?>), option, try, char, string, pzero
    , anyChar, oneOf, updateState, satisfy, isAlpha, getState, choice, lookAhead
    -- ^ above are re-exported
    --
    , toList, pcons, seQ, manY, manyl, fold_many, lineBegin
    , anyTill, parse
    , isWordAny, isDigit_, isAlpha_, isSpace, balancedDelim
    , charl, oneOfl, try_string
    , notWord, wordAny, digit_
    , word_raw, comment, spaces_no_nl, spaces, spaces_comments_normal, endWord
    --
    , spaces_comments, spaces_comments_with_here_doc
    , word, symbol, any_symbol, operator, operator'
    , ident
    --
    , toListToken, toTokens, seq_toTokens, newNode
    ) where

import Data.Char (isAlphaNum, isDigit, isAlpha, isSpace)
import Text.ParserCombinators.Parsec (oneOf, GenParser, CharParser, (<?>), many, many1,
      satisfy, space, try, notFollowedBy, choice, (<|>), anyChar, eof, lookAhead,
      option, getState, updateState, runParser, char, string, getPosition, pzero, newline)
import Text.ParserCombinators.Parsec.Pos (sourceColumn)

import Perl5Parser.Common
import Perl5Parser.Types


toList :: GenParser tok st a -> GenParser tok st [a]
toList = fmap (\c -> [c])

pcons :: GenParser tok st a -> GenParser tok st [a] -> GenParser tok st [a]
pcons li lis = do l <- li
                  ls <- lis
                  return$ l : ls

seQ :: [GenParser tok st [a]] -> GenParser tok st [a]
seQ = fmap concat . sequence

manY :: GenParser tok st [a] -> GenParser tok st [a]
manY = fmap concat . many
--manY1 = fmap concat . many1

manyl :: Eq a => GenParser tok st a -> GenParser tok st [[a]]
manyl = fmap (\c -> if c == [] then [] else [c]) . many

fold_many :: (a -> GenParser tok st a) -> a -> GenParser tok st a
fold_many p accu = option accu (p accu >>= fold_many p)

-- | only run the parser if we are at column 1
lineBegin :: GenParser tok st a -> GenParser tok st a
lineBegin p = do pos <- getPosition
                 if sourceColumn pos == 1 then p else pzero



anyTill :: CharParser st String -> CharParser st String
anyTill p = scan
    where
      scan = p
             <|>
             do{ x <- anyChar; xs <- scan; return (x:xs) }


parse :: CharParser st a -> st -> String -> a
parse p init_state input = 
    case runParser p init_state "" input of
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

spaces_comments_normal :: CharParser st [String]
spaces_comments_normal = seQ [ spaces, manY $ seQ [ toList comment, spaces ] ]

-- | fail if after running p, we are not at the end of a word
endWord :: CharParser st a -> CharParser st a
endWord p = try $ do { r <- p; notFollowedBy (satisfy isWordAny); return r }



spaces_comments :: Perl5Parser [String]
spaces_comments = do state <- getState
                     case next_line_is_here_doc state of
                       Nothing -> spaces_comments_normal
                       Just limit -> spaces_comments_with_here_doc limit

spaces_comments_with_here_doc limit = do l <- spaces_no_nl
                                         l2 <- option [] get_here_doc
                                         return$ l ++ l2
    where get_here_doc = do lookAhead newline
                            updateState (\s -> s { next_line_is_here_doc = Nothing })
                            here_doc <- anyTill (try_string ("\n" ++ limit ++ "\n"))
                            l2 <- spaces_comments_normal
                            return$ [here_doc] ++ l2

                         
word = toTokens (pcons word_raw spaces_comments)

symbol :: String -> Perl5Parser [Node]
symbol s = toTokens$ pcons (endWord (string s)) spaces_comments

any_symbol :: [String] -> Perl5Parser [Node]
any_symbol = choice . map symbol

operator :: String -> Perl5Parser [Node]
operator s = toTokens$ pcons (try_string s) spaces_comments

operator' s = if isWordAny (last s) then symbol s else try $ operator s

-- | ::a  b  c::  d::e
ident = seQ [ option [] (operator "::"), word, manY (seQ [ operator "::", word ]) ]



toListToken :: Perl5Parser String -> Perl5Parser [Node]
toListToken = fmap (\c -> [Token c])

toTokens :: Perl5Parser [String] -> Perl5Parser [Node]
toTokens = fmap (map Token)

seq_toTokens :: [Perl5Parser String] -> Perl5Parser [Node]
seq_toTokens = fmap (map Token) . sequence

newNode :: String -> Perl5Parser [Node] -> Perl5Parser Node
newNode s r = fmap (\l -> show4debug "newNode: " (Node(NodeName s, l))) r <?> s
