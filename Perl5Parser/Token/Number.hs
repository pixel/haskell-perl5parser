module Perl5Parser.Token.Number
    ( p_Number
    , p_VersionNumber
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper

import Text.ParserCombinators.Parsec (octDigit, hexDigit)


p_Number :: Perl5Parser String
p_Number =   seQ [ charl '0', zeroNumFloat_ ]
         <|> decimalFloat
         <|> seQ [ fraction_1n, option "" exponent' ]  -- .123
         <?> "Number"
                  
zeroNumFloat_   =  hexadecimal 
                   <|> seQ [ fraction_0n, option "" exponent' ]  -- .  .123
                   <|> octal
                   <|> return "" -- the real 0
                  
-- | 1  1.  1.2  1.E3  1E3
decimalFloat    = seQ [ decimal, option "" fraction_0n, option "" exponent' ]

fraction_0n = seQ [ charl '.', option "" decimal <?> "fraction" ]
fraction_1n = try$ seQ [ charl '.', endWord decimal <?> "fraction" ]

-- | E-1  E22  E+333
exponent'       = seQ [ oneOfl "eE", sign_, endWord decimal <?> "exponent" ]
                  <?> "exponent"

decimal         = do { d <- digit; l <- many digit_; return$ d : l }
octal           = endWord$ many1 octDigit
hexadecimal     = endWord$ seQ [ oneOfl "xX", many1 hexDigit ]

sign_            = option "" $ oneOfl "-+"


--------------------------------------------------------------------------------
p_VersionNumber :: Perl5Parser String
p_VersionNumber = version_number_strict <|> version_number_old

-- | v5  v5.6
version_number_strict = 
    seQ [ try $ seQ [ charl 'v', endWord decimal ]
        -- try needed for va, v5a
        , manY fraction_1n
        ]

-- | 5  5.  5.6  5.6.1
version_number_old 
    = seQ [ endWord decimal
          , option "" (seQ [ charl '.'
                           , option "" (seQ [ endWord decimal
                                            , manY fraction_1n
                                            ])
                           ])
          ]
