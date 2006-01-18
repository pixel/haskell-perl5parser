module Perl5Parser.Env
    ( get_prototype
    , set_prototype
    , with_new_lexical_block
    , set_package
    ) where

import qualified Monad
import Perl5Parser.Types
import Perl5Parser.ParserHelper

import qualified Data.Map as Map


fq2pkg LocalIdent = getState >>= (return . current_package . env_lexical . env)
fq2pkg fq = return (fq_canonical fq)

get_prototype :: (IdentT, String) -> Perl5Parser (Maybe String)
get_prototype (LocalIdent, f) = do state <- getState
                                   let pkg = current_package (env_lexical (env state))
                                   return$ Monad.mplus (Map.lookup (pkg, f) (prototypes state)) (Map.lookup ("CORE", f) (prototypes state))
get_prototype (fq, f) = do state <- getState
                           return$ Map.lookup (fq_canonical fq, f) (prototypes state)

set_prototype :: (IdentT, String) -> String -> Perl5Parser ()
set_prototype (fq, f) proto = 
    do pkg <- fq2pkg fq
       update_Prototypes (Map.insert (pkg, f) proto)

update_Prototypes :: (Prototypes -> Prototypes) -> Perl5Parser ()
update_Prototypes f = updateState (\state -> state { prototypes = f (prototypes state) })

update_Env :: (Env -> Env) -> Perl5Parser ()
update_Env f = updateState (\state -> state { env = f (env state) })

update_Env_lexical :: (Env_lexical -> Env_lexical) -> Perl5Parser ()
update_Env_lexical f = update_Env (\env -> env { env_lexical = f (env_lexical env) })

set_Env_lexical :: Env_lexical -> Perl5Parser ()
set_Env_lexical env_lex = update_Env_lexical (\_ -> env_lex)

with_new_lexical_block :: Perl5Parser a -> Perl5Parser a
with_new_lexical_block p = 
    do prev_env_lex <- fmap (env_lexical . env) getState
       v <- p
       set_Env_lexical prev_env_lex
       return v

set_package :: String -> Perl5Parser ()
set_package pkg = update_Env_lexical (\env_lex -> env_lex { current_package = pkg })
