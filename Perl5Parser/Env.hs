module Perl5Parser.Env
    ( get_prototype
    , set_prototype
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper

import qualified Data.Map as Map


get_prototype :: (IdentT, String) -> Perl5Parser (Maybe String)
get_prototype (LocalIdent, f) = do state <- getState
                                   return$ Map.lookup f (local_prototypes (prototypes state))
get_prototype (fq, f) = do state <- getState
                           return$ Map.lookup (fq_canonical fq, f) (per_pkg_prototypes (prototypes state))

updatePrototypes :: (Prototypes -> Prototypes) -> Perl5Parser ()
updatePrototypes f = updateState (\state -> state { prototypes = f (prototypes state) })

set_prototype :: (IdentT, String) -> String -> Perl5Parser ()
set_prototype (LocalIdent, f) proto = updatePrototypes update
    where update protos = protos { local_prototypes = Map.insert f proto (local_prototypes protos) }
set_prototype (fq, f) proto = updatePrototypes update
    where update protos = protos { per_pkg_prototypes = Map.insert (fq_canonical fq, f) proto (per_pkg_prototypes protos) }
