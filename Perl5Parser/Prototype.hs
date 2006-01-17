module Perl5Parser.Prototype
    ( parse_prototype
    , get_prototype
    , set_prototype
    , builtin_prototypes
    , filetest_functions
    ) where

import Perl5Parser.Types
import Perl5Parser.ParserHelper
import List (intersect)

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



parse_prototype :: String -> Maybe (Int, Int)
parse_prototype = parse parser () ""
    where
      parser = do left <- parse_simple                     
                  right <- option [] (do { char ';'; parse_simple })
                  return$ compute_size left right

      compute_size left right =
          case (compute_size_one left, compute_size_one right) of
            (Just min, Just n) -> Just (min, min + n)
            _ -> Nothing
      compute_size_one l = 
          case intersect ["@", "%"] l of
            [] -> Just$ length l
            _ -> Nothing

      parse_simple = many (seQ [ charl '\\', p3 ] <|> p3)
      p3 = seQ [ charl '[', manY p4, charl ']' ] <|> p4
      p4 = oneOfl "$@%&*"


builtin_prototypes = Map.unions $ map Map.fromList [ filetest_prototypes, known_builtin_prototypes, unknown_builtin_prototypes ]


filetest_functions = "rwxoRWXOezsfdlpSbctugkTBMAC"

filetest_prototypes = map (\c -> ("-" ++ [c], ";$")) filetest_functions

unknown_builtin_prototypes =
    [ ("chop", ";$")
    , ("chomp", ";$")
    , ("defined", ";$")
    , ("delete", "$")
    , ("do", "&")
    , ("eval", ";$")
    , ("exists", "$")
    , ("glob", ";$")
    , ("grep", "&@")
    , ("local", "$")
    , ("map", "&@")
    , ("my", "$")
    , ("our", "$")
    , ("pos", ";$")
    , ("print", "@")
    , ("prototype", "$")
    , ("readpipe", "$")
    , ("return", "@")
    , ("scalar", "$")
    , ("split", "@")
    , ("sort", "@")
    , ("study", ";$")
    , ("tied", "$")
    , ("undef", ";$")
    , ("untie", "$")
    , ("__FILE__", "")
    , ("__LINE__", "")
    , ("__PACKAGE__", "")
    ]

-- | obtained using perl "prototype" function on @pos and @neg functions from perl_keyword.pl (in perl source)
known_builtin_prototypes =
    [ ("abs", ";$")
    , ("accept", "**")
    , ("alarm", ";$")
    , ("atan2", "$$")
    , ("bind", "*$")
    , ("binmode", "*;$")
    , ("bless", "$;$")
    , ("caller", ";$")
    , ("chdir", ";$")
    , ("chmod", "@")
    , ("chown", "@")
    , ("chr", ";$")
    , ("chroot", ";$")
    , ("close", ";*")
    , ("closedir", "*")
    , ("connect", "*$")
    , ("cos", ";$")
    , ("crypt", "$$")
    , ("dbmclose", "\\%")
    , ("dbmopen", "\\%$$")
    , ("die", "@")
    , ("dump", "")
    , ("each", "\\%")
    , ("endgrent", "")
    , ("endhostent", "")
    , ("endnetent", "")
    , ("endprotoent", "")
    , ("endpwent", "")
    , ("endservent", "")
    , ("eof", ";*")
    , ("exit", ";$")
    , ("exp", ";$")
    , ("fcntl", "*$$")
    , ("fileno", "*")
    , ("flock", "*$")
    , ("fork", "")
    , ("formline", "$@")
    , ("getc", ";*")
    , ("getgrent", "")
    , ("getgrgid", "$")
    , ("getgrnam", "$")
    , ("gethostbyaddr", "$$")
    , ("gethostbyname", "$")
    , ("gethostent", "")
    , ("getlogin", "")
    , ("getnetbyaddr", "$$")
    , ("getnetbyname", "$")
    , ("getnetent", "")
    , ("getpeername", "*")
    , ("getpgrp", ";$")
    , ("getppid", "")
    , ("getpriority", "$$")
    , ("getprotobyname", "$")
    , ("getprotobynumber", "$")
    , ("getprotoent", "")
    , ("getpwent", "")
    , ("getpwnam", "$")
    , ("getpwuid", "$")
    , ("getservbyname", "$$")
    , ("getservbyport", "$$")
    , ("getservent", "")
    , ("getsockname", "*")
    , ("getsockopt", "*$$")
    , ("gmtime", ";$")
    , ("hex", ";$")
    , ("index", "$$;$")
    , ("int", ";$")
    , ("ioctl", "*$$")
    , ("join", "$@")
    , ("keys", "\\%")
    , ("kill", "@")
    , ("lc", ";$")
    , ("lcfirst", ";$")
    , ("length", ";$")
    , ("link", "$$")
    , ("listen", "*$")
    , ("localtime", ";$")
    , ("lock", "\\$")
    , ("log", ";$")
    , ("lstat", ";$") -- perl says it's "*", WTF??
    , ("mkdir", "$;$")
    , ("msgctl", "$$$")
    , ("msgget", "$$")
    , ("msgrcv", "$$$$$")
    , ("msgsnd", "$$$")
    , ("not", "$")
    , ("oct", ";$")
    , ("open", "*;$@")
    , ("opendir", "*$")
    , ("or", "")
    , ("ord", ";$")
    , ("pack", "$@")
    , ("pipe", "**")
    , ("pop", ";\\@")
    , ("push", "\\@@")
    , ("quotemeta", ";$")
    , ("rand", ";$")
    , ("read", "*\\$$;$")
    , ("readdir", "*")
    , ("readline", ";*")
    , ("readlink", ";$")
    , ("recv", "*\\$$$")
    , ("ref", ";$")
    , ("rename", "$$")
    , ("reset", ";$")
    , ("reverse", "@")
    , ("rewinddir", "*")
    , ("rindex", "$$;$")
    , ("rmdir", ";$")
    , ("seek", "*$$")
    , ("seekdir", "*$")
    , ("select", ";*")
    , ("semctl", "$$$$")
    , ("semget", "$$$")
    , ("semop", "$$")
    , ("send", "*$$;$")
    , ("setgrent", "")
    , ("sethostent", "$")
    , ("setnetent", "$")
    , ("setpriority", "$$$")
    , ("setprotoent", "$")
    , ("setpwent", "")
    , ("setservent", "$")
    , ("setsockopt", "*$$$")
    , ("shift", ";\\@")
    , ("shmctl", "$$$")
    , ("shmget", "$$$")
    , ("shmread", "$$$$")
    , ("shmwrite", "$$$$")
    , ("shutdown", "*$")
    , ("sin", ";$")
    , ("sleep", ";$")
    , ("socket", "*$$$")
    , ("socketpair", "**$$$")
    , ("splice", "\\@;$$@")
    , ("sprintf", "$@")
    , ("sqrt", ";$")
    , ("srand", ";$")
    , ("stat", ";$") -- perl says it's "*", WTF??
    , ("substr", "$$;$$")
    , ("symlink", "$$")
    , ("syscall", "$@")
    , ("sysopen", "*$$;$")
    , ("sysread", "*\\$$;$")
    , ("sysseek", "*$$")
    , ("syswrite", "*$;$$")
    , ("tell", ";*")
    , ("telldir", "*")
    , ("time", "")
    , ("times", "")
    , ("truncate", "$$")
    , ("uc", ";$")
    , ("ucfirst", ";$")
    , ("umask", ";$")
    , ("unlink", "@")
    , ("unpack", "$$")
    , ("unshift", "\\@@")
    , ("utime", "@")
    , ("values", "\\%")
    , ("vec", "$$$")
    , ("wait", "")
    , ("waitpid", "$$")
    , ("wantarray", "")
    , ("warn", "@")
    , ("write", ";*")
    , ("xor", "$$")
    ]
