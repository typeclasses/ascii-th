module ASCII.TemplateHaskell
  (
    {- * Characters          -} charExp,     charPat,
    {- * Character lists     -} charListExp, charListPat,
    {- * Character supersets -} isCharExp,   isCharPat,
    {- * String supersets    -} isStringExp, isStringPat,
  )
  where

import qualified ASCII.Char as ASCII
import qualified ASCII.Superset as S

import Data.Data (Data)
import Data.Maybe (Maybe (..))
import Language.Haskell.TH.Syntax (Exp, Pat, Q, dataToExpQ, dataToPatQ)

exp :: Data a => a -> Q Exp
exp = dataToExpQ (\_ -> Nothing)

pat :: Data a => a -> Q Pat
pat = dataToPatQ (\_ -> Nothing)

{- |

@
$(charExp CapitalLetterF) == CapitalLetterF
@

-}

charExp :: ASCII.Char -> Q Exp
charExp = exp

{- |

@
let
    x = case SmallLetterS of
          $(charPat SmallLetterR) -> 1
          $(charPat SmallLetterS) -> 2
          _ -> 3
in
    x == 2
@

-}

charPat :: ASCII.Char -> Q Pat
charPat = pat

{- |

@
$(charListExp [CapitalLetterH, SmallLetterI]) == [CapitalLetterH, SmallLetterI]
@

-}

charListExp :: [ASCII.Char] -> Q Exp
charListExp = exp

{- |

@
let
    x = case [CapitalLetterH, SmallLetterI] of
          $(charListPat [CapitalLetterH, SmallLetterA]) -> 1
          $(charListPat [CapitalLetterH, SmallLetterI]) -> 2
          _ -> 3
in
    x == 2
@

-}

charListPat :: [ASCII.Char] -> Q Pat
charListPat = pat

{- |

@
$(isCharExp CapitalLetterA) == CapitalLetterA

$(isCharExp CapitalLetterA) == (65 :: Word8)

$(isCharExp CapitalLetterA) == ('ASCII.Refinement.asciiUnsafe' 65 :: 'ASCII.Refinement.ASCII' Word8)
@

-}

isCharExp :: ASCII.Char -> Q Exp
isCharExp x = [| S.fromChar $(charExp x) |]

{- |

@
let
    x = case (66 :: Word8) of
          $(isCharPat CapitalLetterA) -> 1
          $(isCharPat CapitalLetterB) -> 2
          _                           -> 3
in
    x == 2
@

-}

isCharPat :: ASCII.Char -> Q Pat
isCharPat x = [p| (S.toCharMaybe -> Just $(charPat x)) |]

isStringExp :: [ASCII.Char] -> Q Exp
isStringExp xs = [| S.fromCharList $(charListExp xs) |]

isStringPat :: [ASCII.Char] -> Q Pat
isStringPat xs = [p| (S.toCharListMaybe -> Just $(charListPat xs)) |]
