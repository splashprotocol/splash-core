{-# LANGUAGE QuantifiedConstraints #-}

module PExtra.Num (
    PNum,
    pproduct,
    psum,
    pexp,
) where

import Plutarch.Prelude

import Plutarch.Rational (PRational (..))

import PExtra.Integer (podd)
import PExtra.Monadic (tcon, tmatch)

type PNum a = forall s. Num (Term s a)

class PNum p => PExpable (p :: PType) where
    pexp :: Term s (p :--> PInteger :--> p)
    pexp = phoistAcyclic $
        plam $ \a n ->
            pif
                (n #< 0)
                perror
                (pexp' # a # n)

    -- pexp' doesn't check if n is negative
    -- the helper function is used so n is only
    -- checked as positive once and not on
    -- recursive calls
    pexp' :: Term s (p :--> PInteger :--> p)
    pexp' = phoistAcyclic $
        pfix #$ plam $ \self a n ->
            pif
                (n #== 0)
                1
                $ pif (podd # n) a 1 * (psquare #$ self # a # (pdiv # n # 2))

    psquare :: Term s (p :--> p)
    psquare = phoistAcyclic $ plam $ \x' -> plet x' $ \x -> x * x

pproduct :: (PListLike l, PElemConstraint l a, PNum a) => Term s (l a :--> a)
pproduct = phoistAcyclic $ pfoldr # plam (*) # 1

psum :: (PListLike l, PElemConstraint l a, PNum a) => Term s (l a :--> a)
psum = phoistAcyclic $ pfoldr # plam (+) # 0
