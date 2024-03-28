module PExtra.Integer (
    podd,
    peven,
    ppow,
) where

import Plutarch.Prelude

podd :: Term s (PInteger :--> PBool)
podd = phoistAcyclic $ plam $ \n -> (pmod # n # 2) #== 1

peven :: Term s (PInteger :--> PBool)
peven = phoistAcyclic $ plam $ \n -> (pmod # n # 2) #== 0

ppow :: Term s (PInteger :--> PInteger :--> PInteger)
ppow = phoistAcyclic $
    plam $ \a n ->
        pif
            (n #< 0)
            perror
            (pexp' # a # n)

-- pexp' doesn't check if n is negative
-- the helper function is used so n is only
-- checked as positive once and not on
-- recursive calls
pexp' :: Term s (PInteger :--> PInteger :--> PInteger)
pexp' = phoistAcyclic $
    pfix #$ plam $ \self a n ->
        pif
            (n #== 0)
            1
            $ pif (podd # n) a 1 * (psquare #$ self # a # (pdiv # n # 2))

psquare :: Term s (PInteger :--> PInteger)
psquare = phoistAcyclic $ plam $ \x' -> plet x' $ \x -> x * x
