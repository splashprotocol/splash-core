module PExtra.Integer (
    podd,
    peven,
    ppow,
    ppow10
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

ppow10 :: Term s (PInteger :--> PInteger :--> PInteger)
ppow10 = phoistAcyclic $
    plam $ \a n ->
        pif
            (n #< 0)
            perror
            (pexp10' # a # n)

pexp10' :: Term s (PInteger :--> PInteger :--> PInteger)
pexp10' = phoistAcyclic $
    pfix #$ plam $ \self a n ->
        pif
            (n #< 12)
            (pexp10constant' # a # n)
            $ pif (podd # n) a 1 * (psquare #$ self # a # (pdiv # n # 2))

-- max degree is 11
pexp10constant' :: Term s (PInteger :--> PInteger :--> PInteger)
pexp10constant' = phoistAcyclic $
    pfix #$ plam $ \self a n ->
        pif
            ( n #== 11 )
            (pconstant 100000000000)
            ( pif (n #== 10)
              (pconstant 10000000000)
              ( pif (n #== 9)
                (pconstant 1000000000)
                ( pif (n #== 8)
                  (pconstant 100000000)
                  ( pif (n #== 7)
                    (pconstant 10000000)
                    ( pif (n #== 6)
                      (pconstant 1000000)
                      ( pif (n #== 5)
                        (pconstant 100000)
                        ( pif (n #== 4)
                          (pconstant 10000)
                          ( pif (n #== 3)
                            (pconstant 1000)
                            ( pif (n #== 2)
                              (pconstant 100)
                              $ pif (n #== 1) (pconstant 10) (pconstant 1)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )

psquare :: Term s (PInteger :--> PInteger)
psquare = phoistAcyclic $ plam $ \x' -> plet x' $ \x -> x * x
