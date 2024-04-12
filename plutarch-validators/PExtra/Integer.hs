module PExtra.Integer (
    podd,
    peven,
    ppow,
    ppowR
) where

import Plutarch.Prelude

podd :: Term s (PInteger :--> PBool)
podd = phoistAcyclic $ plam $ \n -> (pmod # n # 2) #== 1

peven :: Term s (PInteger :--> PBool)
peven = phoistAcyclic $ plam $ \n -> (pmod # n # 2) #== 0

ppow :: Term s (PInteger :--> PInteger :--> PInteger)
ppow = phoistAcyclic $
    pfix #$ plam $ \self a power ->
        pif
            (power #== 0)
            1
            $ a * (self # a # (power - 1))

ppowR :: Term s (PRational :--> PInteger :--> PRational)
ppowR = phoistAcyclic $
    pfix #$ plam $ \self a power ->
        pif
            (power #== 0)
            1
            $ a * (self # a # (power - 1))