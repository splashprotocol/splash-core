# ⚠️ CRITICAL VULNERABILITY — DO NOT USE THIS CONTRACT

**Status: vulnerable. Do not deploy, fork, or build on any code in this directory.**

On 13 September 2026 the Splash ADA/OADA StableSwap pool running this validator family was drained
through a security breach in the pool contract. The full incident report is in this directory:
[SPLASH_STABLESWAP_INCIDENT_2026-09-13.pdf](./SPLASH_STABLESWAP_INCIDENT_2026-09-13.pdf).

## What is wrong

The pool validator computes the tradable reserves as `total − protocol_fees` and never checks that
the result is positive. A negative tradable reserve passes the StableSwap invariant checks, which
lets both assets leave the pool in a single "swap". The code in this directory shares that reserve
computation with the exploited validator.

## What this means for you

- Do not deploy `pool.ak`, `deposit.ak`, `redeem.ak`, `redeem_uniform.ak` or `proxy_dao.ak` from this directory.
- Do not provide liquidity to any pool built from this code.
- A fixed version will be published separately and announced by the Splash team; until then treat every
  stable-pool validator in this repository as unsafe.

Contact: admin@splash.trade
