# Evaluating the Impact of Bolsa Família on Human Development and Child Health

**Evidence from Brazilian Municipalities**

Silvia Pagliarulo · Lucia D'Amore — May 2026

## Overview

Municipality-level panel evidence on the causal impact of Brazil's Bolsa
Família Program (BFP) on two dimensions of human welfare:

1. **Aggregate human development** — Firjan Municipal Development Index (IFDM);
2. **Child health** — infant mortality rate (IMR).

Estimation relies on complementary Two-Way Fixed Effects (TWFE) and event
study specifications over a twenty-year horizon.


## Data

**IFDM model** — 5,561 municipalities × 2006–2016 (60,362 obs.).
Outcome: IFDM (0–1 composite index). Treatment: BFP coverage.
Sources: Firjan, IPEADATA, IBGE/SIDRA.

**IMR model** — 5,570 municipalities × 2000–2020 (113,857 obs. after
restrictions). Outcome: infant deaths per 1,000 live births. Treatment: BFP
coverage (structural zeros for 2000–2003). Sources: DATASUS (SIM + SINASC),
IPEADATA, IBGE/SIDRA.

## Methodology

Baseline TWFE with continuous treatment:

    y_it = β·coverage_it + X'_it·γ + α_i + λ_t + ε_it

For IMR, Poisson Pseudo-Maximum Likelihood (PPML) handles structural zeros.

**Event study.** IFDM: intensity anchored to 2006 baseline; quadratic
de-trending on 2006–2011. IMR: intensity anchored to 2004 (national rollout),
reference year 2003; pre-treatment coefficients (2000–2002) are direct
falsification tests.

**Data construction.** GDP deflated via IPCA (deflateBR); GDP series splicing
(IBGE Tables 21 → 5938); winsorization at p99; exclusion of municipalities
with <20 live births; IMR > 300 dropped as artefacts.


## Limitations

- No genuine pre-treatment period for IFDM (program active by 2006).
- De-trending window (2006–2011) is not truly pre-treatment.
- Time-varying endogeneity of coverage (municipal administrative capacity).
- IMR under-reporting in smaller/remote municipalities.

## References

- Rasella et al. (2013). Effect of a CCT on childhood mortality. The Lancet, 382.
- Soares et al. (2010). Evaluating the impact of Brazil's Bolsa Família. LARR, 45(2).
- Silva & Tenreyro (2006). The log of gravity. REStat.
- Ciula (2022). The impact of Bolsa Família on multidimensional poverty. MPRA.
