# Model spec: si_nigh2002

> Human-reviewed extraction artifact for the Nigh, Krestov & Klinka (2002)
> trembling aspen height-age (site index) model for British Columbia.

## Source

- **Citation:** Nigh, G.D., Krestov, P.V., and Klinka, K. 2002. Trembling Aspen
  Height-Age Models for British Columbia. Northwest Science 76(3): 202-212.
  ⚠ WARN: journal name/volume/pages inferred from running headers
  ("Aspen Height-Age Models", pp. 202-212) and known Nigh aspen literature; the
  reference-list page does not print the host journal. Please confirm the exact
  citation before release. Received 25 October 2001, accepted 1 April 2002.
- **BibTeX key:** @Nigh2002
- **Document in `sources/`:** `sources/site_index/Nigh_etal_2002_SI_tA_BC.pdf`
- **Model family:** si
- **Target function:** `si_nigh2002`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia (interior).
- **Species covered (NFI codes):** trembling aspen (*Populus tremuloides*) ->
  `POPU.TRE`.
- **Valid input ranges:** calibration data spanned BHA 50-177 yr, site index
  5.60-29.56 m, height 5.74-37.50 m (Table 1, Table 2, p. 205). The models are
  logistic height-age curves conditioned to pass through (BHA 50, HT = SI).
- **Age basis:** breast-height age (BHA), years.
- **Base age:** 50 years breast-height age (SI = site height at BHA 50, by
  definition, p. 203).
- **Caveats:** Sampling was not random; statistically-based conclusions across
  zones cannot be drawn (p. 205). Extended model is calibrated to six BEC zones
  (BWBS, ICH, IDF, MS, SBPS, SBS); the base model is general (any BC location or
  unknown zone).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| HT | site height | m |
| SI | site index (site height at BHA 50) | m |
| BHA | breast-height age | yr |
| ln | natural logarithm | - |
| a0, a1, a2 | model parameters | - |

## Model form(s)

Base and extended models share one logistic functional form (eq. 1, p. 203);
the extended model lets a0, a1, a2 vary by BEC zone (eq. 4, p. 204).

```
HT = 1.3 + (SI - 1.3) *
     ( 1 + exp( a0 + a1*ln(49.5) + a2*ln(SI - 1.3) ) ) /
     ( 1 + exp( a0 + a1*ln(BHA - 0.5) + a2*ln(SI - 1.3) ) )
```

- **Direction(s):**
  - Predict HT from (BHA, SI): direct evaluation of eq. 1.
  - Invert to predict SI from (BHA, HT): eq. 1 is not closed-form invertible in
    SI (SI appears both as the multiplier `(SI-1.3)` and inside
    `ln(SI-1.3)`), so SI is solved numerically (root-find on
    HT_pred(BHA, SI) - HT = 0).
- **Notes on form:** The numerator uses the constant 49.5 = base age 50 - 0.5;
  the denominator uses (BHA - 0.5). Both are the "BHA - 0.5" convention with
  base age 50. At BHA = 50 the model returns HT = SI exactly.
- **Weighting functions (eqs. 3, 5) and AR(1) error (eq. 2)** are fitting
  details only; not part of the deterministic prediction.

## Parameters

Coefficients copied digit-for-digit. Two parameter sets:

**Base model** (Table 3, p. 205), one global row:

- a0 = 7.423, a1 = -1.150, a2 = -0.9614  (phi = 0.9892, not used in prediction).

**Extended model** (per-zone effective coefficients, given explicitly in prose
on p. 206; derived by the authors from Table 4 indicator estimates):

- a0 = 7.314 for BWBS, IDF, MS, SBPS; a0 = 8.363 for ICH, SBS.
- a1 = -1.074 for BWBS, IDF, SBS; a1 = -1.446 for ICH; a1 = -1.243 for MS, SBPS.
- a2 = -1.011 for BWBS, ICH, IDF; a2 = -0.9080 for MS, SBPS; a2 = -1.267 for SBS.

The CSV stores one row per parameter set: `base` (zone = NA) plus one row per BEC
zone for the extended model.

- **Source table(s):** Table 3, p. 205 (base); p. 206 prose + Table 4, p. 206
  (extended per-zone effective values).
- **Species -> NFI mapping:** trembling aspen -> `POPU.TRE`.
- **⚠ Flagged / uncertain values:** none illegible. ⚠ The extended a0/a1/a2 rows
  are the authors' *resolved per-zone* coefficients (p. 206 prose), not the raw
  indicator estimates in Table 4. This is intentional and is the directly usable
  parameterization. Cross-check: BWBS reconstruction from Table 4
  (a0 = a00 = 7.314; a1 = a10 = -1.074; a2 = a20 = -1.011) matches the prose.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No worked numeric example or
  height/SI reference table is printed in the source.
- **Self-consistency (near-Tier-1) checks that ARE possible from the equation:**
  - At BHA = 50, HT must equal SI exactly (model definition) — deterministic
    identity check for both base and extended models, all zones.
  - Round-trip: predict HT from (BHA, SI), then recover SI from (BHA, HT); must
    return the original SI.
- **Plausibility (Tier 2):**
  - Compare against `si_hugarcia2009` (BC interior SI model, logistic-type
    height-age) for order-of-magnitude and monotonicity (HT increasing in BHA
    and in SI).
  - ⚠ This model ships with **no external source benchmark table** — note in NEWS
    and rely on the exact-at-base-age identity and round-trip checks as the
    primary fidelity guard.
