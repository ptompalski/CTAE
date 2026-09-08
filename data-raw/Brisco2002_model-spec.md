# Model spec: si_brisco2002

> Human-review extraction artifact for the Brisco, Klinka & Nigh (2002) western
> larch height-growth (site index) model. Coefficients copied digit-for-digit
> from the rendered source images; provenance noted per value. Nothing flagged
> uncertain. **Awaiting human verification before any implementation.**

## Source

- **Citation:** Brisco, D., Klinka, K., and Nigh, G. 2002. Height growth models
  for western larch in British Columbia. West. J. Appl. For. 17(2):66–74.
- **BibTeX key:** @Brisco2002 (to add)
- **Document in `sources/`:** `sources/site_index/Height_growth_models_for_Weste.pdf`
  (scanned image-only PDF; pages rendered at 200 dpi and read as images —
  `pdftotext` recovered only the copyright watermark).
- **Model family:** si
- **Target function:** `si_brisco2002`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia (southeastern BC; native range of
  western larch — IDF, ICH, MS, ESSF biogeoclimatic zones).
- **Species covered (NFI codes):** western larch (*Larix occidentalis*),
  NFI code `LARI.OCC`.
- **Valid input ranges (complete data set, Table 1, p. 68):** breast-height age
  45–134 yr, top height 12.3–37.0 m, SI(50) 9.7–27.1 m, elevation 660–1,690 m.
- **Age basis:** breast-height age (BHA, years); `A` = age at BH (1.3 m).
- **Base age:** site index = height at breast-height age 50 (BHA 50), BC standard.
- **Caveats:**
  - The 0.5 yr offset on `A` removes a small bias at breast height (trees reach
    BH on average midway through the growing season).
  - Authors recommend the **final fitted model, eq. (6)** (the Chapman Richards
    model, eq. 3, refit to the complete data set) for prediction. Model (2)
    (conditioned logistic) is noted as having better extrapolation above ~60 m /
    old stands, but eq. (6) is the recommendation for stands under ~150 yr.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | site (top) height | m |
| A | breast-height age | years |
| S | site index (height at BHA 50) | m |
| e | base of natural logarithm | — |
| b1..b5 | fitted parameters | — |

## Model form(s)

Richards function fit per-plot (eq. 1) to generate height-age pairs — not used
directly for prediction. The four candidate model forms fit to those pairs:

**(2) Conditioned logistic:**
```
H = 1.3 + (S - 1.3) * (1 + exp(b1 + b2*ln(49.5) + b3*ln(S - 1.3))) /
                      (1 + exp(b1 + b2*ln(A - 0.5) + b3*ln(S - 1.3)))
```

**(3) Unconstrained Chapman Richards:**
```
H = 1.3 + b1*(S - 1.3)^b2 * (1 - exp(b3*(A - 0.5)))^(b4*(S - 1.3)^b5)
```

**(4) Conditioned Chapman Richards:**
```
H = 1.3 + b1*(S - 1.3)^b2 * (1 - exp(b3*(A - 0.5)))^p
p = ln( (S - 1.3)^(1 - b2) / b1 ) / ln( 1 - exp(b3*49.5) )
```

**(5) Conditioned Weibull:**
```
H = 1.3 + p * (1 - exp(-b1*(A - 0.5)^(b2*(S-1.3)^b3)))
p = (S - 1.3) / (1 - exp(-b1*49.5^(b2*(S-1.3)^b3)))
```

**(6) FINAL RECOMMENDED MODEL** — eq. (3) refit to the complete data set
(text + boxed eq. 6, p. 71):
```
H = 1.3 + 3.875*(S - 1.3)^0.7850 *
          (1 - exp(-0.01497*(A - 0.5)))^(2.193*(S - 1.3)^(-0.2318))
```
i.e. eq. (3) with b1=3.875, b2=0.7850, b3=-0.01497, b4=2.193, b5=-0.2318.

- **Direction(s) supported:**
  - **Predict height** from `age` + `si`: substitute directly into eq. (6).
  - **Predict SI** from `age` + `height`: eq. (6) cannot be inverted in closed
    form for S (S appears in the base, the leading power, and the outer
    exponent), so solve numerically (root-find on H(A, S) - height = 0). At
    A = 50, eq. (6) returns H = S exactly (self-consistency: the exponent term
    equals (S-1.3)/... — verify numerically during implementation).

- **Notes on form:** single species, single global parameter set. Table 4
  coefficients are the **calibration-set** fits (n=69); eq. (6) is the
  **complete-data-set** refit and is what should be implemented for prediction.
  Table 4 values are retained below for provenance / cross-checking only.

## Parameters

- **Species → NFI mapping:** western larch (western larch / Lw) → `LARI.OCC`.
- **⚠ Flagged / uncertain values:** none. All coefficients legible in the
  rendered images.

### Final model (eq. 6, complete data set) — to be implemented

| parameter | value | source |
|-----------|-------|--------|
| b1 | 3.875     | eq. (6), p. 71 |
| b2 | 0.7850    | eq. (6), p. 71 |
| b3 | -0.01497  | eq. (6), p. 71 |
| b4 | 2.193     | eq. (6), p. 71 |
| b5 | -0.2318   | eq. (6), p. 71 |

### Table 4 (calibration-set fits, n=69) — provenance only, NOT implemented

| Model / Eq | b1 | b2 | b3 | b4 | b5 | MSE | DF |
|------------|----|----|----|----|----|-----|----|
| Conditioned logistic / 2       | 6.487   | -1.174 | -0.4829  |       |         | 1.306 | 1102 |
| Chapman Richards / 3           | 3.072   | 0.8467 | -0.01641 | 1.843 | -0.1643 | 1.277 | 1100 |
| Conditioned Chapman Richards / 4 | 3.027 | 0.8566 | -0.01588 |       |         | 1.293 | 1102 |
| Conditioned Weibull / 5        | 0.01030 | 0.6129 | 0.1963   |       |         | 1.289 | 1102 |

(Italic values under each row in Table 4 are asymptotic standard errors — not
model coefficients; omitted here.)

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No published worked
  (age, SI) → height table exists in the paper. Fidelity checks available:
  1. **Self-consistency:** eq. (6) evaluated at A = 50 must return H = S for any
     S in the valid range (the site-index definition). Verify to machine
     precision / small tolerance.
- **Cross-check (always):** compare eq. (6) height predictions against other
  existing BC `si_*` models over overlapping ages/SI (different species, so not
  identical — expect similar magnitude and monotone shape). Report discrepancies.
- Note in NEWS: implemented from the recommended eq. (6); no published
  worked-example table, validated via SI-at-BHA-50 self-consistency + same-family
  cross-check.

## Open decision for the reviewer

The paper presents four model forms plus the final recommended eq. (6).
**Recommendation:** implement only eq. (6) (the authors' recommended model),
exposing `age` + one of `height`/`si`, mirroring `si_nigh2017`. Confirm whether
you also want the other three forms (2, 4, 5) available as variants; if so they
would use the Table 4 *calibration* coefficients (the only ones published for
those forms).
