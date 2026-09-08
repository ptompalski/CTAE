# Model spec: si_goelz1992

> Human-reviewed extraction artifact. Source PDF is a scanned image-only PDF
> (text layer = watermark only, ~151 chars/page); all equations and the parameter
> table were transcribed from **rendered page images** at 200 dpi
> (`tmp/goelz_pages/page_0*.png`), not the text layer.

## Source

- **Citation:** Goelz, J.C.G., and Burk, T.E. 1992. Development of a well-behaved
  site index equation: jack pine in north central Ontario. Can. J. For. Res.
  **22**: 776–784.
- **BibTeX key:** @Goelz1992
- **Document in `sources/`:** `sources/site_index/goelz-burk-2011-development-of-a-well-behaved-site-index-equation-jack-pine-in-north-central-ontario.pdf`
  (⚠ filename says "2011" but the paper is **1992**; citation confirmed from the
  rendered title page and running headers.)
- **Model family:** si
- **Target function:** `si_goelz1992`

## Scope / domain of applicability

- **Jurisdiction / region:** north central Ontario (ON)
- **Species covered (NFI codes):** PINU.BAN (jack pine) — single species
- **Valid input ranges:** fitting/application range of breast-height age ~20–80
  years (stated as the range likely applied for jack pine in north central
  Ontario, p. 5). Data from 141 plots (109 calibration, 32 validation).
- **Age basis:** **breast-height age** (years). Height growth measured at 5-year
  increments above breast height.
- **Base age:** model is **base-age invariant** — no fixed base age. The paper
  uses base age 50 as the reference for comparison (Figs. 1–2); base age is stored
  as 50 in the CSV only as the conventional reference. Site index (SI) = predicted
  height at a chosen base age.
- **Caveats:** breast height = 1.3 m; height and SI in metres. Eq. 16 was not
  constrained to be exactly base-age invariant (criterion 7) — discrepancy across
  base ages 20/50/80 is < 0.5 m (p. 5).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H1 | predictor (known) height | m |
| A1 | predictor (known) breast-height age | years |
| H2 (Ĥ2) | predicted height at age A2 | m |
| A2 | target breast-height age | years |
| 1.3 | breast height offset | m |

## Model form(s)

Final selected model, **eq. 16** (p. 5) — base-age invariant difference form of the
Chapman–Richards function (eq. 3), obtained by solving for parameter `a` and
expanding `b` as a function of predictor height and age:

```
Ĥ2 = 1.3 + (H1 - 1.3) *
      [ 1 - exp( -b1 * (H1/A1)^b2 * A1^b3 * A2 ) ]^b4
    / [ 1 - exp( -b1 * (H1/A1)^b2 * A1^b3 * A1 ) ]^b4
```

- **Direction(s):** single equation predicts height at any age A2 given a known
  height H1 at any age A1. **Site index** is the special case A2 = base age (e.g.
  50): `SI = predict(H1, A1, A2 = base_age)`. **Height at age** given SI is the
  case H1 = SI, A1 = base age: `H = predict(SI, base_age, A2 = age)`. Both
  directions use the *same* equation (this is the point of base-age invariance);
  no separate inverse equation exists.
- **Notes on form:** A1 appears twice in the `b` function (numerator uses A2,
  denominator uses A1); this ensures Ĥ2 = H1 when A2 = A1. The `c` parameter of
  eq. 3 was found not to depend on site quality (constant b4).

## Parameters

- **Source table:** Table 2, p. 5 ("Parameter estimates and standard errors for
  eqs. 10 and 16").

| Parameter | Estimate | Std. error |
|-----------|----------|------------|
| b1 | 0.0185 | 0.0005 |
| b2 | 1.3382 | 0.0218 |
| b3 | 0.4257 | 0.0111 |
| b4 | 1.0464 | 0.0036 |
| ρ (autocorr.) | 0.3975 | 0.0227 |
| γ (autocorr.) | 0.5721 | 0.0228 |

Only b1–b4 enter the prediction equation; ρ and γ are error-structure (fitting)
parameters and are **not** stored or used for prediction.

- **Species → NFI mapping:** jack pine → PINU.BAN.
- **⚠ Flagged / uncertain values:** none. b3 printed as `0.0111` std. error with a
  mid-dot artifact in scan; estimate `0.4257` is clear. Spot-checked b1 (0.0185)
  and b4 (1.0464) against the rendered image.

## Benchmark plan

- **Fidelity (Tier 1) reference values available? YES.** Fig. 1 caption (p. 5):
  a curve generated from the height–age pair **16 m at breast-height age 50**
  (i.e. SI = 16, base age 50) gives:
  - height at breast-height age **20** = **8.83 m**
  - height at breast-height age **80** = **19.82 m**

  Test: `predict(H1 = 16, A1 = 50, A2 = 20)` ≈ 8.83 and
  `predict(H1 = 16, A1 = 50, A2 = 80)` ≈ 19.82 (expect agreement to ~0.01 m,
  allowing for rounding of the published values to 2 dp).

- **Plausibility cross-check:** compare against `si_carmean2001` (also jack pine
  = PINU.BAN, north central Ontario, base age 50) over the SI/age grid and report
  magnitude/direction of any discrepancies.
