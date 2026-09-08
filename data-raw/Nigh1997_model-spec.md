# Nigh (1997) — Sitka spruce height–age (site index) model

## Source

Nigh, G.D. (1997). A Sitka spruce height–age model with improved extrapolation
properties. *The Forestry Chronicle* **73**(3): 363–369. (MAI/JUIN 1997, Vol. 73,
No. 3.)

> Note: source PDF filename says "nigh-2011"; the rendered journal footer confirms
> the correct citation is **1997**. Function named `si_nigh1997` accordingly.

## Model family

Site index / height–age (`si_` prefix). Same logistic form as `si_nigh2002`.

## Species / scope

- **Species:** Sitka spruce, *Picea sitchensis* (Bong.) Carr. — NFI code `PICE.SIT`.
- **Region:** Coastal British Columbia (developed from Queen Charlotte Islands
  stem-analysis data, CWHwh1 biogeoclimatic variant).
- **Recommended for use in British Columbia** (per abstract).

## Recommended model — equation [8] (page 366)

The paper's recommended, integral-form height–age model:

$$
H = 1.3 + (S - 1.3) \times
\frac{1 + e^{\,8.947 - 1.357\ln(49.5) - 1.013\ln(S-1.3)}}
     {1 + e^{\,8.947 - 1.357\ln(bha - 0.5) - 1.013\ln(S-1.3)}}
$$

General form:

$$
H = 1.3 + (S - 1.3) \times
\frac{1 + e^{\,b_0 + b_1\ln(49.5) + b_2\ln(S-1.3)}}
     {1 + e^{\,b_0 + b_1\ln(bha - 0.5) + b_2\ln(S-1.3)}}
$$

### Variable definitions and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| `H`    | site (top) height | m |
| `S`    | site index (site height at breast-height age 50) | m |
| `bha`  | breast-height age | years |

- **Age basis:** breast-height age (BHA).
- **Base age:** 50 years (breast height). At `bha = 50`, `H = S` exactly
  (`ln(49.5) = ln(bha - 0.5)`), so the model is conditioned to return `height = si`
  at BHA 50.
- Lower asymptote 1.3 m as age → 0.5 from above (doubly asymptotic model).

### Coefficients (Table 3, page 366, model [7])

The recommended coefficients are those of model [7] (fit with AR(1) serial-
correlation correction), which equation [8] uses:

| Coefficient | Estimate | (S.E.) |
|-------------|----------|--------|
| `b0`        | 8.947    | (0.6345) |
| `b1`        | -1.357   | (0.02337) |
| `b2`        | -1.013   | (0.1841) |

Source locator: Table 3 ("Results of the analysis of models [7], [1], and [5]"),
page 366, the model-[7] block. Standard errors recorded for reference only; not
used in prediction.

## Directions

- **Predict height** from `age` (BHA) + `si` (`S`): closed form, equation [8].
- **Predict SI** from `age` (BHA) + `height`: because `S` appears both as a
  multiplier and inside `ln(S - 1.3)`, no closed-form inverse exists; solve
  numerically by root-finding on `height(bha, S) - height = 0` (same approach as
  `si_nigh2002`).

## Domain / caveats

- Developed from Queen Charlotte Islands data (ages to ~98 yr, site index ~22–40 m
  in the sample; Table 1). The model's selling point is improved **extrapolation**
  to old ages (stated use to 400 years old in coastal BC).
- Requires `S > 1.3` and `bha > 0.5`.

## Validation plan

- **Fidelity (Tier 1):** no worked numeric example / reference table is printed in
  the paper (only figures). The one exact anchor is the conditioning identity
  `H = S` at `bha = 50`, which is testable.
- **Plausibility (Tier 2):** cross-check against `si_nigh2002` (identical model
  form, different species/coefficients) for monotonicity, doubly-asymptotic shape,
  and round-trip consistency (predict height then recover SI). Report as
  plausibility, not fidelity.
