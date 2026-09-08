# Model spec: si_nigh2009

> Human-reviewed extraction artifact for the Nigh, Thomas, Yearsley & Wang (2009)
> paper birch height-age (site index) model for British Columbia.

## Source

- **Citation:** Nigh, G.D., Thomas, K.D., Yearsley, K., and Wang, J. 2009.
  Site-dependent Height-Age Models for Paper Birch in British Columbia.
  Northwest Science 83(3): 253-261. https://doi.org/10.3955/046.083.0308
  (Confirmed from the rendered title page and running headers.)
- **BibTeX key:** @Nigh2009
- **Document in `sources/`:** `sources/site_index/Nigh2009.pdf`
- **Reference implementation:** `sources/site_index/SK_SiteIndex_SAS_macros_with_BHAge_20220409.sas`
  macros `SI_Nigh_2009` / `HT_Nigh_2009` (lines 987-1068). **The SAS is the
  stated authority for this task.**
- **Model family:** si
- **Target function:** `si_nigh2009`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia (interior).
- **Species covered (NFI codes):** paper birch (*Betula papyrifera*) ->
  `BETU.PAP`. SAS species codes: `WB`, `BW`.
- **Calibration data:** 61 plots (0.0363 ha) in the SBS (34), ICH (19), and
  IDF (8) biogeoclimatic zones, sampled 1997-1999 (Table 1, p. 254). Site
  height data used at 5-yr intervals from BHA 5. Grand-total average height
  20.81 m, BHA 80 yr, SI 17.34 m (Table 1).
- **Age basis:** breast-height age (BHA), years.
- **Base age:** 50 years breast-height age (SI = site height at BHA 50; Table 1
  note c, p. 254).
- **Caveats:** poor plot distribution across subzones prevented testing at
  subzone level (p. 255). Paper birch rarely lives beyond ~150 yr.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| HT | site height | m |
| SI | site index (site height at BHA 50) | m |
| BHA | breast-height age | yr |
| ln | natural logarithm | - |
| a0, a1, a2 | model parameters | - |

## Model form(s)

All three models share one log-logistic functional form (eq. 1, p. 254). At
BHA = 50 the model returns HT = SI exactly (49.5 = base age 50 - 0.5).

```
HT = 1.3 + (SI - 1.3) *
     ( 1 + exp( a0 + a1*ln(49.5)     + a2*ln(SI - 1.3) ) ) /
     ( 1 + exp( a0 + a1*ln(BHA - 0.5) + a2*ln(SI - 1.3) ) )
```

The three models differ only in their coefficients:

- **Model 1 (base):** the plain log-logistic base fit (Table 2). This is the
  form and coefficients used by the SAS reference macros. Use anywhere in BC or
  when the BEC zone is unknown and Model 2 is not desired.
- **Model 2 (operational):** the operational form of the mixed model (random
  effect, AR(3), weighting dropped for prediction). Recommended by the authors
  when height is NOT being estimated in ICH/IDF/SBS, or when the zone is unknown
  (p. 259, Discussion eq. [2]).
- **Model 3 (zonal):** indicator-variable model differentiating SBS from
  ICH/IDF. Only a1 carries a significant zone effect: a1 = -(1.077 + 0.08712*SBS)
  where SBS = 1 for the SBS zone, 0 for ICH/IDF (Discussion eq. [3], p. 259).
  Recommended for estimating height in the ICH, IDF, and SBS zones.

- **Direction(s):**
  - Predict HT from (BHA, SI): direct evaluation of eq. 1.
  - Invert to predict SI from (BHA, HT): eq. 1 is not closed-form invertible in
    SI; SAS solves it by the fixed-point iteration
    `si1 = 1.3 + (HT-1.3)*(x2/x1)` with damping `si0 = (si0+si1)/2`, tol 1e-8.
    Equivalent to a root-find on HT_pred(BHA, SI) - HT = 0.

## Parameters

Coefficients copied digit-for-digit from Table 2 (p. 256) and cross-checked
against the inline operational equations in the Discussion (p. 259) and, for
Model 1, against the SAS reference macros.

| Model | a0 | a1 | a2 | Source |
|-------|-----|-----|-----|--------|
| 1 (base)       | 8.842 | -1.124   | -1.561 | Table 2; SAS b1/b2/b3 |
| 2 (operational)| 9.604 | -1.113   | -1.849 | Table 2; eq. [2] p.259 |
| 3 (ICH/IDF)    | 9.732 | -1.077   | -1.888 | Table 2 a00/a10/a20; eq. [3] |
| 3 (SBS)        | 9.732 | -1.16412 | -1.888 | a10=-1.077, a12=-0.08712; a1 = -(1.077+0.08712) |

Model 3 a12 = -0.08712 (SE 0.04063) is the SBS increment on a1. Because eq. [3]
writes a1 = -(1.077 + 0.08712*SBS), the effective SBS a1 = -(1.077+0.08712) =
-1.16412. ICH and IDF share the SBS=0 row.

- **Species -> NFI mapping:** paper birch -> `BETU.PAP`.
- **Flagged / uncertain values:** none. Table 2's Model-column labels are
  vertically offset in the text layer (a `pdftotext` artifact), but the values
  group unambiguously into three blocks of three coefficients, each independently
  confirmed by the inline Discussion equations. The PDF could not be rendered to
  an image in this environment (pdftools/magick crash the session on this file);
  fidelity rests on the two-source text agreement plus the SAS cross-check.

## Divergence from the SAS reference

The SAS reference implements **only Model 1** (base). Per user instruction for
this task, all three published models are implemented; Model 1 reproduces the
SAS behavior exactly, and Models 2 and 3 are additional. This divergence is
intentional and flagged here and in NEWS.

## Benchmark plan

- **Fidelity (Tier 1):** primary benchmark is the **SAS reference** for Model 1.
  Generate `HT_Nigh_2009` / `SI_Nigh_2009` outputs over a grid of (BHA, SI) /
  (BHA, HT) and compare against `si_nigh2009(..., model = 1)`. No printed numeric
  worked example exists in the paper for Models 2/3.
- **Self-consistency:** at BHA = 50, HT must equal SI exactly for every model.
  Round-trip: predict HT from (BHA, SI), recover SI from (BHA, HT).
- **Plausibility (Tier 2) for Models 2 & 3:** monotonic in BHA and SI; magnitudes
  consistent with Model 1 and with the paper's Figure 3 (Models 1-3 "virtually
  the same over the range of the data"). Flag "no source benchmark table for
  Models 2/3" in NEWS.
