# Model spec: si_carmean2006

> Human-review extraction artifact. Values transcribed from the source PDF and
> cross-checked against the SAS macro where available. Uncertain values flagged
> with WARN.

## Source

- **Citation:** Carmean, W.H., Hazenberg, G., and Deschamps, K.C. 2006.
  Polymorphic site index curves for black spruce and trembling aspen in
  northwest Ontario. The Forestry Chronicle 82(2): 213-231.
- **BibTeX key:** @Carmean2006
- **Document in `sources/`:**
  `sources/site_index/carmean-et-al-2011-polymorphic-site-index-curves-for-black-spruce-and-trembling-aspen-in-northwest-ontario.pdf`
  (NB: the filename says "2011"; the article footer and the SAS macro both
  identify the publication year as **2006**, Vol. 82 No. 2, March/April.)
- **Model family:** si
- **Target function:** `si_carmean2006`

## Scope / domain of applicability

- **Jurisdiction / region:** Northwest Ontario (ON)
- **Species covered (NFI codes):**
  - Black spruce — `PICE.MAR` (Picea mariana)
  - Trembling aspen — `POPU.TRE` (Populus tremuloides)
- **Valid input ranges:** curves fitted to data <= 100 years breast-height age;
  can be extended to ~150 years BH age with reduced precision (dashed
  extrapolation in Figs. 1-2). Site index range roughly 7-19 m (black spruce)
  and 12-24 m (aspen) per the Table 3 plot distributions.
- **Age basis:** breast-height age (years). Curves start at breast height
  (0 years at BH).
- **Base age:** 50 years breast-height age. Site index S = height at 50 yr BH age.
- **Caveats:** dominant/codominant trees in natural, fully-stocked, even-aged
  stands. Newnham (1988) constrained version of the Ek (1971) nonlinear model.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | predicted height of dominant/codominant trees | m |
| S | site index = mean height at 50 yr breast-height age | m |
| Age | breast-height age | years |
| k | model-internal constant derived from S (see form) | - |

## Model form(s)

Newnham (1988) constrained model. Height from site index and BH age (eq. 1 for
black spruce, eq. 2 for trembling aspen; identical form, different coefficients):

```
H = 1.3 + b1 * (S - 1.3)^b2 * ( 1 - k^(Age/50) )^( b3 * (S - 1.3)^b4 )

where
  k = 1 - [ (S - 1.3) / ( b1 * (S - 1.3)^b2 ) ]^( 1 / ( b3 * (S - 1.3)^b4 ) )
```

- **Direction(s):**
  - Forward: predict H from S and Age (closed form above).
  - Inverse: predict S from H and Age (no closed form; solved iteratively).
    The SAS macro solves S by fixed-point iteration:
    `si1 = 1.3 + ((H - 1.3) / x1)^(1/b2)` with `x1 = b1*(1 - k^(Age/50))^(b3*(S-1.3)^b4)`,
    damped update `si0 = (si0 + si1)/2`, starting `si0 = 20`, tol 1e-8.
- **Notes on form:** the constraint is that at Age = 50 the curve passes through
  H = S exactly. Base (reference) age Tr = 50.

## Parameters

Source: equations [1] and [2], p. 7 (rendered to image; text layer dropped the
math). Coefficients (b1, b2, b3, b4):

| Species | b1 | b2 | b3 | b4 | source |
|---------|------|--------|--------|---------|--------|
| Trembling aspen | 4.36 | 0.6654 | 1.2137 | -0.0761 | eq. 2, p. 7 |
| Black spruce | 16.95 | 0.1136 (WARN) | 0.6167 | 0.3116 | eq. 1, p. 7 |

- **Species -> NFI mapping:** black spruce -> `PICE.MAR`; trembling aspen ->
  `POPU.TRE`. (SAS accepts aspen aliases TA/AT/AW/BP/PB; black spruce not in SAS.)
- **Cross-check vs SAS macro** (`SK_SiteIndex_SAS_macros_with_BHAge_20220409.sas`,
  `%macro SI/HT_Carmean_2006`, lines 1070-1164): the SAS file implements **only
  trembling aspen** and confirms aspen coefficients digit-for-digit
  (`b1 = 4.36; b2 = 0.6654; b3 = 1.2137; b4 = -0.0761`) and the exact equation
  form / base age (Tr = 50). Black spruce has **no SAS cross-check**.
- **WARN Flagged / uncertain values:** black spruce `b2`. In eq. [1] the exponent
  on `16.95 (S - 1.3)` renders as `0.113b` / `0.1136` — the final glyph is
  degraded in the source raster and not cleanly legible at 300/600 dpi. Best
  reading is **0.1136**, but this needs human confirmation against a clean copy
  of the paper. b1 (16.95), b3 (0.6167), b4 (0.3116) are legible.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No worked numeric example or
  reference height table appears in the paper (only distribution tables and
  figures).
  - Aspen: cross-validate against the SAS macro by reproducing its forward (HT)
    and inverse (SI) computation for a grid of ages/site indices. This is a
    faithful independent implementation and serves as the fidelity benchmark for
    aspen.
- **Black spruce — plausibility (Tier 2):** no source benchmark and no SAS
  implementation. Compare against same-family black-spruce SI curves
  (e.g. `si_cieszewskibella1991`, `si_payandeh1974`) for plausible magnitude and
  monotonicity, AND verify the built-in constraint H(Age=50) = S numerically.
  - WARN Black spruce ships with **no source benchmark** and one flagged
    coefficient (b2) — note in NEWS.
