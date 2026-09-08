# Model spec: si_carmean2001

> Human-review extraction artifact. Values transcribed from the source PDF
> (rendered to image; the math text layer dropped) and cross-checked against the
> SAS macro. Uncertain values flagged with WARN.

## Source

- **Citation:** Carmean, W.H., Niznowski, G.P., and Hazenberg, G. 2001.
  Polymorphic site index curves for jack pine in northern Ontario. The Forestry
  Chronicle 77(1): 141-150. (January/February 2001, Vol. 77, No. 1; article
  begins p. 141.)
- **BibTeX key:** @Carmean2001
- **Document in `sources/`:**
  `sources/site_index/carmean-et-al-2011-polymorphic-site-index-curves-for-jack-pine-in-northern-ontario.pdf`
  (NB: the filename says "2011"; the page footer identifies the publication as
  **Jan/Feb 2001, Vol. 77, No. 1**, and the SAS macro also names it
  "Carmean 2001".)
- **Model family:** si
- **Target function:** `si_carmean2001`

## Scope / domain of applicability

- **Jurisdiction / region:** Northern Ontario (ON) — combined Northwestern,
  North Central, Northern and Northeastern regions.
- **Species covered (NFI codes):** jack pine — `PINU.BAN` (Pinus banksiana).
- **Valid input ranges:** stem-analysis data from 383 plots; site index range
  7.6-22.4 m, BH-age range 50-157 years (Table 1). Recommended equation fitted
  to data <= 100 years breast-height age; the paper reports poor sites grow
  almost linearly to ~100 yr BH age.
- **Age basis:** breast-height age (years). Curves start at breast height.
- **Base age:** 50 years breast-height age. Site index SI = total height (m) of
  dominant/codominant trees at 50 yr BH age.
- **Caveats:** dominant/codominant trees in fully-stocked, even-aged,
  undisturbed mature jack pine stands. Newnham (1988) constrained version of the
  Ek (1971) nonlinear model.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H (Ĥ)  | predicted height of dominant/codominant trees | m |
| SI     | site index = total height at 50 yr breast-height age | m |
| BHage  | breast-height age | years |
| K      | model-internal constant derived from SI (see form) | - |

## Model form(s)

Newnham (1988) constrained model (eq. 1, p. 4). Height from site index and BH
age:

```
Ĥ = 1.3 + b1 * (SI - 1.3)^b2 * ( 1 - K^((BHage/50) * b3) )^( (SI - 1.3)^b4 )

where
  K = 1 - [ (SI - 1.3) / ( b1 * (SI - 1.3)^b2 ) ]^( 1 / ( b3 * (SI - 1.3)^b4 ) )
```

- **Direction(s):**
  - Forward: predict H from SI and BHage (closed form above).
  - Inverse: predict SI from H and BHage (no closed form; solved numerically).
    The SAS macro solves SI by damped fixed-point iteration
    (`si0 = 20` start, `si1 = 1.3 + ((H - 1.3)/x1)^(1/b2)`,
    `x1 = b1*(1 - K^(BHage/50))^(b3*(SI-1.3)^b4)`, update `si0=(si0+si1)/2`,
    tol 1e-8). This implementation uses root-finding on
    `height(BHage, SI) - height = 0` instead (same solution).
- **Notes on form:** the exponent structure places `b3` inside the `1 - K^(...)`
  term as `(BHage/50)*b3` (see rendered eq. 1). Algebraically the SAS writes
  `1 - K^(BHage/50)` raised to `b3*(SI-1.3)^b4` — these are the **same** because
  `b3` factors out of the outer exponent; both forms give identical H (and the
  SAS is the authoritative computational reference). Base (reference) age = 50.
  The curve is constrained to pass through H = SI at BHage = 50.

## Parameters

The paper reports several fitted variants. The **recommended** equation is the
one fitted to data <= 100 years BH age for all of northern Ontario (Table 3
"Eq." row with 383 plots; identical to the Table 5 "all"/eq. 3 row). This is the
single set implemented, matching the SAS `%macro SI/HT_Carmean_2001`.

Source: Table 3, p. 5 (rendered to image) and Table 5, p. 7 ("all" row).

| Species | b1 | b2 | b3 | b4 | base_age | source |
|---------|--------|--------|--------|---------|----------|--------|
| Jack pine | 4.1459 | 0.6224 | 1.3723 | -0.0802 | 50 | Table 3 (383 plots, <=100 yr) / Table 5 all=eq.3 |

Other tabulated variants **not** implemented (available if wanted later):
- Table 3: eq. fitted <=80 yr (b1=4.1052, b2=0.6306, b3=1.3610, b4=-0.0800);
  eq. fitted all ages incl. >100 yr (b1=4.3477, b2=0.6099, b3=1.3281, b4=-0.0784).
- Table 4 site-class equations (poor/medium/good).
- Table 5 per-region equations (NW / NC / NE / NO), computation & verification.

- **Species -> NFI mapping:** jack pine -> `PINU.BAN`. (SAS species code `JP`.)
- **Cross-check vs SAS macro** (`SK_SiteIndex_SAS_macros_with_BHAge_20220409.sas`,
  `%macro SI/HT_Carmean_2001`, lines 379-473): confirms the four coefficients
  digit-for-digit (`b1=4.1459; b2=0.6224; b3=1.3723; b4=-0.0802`) and base age
  `Tr = 50`.
- **WARN Flagged / uncertain values:** none. All four coefficients are legible in
  the Table 3 render and match Table 5 and the SAS macro exactly.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No worked numeric example or
  reference height table appears in the paper (only distribution tables and
  figures). The SAS macro is a faithful independent implementation and serves as
  the fidelity benchmark: reproduce its forward (HT) and inverse (SI) computation
  across a grid of ages/site indices via `tmp/generate_si_carmean2001_comparison_values.R`.
- **Also** verify the built-in constraint H(BHage=50) = SI numerically, and
  cross-check magnitude/monotonicity against the sibling `si_carmean2006`
  (same model form, different species/region) as a plausibility comparison.
