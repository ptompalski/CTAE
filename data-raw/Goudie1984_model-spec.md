# Model spec: si_goudie1984

> Human-reviewed extraction artifact. The **SAS macro is the reference** per the
> user; the PDF is a scanned image-only document used to confirm the model form,
> citation, and (for pine) the coefficients.

## Source

- **Citation:** Goudie, J.W. 1984. Height Growth and Site Index Curves for
  Lodgepole Pine and White Spruce and Interim Managed Stand Yield Tables for
  Lodgepole Pine in British Columbia. Final Report FY-1983-84. Research Branch,
  British Columbia Ministry of Forests, Victoria, B.C.
- **BibTeX key:** @Goudie1984
- **Document in `sources/`:** `sources/site_index/bib89331.pdf` (scanned;
  ~75 chars text layer total — OCR used to read prose/equations).
- **Reference implementation:** `sources/site_index/SK_SiteIndex_SAS_macros_with_BHAge_20220409.sas`,
  macros `%SI_Goudie_1984` (lines 664-709) and `%HT_Goudie_1984` (lines 711-750).
- **Model family:** si
- **Target function:** `si_goudie1984`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia (and Alberta; curves fit to BC + AB data).
- **Species covered (NFI codes):**
  - Lodgepole pine — `PINU.CON` (SAS labels: PL, LP, PJ, JP)
  - White spruce — `PICE.GLA` (SAS labels: WS, SW)
- **Valid input ranges:** BHA up to ~120-140 yr (curves plotted to age 140-160);
  SE increases with distance from base age (max ~1.76 m at age 120 for pine).
- **Age basis:** breast-height age (BHA), years.
- **Base age:** 50 years at breast height (site index = site height at BHA 50).
- **Caveats:**
  - SAS applies a 2004 modification giving height = 1.3 m at BHA 0.5 yr (the
    `- 0.5` shift on BHA and base age inside the logs). Domain guards in SAS:
    `bhage <= 0.5` or `height < 1.3` (SI mode) / `si < 1` (HT mode) return -1.
  - For pine the paper offers habitat-specific coefficients (eq. 7; dry vs wet).
    The SAS uses the **dry-site** coefficients (recommended by the author when no
    ecological information is available). This implementation follows the SAS: a
    single coefficient set per species, no habitat term.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| BHA | breast-height age | years |
| H   | site height | m |
| S / SI | site index (height at BHA 50) | m |
| H13 | H - 1.3 (height above breast height) | m |
| S13 | S - 1.3 (site index above breast height) | m |

## Model form(s)

Monserud/Dahms logistic height-age curve (paper eqs. 5-7), as implemented in SAS:

```
Tr  = 50                     # base age (BH)
x1  = 1 + exp(b1 + b2*ln(Tr  - 0.5) - b3*ln(SI - 1.3))
x2  = 1 + exp(b1 + b2*ln(BHA - 0.5) - b3*ln(SI - 1.3))

# Predict height from age + site index (HT_Goudie_1984):
H   = 1.3 + (SI - 1.3) * (x1 / x2)

# Predict site index from age + height (SI_Goudie_1984):
#   solve for SI in  H = 1.3 + (SI - 1.3) * (x1 / x2)
#   (SI appears both as multiplier and inside the log -> no closed form)
```

- **Direction(s):** predict height from age+SI; invert to predict SI from age+height.
- **Notes on form:** SAS solves the SI inverse with a fixed-point averaging
  iteration (`si0 = (si0 + si1)/2`, tol 1e-8). Numerically equivalent to
  root-finding on `H(BHA, SI) - H = 0`; this implementation uses `uniroot`
  (as in `si_nigh2002`), which solves the same equation.

## Parameters

Copied digit-for-digit from the SAS reference (lines 679-682 / 725-728), which
matches the PDF eq. 7 dry-site pine values (7.815, -1.285, 1.007).

| NFI code | SAS label | b1 | b2 | b3 | source |
|----------|-----------|----|----|----|--------|
| PICE.GLA | WS, SW | 9.794 | -1.466 | 1.287 | SAS l.680/726 |
| PINU.CON | PL, LP, PJ, JP | 7.815 | -1.285 | 1.007 | SAS l.682/728; PDF eq.7 (dry) |

- **Species -> NFI mapping:** WS/SW -> PICE.GLA; PL/LP/PJ/JP -> PINU.CON.
- **Flagged / uncertain values:** none. Pine coefficients cross-checked against
  PDF eq. 7 (dry-site) OCR. White spruce coefficients taken from the SAS
  reference (the PDF's spruce equation did not OCR cleanly, but the SAS is the
  designated authority).

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** Yes — the SAS macros are the
  designated reference. Generate expected (BHA, SI) -> H and (BHA, H) -> SI values
  by porting the exact SAS arithmetic in R (`tmp/generate_si_goudie1984_comparison_values.R`)
  and test `si_goudie1984` against them.
- At BHA 50 the curve is conditioned so H == SI; use as an exact check.
