# Model spec: si_cieszewski1993

> Human-reviewed extraction artifact for the Cieszewski, Bella & Yeung (1993)
> preliminary variable-age site-index height-growth model for Saskatchewan.
> Source is a **scanned, image-only PDF** (no usable text layer); all values were
> transcribed from rendered page images (300-400 dpi). Flagged items marked WARN.

## Source

- **Citation:** Cieszewski, C.J., Bella, I.E., and Yeung, D.P. (1993).
  *Preliminary site-index height growth curves for eleven timber species in
  Saskatchewan.* Draft unpublished project report, Canada-Saskatchewan
  Partnership Agreement in Forestry. Natural Resources Canada - Canadian Forest
  Service, Prince Albert, Saskatchewan.
- **BibTeX key:** @Cieszewski1993
- **Document in `sources/`:** `sources/site_index/Cieszewski_etal_1993_SI_SK_.pdf`
- **Model family:** si
- **Target function:** `si_cieszewski1993`

## Scope / domain of applicability

- **Jurisdiction / region:** Saskatchewan (SK).
- **Species covered (11, NFI codes):** ABIE.BAL, POPU.BAL, PICE.MAR, PINU.BAN,
  PINU.CON, ACER.NEG, POPU.TRE, LARI.LAR, BETU.PAP, ULMU.AME, PICE.GLA.
- **Valid input ranges:** curves plotted over breast-height age 0-140 yr; the
  "supported" (non-extrapolated) range varies by species/SI class on each figure.
  Source stresses the models are preliminary and should be applied only within the
  range of the original data; extrapolation only with local support.
- **Age basis:** **breast-height (BH) age**, years. Data screening required a
  minimum BH age of 50 yr per tree.
- **Base age:** the model is a *variable-age* formulation. Site index is not a
  fixed model input; instead a reference height `hx` at a reference age `x` defines
  the curve. Setting the reference age `x = 50` yields the conventional SI
  (height at BH age 50). The `50` constant in `d = b / 50^a` is fixed in the
  published equation (`Age_SI = 50`).
- **Caveats:** "Preliminary" / "draft unpublished" report. Coefficients estimated
  by nonlinear least squares (customized SHAZAM) on decadal (10-yr interval) height
  values from stem-analysis data. D.P. Yeung is a listed author but (per a
  handwritten margin note) not credited on the manuscript routing form.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H (predHt) | computed tree height at prediction age `t` | m |
| t | prediction age (breast-height age) | years |
| hx | known height above breast height at reference age (= ObsHt - 1.3) | m |
| x | reference age of `hx` (breast-height age) | years |
| a | fitted exponent coefficient (per species) | dimensionless |
| b | fitted scale coefficient (per species) | (units per model) |
| d | derived constant `b / 50^a` | m |

Note: `1.3` m is breast height. Observed/known **total** height enters as
`hx = ObsHt - 1.3`; predicted total height adds `1.3` back.

## Model form(s)

Working form used in the study is eq. [2] (a simplification of eq. [1], the
Cieszewski & Bella 1989 variable-age polymorphic model). The unambiguous
algorithm is given by the paper's own SAS / FORTRAN / spreadsheet code (pp. 4-5):

```
d      = b / 50^a
hxRoot = hx + sqrt( (hx - d)^2 + 4*b*hx / x^a )
predHt = (hxRoot + d) / ( 2 + 4*b / t^a / (hxRoot - d) ) + 1.3
```

where `hx = ObsHt - 1.3`, `x` = age of the reference height, `t` = prediction age.

- **Direction(s):**
  - Predict height at any age `t` from a reference (height, age) pair -> direct.
  - Predict site index = height at reference age 50 -> evaluate at `t = 50`.
  - Invert to site index from an observed (age, height): pass `x = observed age`,
    `hx = observed height - 1.3`, and evaluate at `t = 50` (closed form; the
    reference point *is* on the curve, so at `t = x` it returns the input height
    exactly). No root-finding required.
- **Notes on form:** single functional form; per-species differences captured
  entirely by `(a, b)`. Verified numerically that `predHt(t = x) == ObsHt`
  (reference point lies on its own curve) and that curves are monotonic in age.

## Parameters

Per-species `a`, `b` (plus fit diagnostics `N`, `ME`, `SE`) are printed in the
inset box of each species' height-growth figure (one figure per species,
pp. 7-17 of the report; PDF pages 8-18). `N` matches Table 1 "N (after
screening)", confirming row-to-species alignment.

- **Source table(s):** per-species figure inset boxes (11 figures).
- **Species -> NFI mapping (source 2-letter code -> NFI):**
  BF -> ABIE.BAL, BP -> POPU.BAL, BS -> PICE.MAR, JP -> PINU.BAN,
  LP -> PINU.CON, MM -> ACER.NEG, TA -> POPU.TRE, TL -> LARI.LAR,
  WB -> BETU.PAP, WE -> ULMU.AME, WS -> PICE.GLA.
- **WARN / flagged values (resolved via SAS macro cross-check):**
  - **JP `a` = 1.1872291**: RESOLVED. The figure inset reads `1.1872291`; the
    worked example (p. 4-5) uses `1.178713`. The companion SAS macro file
    (`SK_SiteIndex_SAS_macros_with_BHAge_20220409.sas`, NRCan, 2022-04-09)
    hardcodes `a = 1.1872291; b = 1358.819` for JP, confirming the figure value.
    The example's `a` was illustrative/older.
  - **WB `a` = 1.244998**: the scan's final digit is ambiguous (reads 6 or 8 even
    at 600 dpi). The SAS macro gives `a = 1.244998`; adopted that value.
  - All other 9 species' `a` and all 11 `b` values match the SAS macro
    **digit-for-digit**. See cross-check note below.

## Cross-check against companion SAS macros

`sources/site_index/SK_SiteIndex_SAS_macros_with_BHAge_20220409.sas` (NRCan
Saskatchewan SI macros, 2022-04-09) contains `%SI_Ciesz_1993` and
`%HT_Ciesz_1993` macros implementing this exact model. Independent corroboration:

- **Model form matches** eq. [2] / the paper's own code, with base age fixed at
  `Tr = 50`. `HT_` predicts height from SI-at-age-50; `SI_` predicts
  SI-at-age-50 from an observed (BH age, height) pair. Both are closed-form (no
  root-finding), confirming the model is analytically invertible when the
  reference age is the base age 50.
- **All 11 (a, b) pairs match** my extracted CSV digit-for-digit (after adopting
  the SAS values for the two ambiguous cells above).
- **Species aliasing in the SAS code** (same coefficients shared across codes):
  `LP = RP = SP`; `MM = GA = BO`; `TA = PC`; `TL = SL`. These are SK inventory
  species-code aliases, not additional NFI species. The 11 NFI-mapped rows in the
  CSV remain the authoritative parameter set; aliasing is a downstream code
  concern, not a parameter change.

### SAS `HT_Ciesz_1993` (height from SI at base age 50)
```
Tr = 50; hx = si - 1.3;
hxroot = ((hx - b/Tr**a)**2 + 4*b*hx/Tr**a)**0.5;
ht = 1.3 + (hx + b/Tr**a + hxroot) / (2 + (4*b/bhage**a) / ((hx - b/Tr**a) + hxroot));
```

### SAS `SI_Ciesz_1993` (SI at base age 50 from observed BH age + height)
```
Tr = 50; hxs = height - 1.3; d = b/Tr**a;
c = (hxs - d)**2 + 4*b*hxs/bhage**a;
hxroots = hxs + c**0.5;
si = (d + hxroots) / (2 + (4*b/Tr**a)/(hxroots - d)) + 1.3;
```

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** Partial. The report gives a
  worked example (JP-family `a = 1.178713`, `b = 1358.819`, reference height 12 m
  at age 50) via SAS/FORTRAN/spreadsheet code. This anchors the **algorithm**
  (reproduced: at `t = 50` the curve returns 12.000 m; monotonic elsewhere) but
  uses an `a` that differs from the published JP figure coefficient.
- **Recommended benchmark:** encode the worked-example algorithm as a fidelity
  test using the example's own coefficients (`a = 1.178713`, `b = 1358.819`),
  checking `predHt(t = 50, hx = 12, x = 50) == 12` and a few off-reference ages
  computed independently. Separately, a plausibility check on the published
  per-species coefficients: monotonic increasing height with age, `height = si`
  at age 50, and comparable magnitude to `si_cieszewskibella1991` (same model
  family, overlapping species PINU.CON/PICE.GLA/PICE.MAR/POPU.TRE).
- No per-species reference height/age tables are printed (only curves), so
  species-level predictions are validated at the **plausibility** tier. Flag
  "no per-species source benchmark" in NEWS.
