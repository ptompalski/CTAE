# Model spec: si_alemdag1991

> Human-reviewed extraction artifact for Alemdag (1991), national site-index and
> height-growth curves for white spruce. All coefficients verified against the
> rendered page images (Table 3 crop at 300 dpi), not the PDF text layer (the
> source PDF has essentially no usable text layer — image-only scan with a
> watermark overlay). Both equation directions were verified for internal
> consistency (see Benchmark plan).

## Source

- **Citation:** Alemdag, I.S. 1991. National site-index and height-growth curves
  for white spruce growing in natural stands in Canada. Canadian Journal of
  Forest Research 21(10): 1466–1474.
- **BibTeX key:** @Alemdag1991
- **Document in `sources/`:**
  `sources/site_index/alemdag-2011-national-site-index-and-height-growth-curves-for-white-spruce-growing-in-natural-stands-in-canada.pdf`
- **Model family:** si
- **Target function:** `si_alemdag1991`

## Scope / domain of applicability

- **Jurisdiction / region:** National (Canada), Yukon to Newfoundland. A single
  set of "combined" national curves; data from the two territories and eight
  provinces (no data from British Columbia or Nova Scotia). Regions pooled:
  Y.T. & N.W.T., Prairies (AB, SK, MB), Ontario & Quebec, Atlantic (NB, PE, NL).
- **Species covered (NFI codes):** white spruce, *Picea glauca* (Moench) Voss →
  `PICE.GLA`.
- **Valid input ranges:** natural, unmanaged, even-aged stands; dominant and
  codominant crown classes. Breast-height ages 10–150 years; site indices
  ~5–25 m; heights ~1.3–35 m (Tables 1–2; Figs. 5–6). Model is defined only for
  heights and site indices above breast height (1.30 m).
- **Age basis:** breast-height age A (years), i.e. rings counted at 1.30 m.
- **Base age:** index age A1 = 50 years (breast-height age). Site index S is the
  total tree height at A1 = 50.
- **Caveats / assumptions:** total tree height (not top height); site index
  expressed as height at 50 yr BH age; national pooling means regional curves
  may differ modestly (paper judged differences not practically significant).
  Ontario & Quebec were represented by only 33 sample trees.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| A  | breast-height age | years |
| A1 | index (base) age = 50 | years |
| H  | total tree height | m |
| S  | site index = total height at A1 = 50 | m |
| c1,c2,c4,c5 | site-index equation (Model [9]) coefficients | — |
| b1,b2,b4,b5 | height-growth equation (Model [4]) coefficients | — |

Both equations use `H - 1.30` and `S - 1.30` (heights measured above breast
height, 1.30 m), because age is measured at breast height.

## Model form(s)

Two independently fitted "modified Chapman–Richards" equations for the combined
national data. They are NOT exact inverses of each other (fitted separately),
but each is internally self-consistent (S = H at A = 50).

### Site index from age + height — Model [9] (final national SI equation)

The primal reciprocal form (eq. [6]) is
```
1/(S - 1.30) = c1 (H - 1.30)^c2  *  [1 - exp(c3 A)]^( c4 (H - 1.30)^c5 )
```
Conditioning S = H at the index age A1 (eq. [7]) makes c3 a function of H;
writing m = exp(c3 A1) (eq. [8]) and solving for S gives the four-parameter form:

```
[9]  S = 1.30 + 1 / [ c1 (H - 1.30)^c2 * ( 1 - m^(A/A1) )^( c4 (H - 1.30)^c5 ) ]

where (eq. [8])
     m = 1 - [ 1 / ( c1 (H - 1.30)^(1 + c2) ) ]^( 1 / ( c4 (H - 1.30)^c5 ) )
```

- `A1 = 50`.
- At `A = A1`: `m^(A/A1) = m`, and the construction guarantees `S = H`.

### Height from age + site index — Model [4] (final national height equation)

Newnham's constrained Chapman–Richards form (eq. [4]):

```
[4]  H = 1.30 + b1 (S - 1.30)^b2 * ( 1 - k^(A/A1) )^( b4 (S - 1.30)^b5 )

where (eq. [5])
     k = 1 - [ (S - 1.30) / ( b1 (S - 1.30)^b2 ) ]^( 1 / ( b4 (S - 1.30)^b5 ) )
```

- `A1 = 50`.
- At `A = A1`: `k^(A/A1) = k`, and the construction guarantees `H = S`.

- **Direction(s):** S ← (A, H) via Model [9]; H ← (A, S) via Model [4].
- **Notes on form:** both directions are closed-form (no numerical solving). The
  paper also presents a discarded linear method (Models [1]/[2]); only Models
  [9]/[4] with the combined-data coefficients are the recommended national
  equations (paper Conclusions, p. 1473).

## Parameters

Single national parameter set (combined data), Table 3, p. 1467. Copied
digit-for-digit from the rendered Table 3 crop:

### Model [9] — site index (c coefficients)

| name | value | source locator |
|------|-------|----------------|
| c1 | 40.6506  | Table 3, p. 1467 (rendered crop) |
| c2 | 5.6605   | Table 3, p. 1467 |
| c4 | 1.2544   | Table 3, p. 1467 |
| c5 | -0.1567  | Table 3, p. 1467 |

### Model [4] — height growth (b coefficients)

| name | value | source locator |
|------|-------|----------------|
| b1 | 16.5383 | Table 3, p. 1467 (rendered crop) |
| b2 | 0.2336  | Table 3, p. 1467 |
| b4 | 2.8896  | Table 3, p. 1467 |
| b5 | -0.2556 | Table 3, p. 1467 |

- Index age `A1 = 50` (Table 2 text, p. 1467: "S ... at a breast-height age (A)
  of 50 years (index age, A1)").
- **Species → NFI mapping:** white spruce → `PICE.GLA`.
- **⚠ Flagged / uncertain values:** none. All eight coefficients read cleanly
  from the Table 3 crop; asymptotic 95% CIs in the same table corroborate the
  point estimates (e.g. c1 40.6506 ∈ 39.67–41.63; b1 16.5383 ∈ 14.82–18.25).

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No. The paper prints no
  worked-example prediction grid. Tables 5 and 6 report *biases* (mean residuals
  by age/height and age/site classes), not fitted predictions, so they cannot
  serve as an exact numeric benchmark.
- **Internal consistency check (strong self-benchmark):**
  - Model [4] at `A = 50` returns `H = S` exactly (verified: S = 15, 20, 25 →
    15.00, 20.00, 25.00).
  - Model [9] at `A = 50` returns `S = H` (verified: H = 5, 10, 15, 20, 25 →
    5.00, 10.00, 15.00, 19.997, 24.90; small departures at high H reflect the
    rounded coefficients and the c3→m conditioning, consistent with the paper's
    reported near-zero overall bias of 0.06 m).
- **Plausibility (Tier 2) comparison + figure check:**
  - Model [9] SI curves match Fig. 3 shape: for H = 15 m, S ≈ 28 at A = 10,
    15 at A = 50, ~7 at A = 150; for H = 25 m, ~25 at 50 declining to ~11.5 at
    150.
  - Model [4] height curves match Fig. 6 shape: for S = 15 m, H ≈ 15 at A = 50,
    rising to ~28.6 at A = 150.
  - Cross-check against existing white-spruce SI functions in the package
    (`si_thrower1994`, `si_huang1994`, `si_hugarcia2009`, `si_carmeanhahn1981`)
    over overlapping BH ages/site indices — expect similar magnitude and
    monotonic behavior, not identical values (different data/regions).
- ⚠ Ships with **no printed source prediction grid**; validation rests on the
  exact self-consistency identities plus figure/same-family plausibility checks.
  Note "no source benchmark" in NEWS.
