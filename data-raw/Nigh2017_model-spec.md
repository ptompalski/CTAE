# Model spec: si_nigh2017

> Human-reviewed extraction artifact for the Nigh (2017) lodgepole pine g-GADA
> site index model. Values copied digit-for-digit from the source; provenance
> noted per value. Nothing flagged uncertain.

## Source

- **Citation:** Nigh, G.D. 2017. Development of a lodgepole pine site index
  model with the grounded-Generalized Algebraic Difference Approach (g-GADA).
  Prov. B.C., Victoria, B.C. Res. Rep. 31.
- **BibTeX key:** @Nigh2017
- **Document in `sources/`:** `sources/site_index/Nigh2017_Rr31.pdf`
- **Model family:** si
- **Target function:** `si_nigh2017`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia
- **Species covered (NFI codes):** lodgepole pine (*Pinus contorta* var.
  *latifolia*), NFI code `PINU.CON`.
- **Valid input ranges:** fitting data breast-height age 46–141 yr, height
  13.44–34.32 m (Table 1, p. 3). The SI→β0 cubic is valid for site index
  5–30 m (base age 50); the source warns against applying it outside 5–30 m.
- **Age basis:** breast-height age (BHA, years).
- **Base age:** site index = site height at breast-height age 50.
- **Caveats:**
  - g-GADA localizes on a tree-specific asymptote parameter `β0`, not directly
    on SI. Predicting SI from a (BHA, height) pair requires numerically
    calibrating `β0`, then evaluating the height curve at BHA 50.
  - The SI→β0 cubic (below) is only applicable for base age 50 and SI 5–30 m,
    with a stated maximum β0 error of 16 cm (Discussion, p. 18).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| ht | site height | m |
| bha | breast-height age | years |
| SI | site index (site height at BHA 50) | m |
| β0 | tree-specific asymptote parameter (localizing param) | — (~m scale) |
| β10, β11, β20, β21 | global g-GADA parameters | — |

## Model form(s)

Fitted g-GADA model (Equation 4, and restated in the Discussion, p. 17):

```
ht = 1.3 + β0 * (1 - exp((β10 + β11*β0) * (bha - 0.5)))^(β20 + β21*β0)
```

with the fitted global parameters substituted (Table 2, p. 11):

```
ht = 1.3 + β0 * (1 - exp((-0.009737 - 0.0003742*β0) * (bha - 0.5)))^(1.5521 - 0.01308*β0)
```

Direct SI → β0 conversion for base age 50 (Discussion, p. 18):

```
β0 = 0.39374 + 2.2169*SI - 0.047173*SI^2 + 0.00060620*SI^3     (SI in m, BHA-50 base; valid SI 5-30 m)
```

Analytic first derivative of ht w.r.t. β0 (Appendix 1, p. 24), used for
numeric β0 calibration via the secant method:

```
T  = 1 - exp(-(0.009737 + 0.0003742*β0) * (bha - 0.5))
β2 = 1.5521 - 0.01308*β0
dht/dβ0 = β0 * T^β2 * ( -0.01308*ln(T)
            - 0.0003742*(bha - 0.5) * β2 * (T - 1)/T ) + T^β2
```

- **Direction(s) supported:**
  - **Predict height** from `age` + `si`: SI → β0 (cubic) → Equation 4.
  - **Predict SI** from `age` + `height`: numerically calibrate β0 to the
    (BHA, height) pair (root-find / secant on the SS derivative, Appendix 1),
    then evaluate Equation 4 at BHA 50 to get SI.
- **Notes on form:** single species, single global parameter set. The cubic
  SI→β0 conversion is the paper's own recommended practitioner shortcut for
  base-age-50 agencies; using it makes `height = si` hold at BHA 50 to within
  the paper's ~16 cm tolerance rather than exactly.

## Parameters

- **Source table(s):** Table 2, p. 11 (g-GADA global parameters β10, β11, β20,
  β21); Discussion p. 18 (SI→β0 cubic coefficients).
- **Species → NFI mapping:** lodgepole pine (Pli) → `PINU.CON`.
- **⚠ Flagged / uncertain values:** none. Two spot checks against the rendered
  Table 2 image: β11 = -0.0003742 (Std.err 0.00006312) and β21 = -0.01308
  (Std.err 0.001489), both confirmed. Row-to-parameter alignment confirmed
  against the g-GADA block of the table.

Fitted global g-GADA parameters (Table 2, p. 11):

| parameter | estimate | std_err |
|-----------|----------|---------|
| β10 | -0.009737 | 0.001814 |
| β11 | -0.0003742 | 0.00006312 |
| β20 | 1.5521 | 0.04272 |
| β21 | -0.01308 | 0.001489 |

SI→β0 cubic (Discussion, p. 18): c0 = 0.39374, c1 = 2.2169,
c2 = -0.047173, c3 = 0.00060620.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** Partial. The paper gives no
  worked (age, SI, height) example table, but it does provide the SI→β0 cubic
  with a stated maximum error of 16 cm at BHA 50. This supports a fidelity-style
  self-consistency check: for SI in 5–30 m, predicting height at BHA 50 must
  reproduce SI to within ~16 cm. Verified during extraction (max |error| ≈ 10 cm
  across SI 5,10,...,30).
- **Cross-check (always):** compare against `si_nigh2002` and other existing BC
  `si_*` models for overlapping ages — expect similar magnitude/shape (different
  species, so not identical). Report discrepancies.
- Not a pure fidelity benchmark (no published height table); note "no published
  worked-example table" in NEWS and rely on the cubic-consistency + cross-check.
