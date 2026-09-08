# Model spec: si_hugarcia2009

> Human-reviewed extraction artifact for the Hu & García (2009) interior spruce
> height-growth / site-index model. All coefficients verified against the
> rendered page images (crops in `tmp/`), not just the PDF text layer.

## Source

- **Citation:** Hu, Z., and García, O. 2010. A height-growth and site-index
  model for interior spruce in the Sub-Boreal Spruce biogeoclimatic zone of
  British Columbia. Canadian Journal of Forest Research 40(6): 1175–1183.
  (Published online 2009; journal issue Vol. 40, 2010.)
- **BibTeX key:** @HuGarcia2009
- **Document in `sources/`:** `sources/site_index/Hu_Garcia_2009_SI_wS_BC.pdf`
- **Model family:** si
- **Target function:** `si_hugarcia2009`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia interior, Sub-Boreal Spruce (SBS)
  biogeoclimatic zone.
- **Species covered (NFI codes):** interior spruce complex (white spruce
  `PICE.GLA`, hybrids with Engelmann `PICE.ENG`). The paper treats these as a
  single "interior spruce" entity; primary mapping is `PICE.GLA`.
- **Valid input ranges:** even-aged, spruce-dominated stands; data span roughly
  breast-height ages up to ~150+ years and site indices ~10–30 m (Figs. 1–4).
  Model is defined only for heights ≥ 1.3 m (post-breast-height).
- **Age basis:** breast-height age (BH age). The curve origin is H0 = 1.3 m at
  t0 = 0.5 years BH age.
- **Base age:** 50 years breast-height age (BC convention). Site index S is the
  predicted top height at 50 yr BH age.
- **Caveats / assumptions:** top height (not mean height); even-aged stands;
  SBS zone macroclimate. Height growth below breast height is not modelled
  (paper notes optional use of Nigh 1999 growth-intercept models for ages 1–50).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H, H0 | top height (H0 = height at curve origin) | m |
| t, t0 | breast-height age (t0 = 0.5 yr at origin) | years |
| S | site index = top height at 50 yr BH age | m |
| q | local (site) parameter estimated per plot | (rate, ~yr⁻¹) |
| a | asymptote (site-dependent via q) | m |
| b | rate parameter (= q, site dependent) | yr⁻¹ |
| c | shape parameter (global constant) | dimensionless |

## Model form(s)

Bertalanffy–Richards growth model. Differential form (eq. 1) integrates to
the projection form (eq. 2):

```
[2]  H = a * { 1 - [ 1 - (H0/a)^c ] * exp[ -b * (t - t0) ] }^(1/c)
```

Selected model = "combined model 4": a polymorphic parametrization with

```
a = 283.9 * q^0.5137      (site-dependent asymptote)
b = q                     (site-dependent rate)
c = 0.5829                (global constant shape)
```

with fixed origin (t0, H0) = (0.5 yr, 1.3 m).

**Height from age + site (site index S):**
1. Solve for q from S using eq. 3 (numerically — no closed form):
   ```
   [3]  S = a * { 1 - [ 1 - (1.3/a)^c ] * exp(-49.5 * b) }^(1/c),   a = 283.9 q^0.5137, b = q
   ```
   The paper's implementation uses the fixed-point iteration:
   ```
   q_{n+1} = (1 / (t - 0.5)) * ln[ (1 - (1.3/a)^0.5829) / (1 - (H/a)^0.5829) ]
   ```
   evaluated with (t, H) = (50, S) and a = 283.9 * q_n^0.5137, starting q = 0.02,
   until q converges.
2. Given q (hence a, b), predict H at any BH age t via eq. 2 with (t0, H0)=(0.5, 1.3).

**Site index from age + height (invert):** solve q from an observed (t, H) pair
using the same iteration (with the observed t, H), then set S = height at t = 50
via eq. 2/eq. 3.

- **Direction(s):** height ← (BH age, S); S ← (BH age, H); general projection
  H(t) ← (H0, t0, t) once q is known.
- **Notes on form:** "truly polymorphic" (advanced polymorphic): asymptote and
  time-scale both vary with site; c is a shared global constant. q must be found
  numerically; the paper gives a Newton (García 1996) or fixed-point iteration.

## Parameters

Global constants only (no per-species table — single interior-spruce entity):

| name | value | source locator |
|------|-------|----------------|
| a_coef (283.9) | 283.9 | §4.2 Model, p. 1179 (text + rendered crop `tmp/hu_garcia_eq_crop.png`) |
| a_exp (β on q) | 0.5137 | §4.2 Model, p. 1179 (same) |
| c (shape) | 0.5829 | §4.2 Model, p. 1179 (same) |
| H0 (origin height) | 1.3 | eq. 2 text, p. 1178 |
| t0 (origin age) | 0.5 | eq. 2 text, p. 1178 |
| base_age | 50 | §4.1 / eq. 3, p. 1178–1179 |

- **Source table(s):** values are stated inline in §4.2 (p. 1179), not in a
  numbered table. Verified against rendered image crops in `tmp/`.
- **Species → NFI mapping:** interior spruce → `PICE.GLA` (primary), with
  `PICE.ENG` hybrids acknowledged.
- **⚠ Flagged / uncertain values:** none. All three fitted coefficients
  (283.9, 0.5137, 0.5829) read cleanly and consistently from text and image.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No exact worked-example
  table of (age, S) → height is printed. The paper gives figures (Figs. 3–6)
  and residual summaries (Table 2), but no numeric prediction grid.
- **Internal consistency check (usable as a strong self-benchmark):** the model
  is self-referential — for a given S, solving q from S (eq. 3) and then
  evaluating eq. 2 at t = 50 must return H = S. This round-trip identity, plus
  reproducing the reported iteration constants, provides a tight correctness
  check on the q-solver.
- **Plausibility (Tier 2) comparison:** compare against existing BC/interior
  spruce SI models in the package (e.g. `si_thrower1994`, `si_huang1994` for
  white spruce) over overlapping BH ages and site indices — expect similar
  magnitude and monotonic height-in-age / height-in-S behavior, not identical.
- ⚠ This model ships with **no printed source prediction grid**; validation
  rests on the exact self-consistency identity (Tier 1-like) plus a same-family
  plausibility check. Note in NEWS.
