# Model spec: si_nigh1998

> Human-reviewed extraction artifact for the western hemlock site-index system
> of Nigh (1998). Coefficients transcribed digit-for-digit from rendered page
> images (raw PDF text layer was unreliable). Flagged uncertainties marked WARN.

## Source

- **Citation:** Nigh, G.D. 1998. A system for estimating height and site index
  of western hemlock in the interior of British Columbia. The Forestry Chronicle
  74(4): 588–596.
- **BibTeX key:** @Nigh1998
- **Document in `sources/`:** `sources/site_index/nigh-2011-a-system-for-estimating-height-and-site-index-of-western-hemlock-in-the-interior-of-british-columbia.pdf`
  - ⚠ The **filename says 2011 but the paper is 1998** (footer: "JUILLET/AOÛT
    1998, VOL. 74, NO. 4, THE FORESTRY CHRONICLE"). Citation confirmed from the
    rendered text, per skill guidance.
- **Model family:** si (site index / height–age system)
- **Target function:** `si_nigh1998`
  - Note: `si_nighcourtin1998` already exists (a *different* 1998 paper, Nigh &
    Courtin). This is Nigh solo 1998. No name collision.

## Scope / domain of applicability

- **Jurisdiction / region:** Interior of British Columbia (BC). The most precise
  ecosystem–site index model (eq 7) is limited to the ICH biogeoclimatic
  subzone/variants sampled: ICHmc1, ICHmc1a, ICHmc2, ICHmw2, ICHmw3, ICHwk1.
- **Species covered (NFI code):** western hemlock, *Tsuga heterophylla* → `TSUG.HET`.
- **Data basis:** 44 stem-analysis plots; stands generally > 80 yr bha targeted.
- **Age basis:** breast-height age (bha), i.e. years since the tree reached
  breast height (1.3 m). The years-to-breast-height model converts bha ↔ total age.
- **Base / index age:** site index = top height (m) at **bha 50**.
- **Valid ranges (stated / implied):**
  - Height–bha curves plotted to bha 300; model fit on data to bha 50, so ages
    beyond ~50 are extrapolation (source notes max height may be slightly high in
    old stands).
  - Growth-intercept model defined for bha 1–50; coefficients tabulated only for
    bha 5, 10, 20, 30, 40, 50.
  - YTBH model: caution below SI 10 m (one influential low-SI observation inflates
    estimates there).
- **Caveats:** Height–bha and growth-intercept models assume trees are expressing
  site potential; may bias estimates in suppressed/damaged stands. Ecosystem–site
  index models apply in any stand.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | top height of the stand | m |
| A | breast-height age (bha) | years |
| SI | site index = top height at bha 50 | m |
| YTBH | years to grow from germination to breast height | years |
| GI | growth intercept (average annual height growth just above bh) | cm/yr |

## Model form(s)

### Eq 5 — Years-to-breast-height (YTBH) — Table 1

```
YTBH = b0 * SI^b1
     = 446.6 * SI^(-1.432)
```
- Converts bha ↔ total age (total age = bha + YTBH). Invertible for SI given YTBH.

### Eq 6 (fitted form of eq 2) — Height–bha (log-logistic) — Table 2

```
H = 1.3 + (SI - 1.3) *
    ( 1 + exp( b0 + b1*ln(49.5) + b2*ln(SI - 1.3) ) )
    ---------------------------------------------------
    ( 1 + exp( b0 + b1*ln(A - 0.5) + b2*ln(SI - 1.3) ) )
```
with b0 = 8.998, b1 = -1.434, b2 = -1.051.
- Conditioned to return SI at A = 50 (the 49.5 = 50 - 0.5, and A - 0.5 offset).
- **Direction:** predict H from (A, SI). Invert numerically to predict SI from
  (A, H) if an SI-from-height direction is wanted.

### Eq 3 / Eq 4 — Growth-intercept site index — Table 3

Eq 3 defines GI from measured heights (not needed if GI is supplied):
```
GI_{i,A} = (H_{i,A} - 1.3) / (A - A_{i,1}) * 100
```
Eq 4 predicts SI from GI, with a **separate (b1, b2) pair per bha**:
```
SI = 1.3 + exp(b1) * GI^b2
```
- ⚠ **Important fidelity note:** the text states parameter b1 "was transformed so
  that it appeared in the model as e^b1 (Ratkowsky 1983)". So the **tabulated b1 is
  on the log scale**; the implementation must use `exp(b1)` as the multiplier, not
  b1 directly. (Table 3 lists b1, b2 per bha.)
- **Direction:** predict SI from (GI, bha).

### Eq 7 — Ecosystem–site index, localized (ICH subzones + soil moisture) — Table 4

```
SI = 13.55
     - 6.41*mc1 - 7.11*mc1a - 0.28*mc2 + 3.49*mw2 + 2.69*mw3 + 0.00*wk1
     + 4.21*F   + 4.12*M    + 0.00*SD
```
where dummy = 1 for the matching level, 0 otherwise:
- subzone/variant: mc1, mc1a, mc2, mw2, mw3, wk1 (reference level = ICHmc1a? no —
  intercept 13.55 is the model constant; each listed subzone has its own dummy, so
  the "omitted"/reference combination is intercept-only. ⚠ Confirm which subzone is
  the baseline; equation lists a dummy for all six, implying the constant is the
  grand baseline. See review question below.)
- soil moisture regime: F (fresh), M (moist), SD (slightly dry). Reference moisture
  level is the omitted category (SD coefficient is 0.00).

### Eq 8 — Ecosystem–site index, general (soil nutrient regime only)

```
SI = 20.00 - 5.27*B - 3.01*C + 0.00*D
```
where soil nutrient regime dummy = 1 for the matching level:
- B (poor), C (medium), D (rich). D coefficient is 0.00 (rich = reference-ish).
- Applicable beyond the sampled ICH subzones (less precise than eq 7).

## Parameters

- **Source tables:** Table 1 (YTBH), p. 590/591; Table 2 (height–bha), p. 592;
  Table 3 (growth intercept), pp. 591–592; eqs 7 & 8 coefficients inline on p. 591;
  Table 4 gives ANOVA only (not needed for prediction).
- **Species → NFI mapping:** western hemlock → `TSUG.HET`.
- **⚠ Flagged / uncertain values:** none illegible. Two items to confirm in review:
  1. Growth-intercept `exp(b1)` transformation (see fidelity note above).
  2. Ecosystem-SI baseline category interpretation for eq 7 subzone dummies.

### Table 1 — YTBH (eq 5)

| param | estimate | std_error |
|-------|----------|-----------|
| b0 | 446.6 | 100.8 |
| b1 | -1.432 | 0.09327 |

RMSE 2.819.

### Table 2 — Height–bha (eq 6)

| param | estimate | std_error |
|-------|----------|-----------|
| b0 | 8.998 | 0.06582 |
| b1 | -1.434 | 0.01667 |
| b2 | -1.051 | 0.02090 |

RMSE 0.9404.

### Table 3 — Growth intercept (eq 4), b1 on log scale (use exp(b1))

| bha | b1 | b2 | rmse |
|-----|---------|--------|--------|
| 5  | 1.213  | 0.4708 | 3.673  |
| 10 | 1.072  | 0.5088 | 3.274  |
| 20 | 0.1969 | 0.7540 | 2.001  |
| 30 | -0.2943| 0.8860 | 1.243  |
| 40 | -0.4341| 0.9218 | 0.5632 |
| 50 | -0.7061| 1.002  | 0.04202|

### Eqs 7 & 8 — Ecosystem–site index dummy coefficients

Captured directly in the equations above (intercepts 13.55 and 20.00).

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** Yes — **Fig. 2** plots the
  height–bha model for SI = 5, 10, 15, 20, 25 m across bha 0–300. This gives a
  read-off fidelity check for the height model (loose figure-reading tolerance,
  e.g. ±1 m). More usefully, the height–bha model is **self-consistent by
  construction**: at A = 50, H must equal SI exactly (the conditioning). That is an
  exact analytic fidelity check (tolerance ~1e-8).
  - YTBH, growth-intercept, and ecosystem-SI models have no worked numeric example;
    they are exact-form transcriptions and can be checked against their own defining
    equations plus the H(A=50)=SI identity is not applicable. Validate these against
    hand-computed values from the transcribed coefficients (Tier 1 by direct formula)
    and cross-check magnitudes vs. existing `si_*` hemlock/BC models (Tier 2).
- **Cross-check (always):** compare `si_nigh1998` height–bha output against existing
  BC interior `si_*` functions for overlapping age/SI ranges and report discrepancies.
