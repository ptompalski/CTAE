# Model spec: si_batho2014

> Human-reviewed extraction artifact for the Batho & García (2014) lodgepole pine
> site index model. Key constants verified digit-for-digit against a 300-dpi
> render of the equation page (`tmp/batho2014_eqs3.png`, `tmp/batho2014_eqs4.png`).

## Source

- **Citation:** Batho, A., and García, O. 2014. A Site Index Model for Lodgepole
  Pine in British Columbia. Forest Science 60(5):982–987.
  https://doi.org/10.5849/forsci.13-509
- **BibTeX key:** @Batho2014
- **Document in `sources/`:** `sources/site_index/A_Site_Index_Model_for_Lodgepo.pdf`
- **Model family:** si
- **Target function:** `si_batho2014`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia — Sub-Boreal Spruce (SBS)
  biogeoclimatic zone (northern half of the Interior Plateau).
- **Species covered (NFI codes):** lodgepole pine, *Pinus contorta* var.
  *latifolia* → `PINU.CON`.
- **Valid input ranges:** fitted over roughly 0.5–192 y breast-height age and
  ~1.3–35 m top height (Table 1); site index range ~7–26 m. No hard bounds
  stated; extrapolation beyond the data is cautioned against for planted stands.
- **Age basis:** breast-height age (rings at 1.3 m). Convention implies age 0.5 y
  at breast height, i.e. `t0 = 0.5`, `H0 = 1.3`.
- **Base age:** 50 years breast-height.
- **Caveats / assumptions:** final model is the *Power combined* fit pooling
  natural + planted PSP and stem-analysis data. Height is **top height** (m),
  U-estimator of García & Batho (2005).

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H  | top height at age t | m |
| H0 | top height at reference age t0 | m |
| t  | breast-height age | years |
| t0 | reference breast-height age | years |
| S  | site index (top height at base age 50) | m |
| q  | local site parameter (per-plot) | — |
| a_q | asymptote as a function of q | m |

## Model form(s)

Bertalanffy–Richards, Power combined parametrization. Global constants:
c = 0.8297 (shape exponent), and a_q = 12313 * q^1.645.

Height transition (Eq 3), predicts H at age t from a known (t0, H0):

```
a_q = 12313 * q^1.645                                              (Eq 4)

H = a_q * ( 1 - (1 - (H0/a_q)^0.8297) * exp(-q*(t - t0)) )^(1/0.8297)   (Eq 3)
```

Site index (Eq 5), substituting t0 = 0.5, H0 = 1.3, t = 50 (so t - t0 = 49.5):

```
S = a_q * ( 1 - (1 - (1.3/a_q)^0.8297) * exp(-49.5*q) )^(1/0.8297)     (Eq 5)
```

Recover q from any two points (t0, H0) and (t, H) by fixed-point iteration
(Eq 6):

```
q <- ( ( (H^0.8297 - H0^0.8297 * exp(-q*(t - t0))) / (1 - exp(-q*(t - t0))) )^(1/0.8297) / 12313 )^(1/1.645)   (Eq 6)
```

Note: the paper states the Hu & García (2010) closed iteration does not converge
here; Eq 6 is the recommended iteration. For SI-from-(age, height), a root-find
on Eq 5/Eq 3 (via `uniroot` on q) is equivalent and robust.

- **Directions supported:**
  1. height from (age, q or SI) — Eq 3 with q, or solve S→q then Eq 3;
  2. site index from (age, height) — solve for q (Eq 6 / root-find), then Eq 5;
  3. q from (age, height) — Eq 6.
- **Notes on form:** single global form; no per-species/per-subregion variants.

## Parameters

Global scalar constants only — **no coefficient table / CSV needed**. All three
constants appear in-text (Eqs 3–6):

- `c = 0.8297` (shape exponent)
- `k = 12313` (a_q scale)
- `p = 1.645` (a_q exponent on q)

- **Source location:** p. 985, Eqs 3–6 (verified against 300-dpi render).
- **Species → NFI mapping:** *Pinus contorta* var. *latifolia* → `PINU.CON`.
- **⚠ Flagged / uncertain values:** none — all constants legible and confirmed.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** No. The paper reports only
  residual statistics (Table 4) and graphical site-index curves (Figs 3–4); no
  worked numeric example (age/height → SI) is tabulated.
- **Plausibility (Tier 2) comparison:**
  - Existing same-family functions: `si_thrower1994`, `si_nigh2016` (BC
    lodgepole pine), and internal round-trip consistency (SI→height→SI).
  - Expected relationship: monotonic increasing height in age, SI within the
    fitted ~7–26 m range; broadly similar magnitude to Thrower (1994), which the
    paper compares against in Fig. 5 (curves close: e.g. 27.5 vs 27.6 at high
    site).
  - ⚠ Ships with **no source benchmark** — note in NEWS.
```
