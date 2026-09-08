# Model spec: si_nigh2004

> Human-reviewed extraction artifact. Source PDF: `tmp/si_papers/Nigh2004_Rr25.pdf`.

## Source

- **Citation:** Nigh, G.D. 2004. Juvenile Height Models for Lodgepole Pine and
  Interior Spruce: Validation of Existing Models and Development of New Models.
  Res. Rep. 25. B.C. Ministry of Forests, Forest Science Program, Victoria, B.C.
- **BibTeX key:** @Nigh2004
- **Document:** `tmp/si_papers/Nigh2004_Rr25.pdf` (26 pp.)
- **Model family:** si (juvenile height–age model, driven by site index)
- **Target function:** `si_nigh2004`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia (interior).
- **Species covered (NFI codes):**
  - Lodgepole pine (*Pinus contorta* var. *latifolia*) → `PINU.CON`
  - Interior spruce (*Picea glauca*, *P. engelmannii*, and their hybrid) →
    modelled as white spruce `PICE.GLA` (paper: white-spruce models applied to
    interior spruce).
- **Age basis:** **total age** (years). NOT breast-height age. The model is
  conditioned to predict height = 0 at total age 0.
- **Base age of SI:** site index as used here is the standard SI (m). SI for each
  sample tree was estimated with growth-intercept models (Nigh 1997, 2004).
  The juvenile models apply over roughly total age 0–15 yr (pine) / 0–20 yr
  (spruce) — the juvenile height range the models were fit for.
- **Zones (BEC):** BWBS, ESSF, ICH, IDF, MS, SBS, SBPS. A province-wide
  parameter set (model 3, Table 2) is the fallback where the zone is unknown or
  not sampled.
- **Caveats:** juvenile models; data from managed stands. Precision poor on a
  per-zone basis (small n). Province-wide set = average across sampled zones.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | tree (site) height | m |
| SI | site index | m |
| A | age | years (total age) |
| a1..a4 | model parameters | — |

## Model form(s)

Base model (equation 3):

```
H = a1 * SI * A^(a2 + a3 * SI) * a4^A
```

- **Direction(s):** predict **height** from age + SI directly. Inverting for SI
  (given age + height) has no closed form (SI appears in the exponent); solve
  numerically if height→SI is supported.
- **Zone effects (equations 4–6):** the province-wide parameters ai0 (Table 2)
  are adjusted by additive zone terms. After deleting/combining non-significant
  terms, the fitted parameter equations are:

  Lodgepole pine (eq. 5):
  ```
  a1 = a10 + a11*(IDF + SBPS) + a12*SBS
  a2 = a20 + a21*(BWBS + MS)  + a22*(ICH + SBPS)
  a3 = a30 + a31*ESSF + a32*ICH + a33*IDF
  a4 = a40 + a41*(BWBS + ICH + MS) + a42*SBS
  ```

  Interior spruce (eq. 6):
  ```
  a1 = a10 + a11*ICH + a12*IDF + a13*(MS + SBPS)
  a2 = a20 + a21*BWBS + a22*(ESSF + IDF) + a23*(ICH + MS + SBPS + SBS)
  a3 = a30 + a31*BWBS + a32*ICH + a33*(IDF + MS) + a34*SBS
  a4 = a40 + a41*(ESSF + SBS) + a42*ICH + a43*MS
  ```
  where each zone indicator is 0/1. The province-wide row uses all indicators = 0,
  i.e. a_i = a_i0.

## Parameters

Copied digit-for-digit from **Table 3** (p. 11 of the report; parameter sets
5 & 6), cross-checked against **Table 2** (p. 9; province-wide model 3, where
a1=a10, a2=a20, a3=a30, a4=a40 match exactly).

### Table 3 — Lodgepole pine (set 5)

| Name | Estimate |
|------|----------|
| a10 | 0.001424 |
| a11 | -0.0009260 |
| a12 | 0.0008032 |
| a20 | 1.801 |
| a21 | 0.07098 |
| a22 | 0.3509 |
| a30 | 0.01820 |
| a31 | -0.003024 |
| a32 | -0.01257 |
| a33 | 0.01581 |
| a40 | 0.9537 |
| a41 | -0.01083 |
| a42 | -0.02025 |

### Table 3 — Interior spruce (set 6)

| Name | Estimate |
|------|----------|
| a10 | 0.0009952 |
| a11 | 0.0005208 |
| a12 | -0.0006785 |
| a13 | -0.0008774 |
| a20 | 0.9842 |
| a21 | 0.2521 |
| a22 | -0.2893 |
| a23 | 0.5893 |
| a30 | 0.02943 |
| a31 | -0.008403 |
| a32 | -0.01388 |
| a33 | 0.02672 |
| a34 | -0.03586 |
| a40 | 1.017 |
| a41 | 0.03818 |
| a42 | -0.04231 |
| a43 | -0.07806 |

- **Species → NFI mapping:** lodgepole pine → `PINU.CON`; interior spruce →
  `PICE.GLA`.
- **⚠ Flagged / uncertain values:** none. Table 3 rendered cleanly; Table 2
  intercepts corroborate a_i0.

Representation choice: the CSV stores the final resolved **a1, a2, a3, a4** per
(species × zone) plus a `PROV` (province-wide) row, computed from eqs. 5/6 above.
This keeps runtime lookup a simple row select and is directly auditable against
the equations.

## Benchmark plan

- **Fidelity (Tier 1) reference values:** Figure 4 plots province-wide predicted
  height trajectories for SI 10, 15, 20, 25 m; values are graphical only (no
  numeric table of predictions). So there is **no exact numeric benchmark**.
- **Plausibility (Tier 2):** verify H=0 at A=0; monotonic increasing in A over the
  juvenile range; higher SI → greater height; province-wide row equals a_i0.
  Cross-check the province-wide pine curve against the older Nigh & Love model
  (eq. 1: `H = (-0.03993 + 0.004828*SI)*A^1.902*0.9645^A`) — expect same order of
  magnitude, with model 3 avoiding negative heights at low SI.
- ⚠ Ships with **no exact source benchmark** — note in NEWS.
