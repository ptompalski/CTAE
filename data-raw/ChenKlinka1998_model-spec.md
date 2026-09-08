# Model spec: si_chenklinka1998

> Human-reviewed extraction artifact. Values transcribed digit-for-digit from the
> rendered PDF pages (the text layer dropped all math notation, so equations and
> coefficients were read from ~300 dpi page renders in `tmp/chenklinka/`).

## Source

- **Citation:** Chen, H.Y.H. & Klinka, K. (1998). Height growth curves and site
  index tables for subalpine fir, Engelmann spruce, and lodgepole pine in the ESSF
  zone of BC. *Scientia Silvica*, Extension Series, Number 15. Forestry Sciences
  Department, University of British Columbia, Vancouver.
- **BibTeX key:** @ChenKlinka1998
- **Document in `sources/`:** `sources/site_index/SSES015.PDF.pdf`
- **Model family:** si
- **Target function:** `si_chenklinka2000`

## Scope / domain of applicability

- **Jurisdiction / region:** British Columbia — Engelmann Spruce–Subalpine Fir
  (ESSF) biogeoclimatic zone.
- **Species covered (NFI codes):**
  - `ABIE.LAS` — subalpine fir (*Abies lasiocarpa*)
  - `PICE.ENG` — Engelmann spruce (*Picea engelmannii*)
  - `PINU.CON` — lodgepole pine (*Pinus contorta*)
- **Valid input ranges:** curves/tables published for breast-height age 5–150 yr
  and site index roughly 3–25 m (species-dependent, see tables pp. 3–4). No hard
  bounds stated in the text.
- **Age basis:** breast-height age (years).
- **Base age:** 50 years (breast-height). Site index S = top height at 50 yr b.h.a.
- **Caveats:** Models fit to two-thirds of 329 ESSF plots (165 subalpine fir, 90
  Engelmann spruce, 74 lodgepole pine) using a conditioned Chapman-Richards
  function; validated on the remaining third. Intended to replace biased
  low/mid-elevation surrogate models for high-elevation ESSF stands.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| H | top height | m |
| S | site index (top height at 50 yr breast-height age) | m |
| A | breast-height age | years |
| p | conditioning exponent (derived from S and coefficients) | — |
| e | base of the natural exponential function | — |
| ln | natural logarithm | — |

## Model form(s)

Single conditioned Chapman-Richards form, identical structure for all three
species; only the three coefficients b1, b2, b3 differ.

```
H = 1.3 + b1 * [ (S - 1.3)^b2 * (1 - e^(-b3 * A))^p ]

where:

        ln( (S - 1.3)^(1 - b2) / b1 )
p  =  ---------------------------------
           ln( 1 - e^(-b3 * 50) )
```

- **Direction(s):**
  - Forward (published): predict height H from breast-height age A and site index S.
  - Inverse: predict site index S from A and H. The source does not give a
    closed-form inverse; S must be obtained numerically (e.g. root-finding on the
    forward equation), consistent with how other `si_*` functions in this package
    handle non-invertible forms.
- **Notes on form:** `p` depends on S and the coefficients but not on A, so it is
  computed once per (species, S). At A = 50, `(1 - e^(-b3*50))^p` reduces so that
  H = S (the conditioning constraint), which the site-index tables confirm
  (row A = 50 equals the column's S value exactly).

## Parameters

Coefficients read from the three height-growth-model equations (pp. 1–2 of the
rendered PDF). One row per species.

- **Source location:** subalpine fir eqn — p. 1; Engelmann spruce & lodgepole pine
  eqns — p. 2.
- **Species → NFI mapping:** subalpine fir → `ABIE.LAS`; Engelmann spruce →
  `PICE.ENG`; lodgepole pine → `PINU.CON`.

| Species (source) | nfi_species | b1 | b2 | b3 |
|------------------|-------------|---------|---------|---------|
| Subalpine fir | ABIE.LAS | 5.88585 | 0.59774 | 0.01798 |
| Engelmann spruce | PICE.ENG | 6.47677 | 0.59383 | 0.01748 |
| Lodgepole pine | PINU.CON | 2.51526 | 0.89265 | 0.01759 |

- **⚠ Flagged / uncertain values:** none. All coefficients rendered clearly.

## Benchmark plan

- **Fidelity (Tier 1) reference values available?** Yes.
  - Site index tables (p. 3: subalpine fir & Engelmann spruce; p. 4: lodgepole
    pine) give H tabulated over breast-height age 5–150 yr (rows) and site index
    3/4–25 m (columns), to 0.1 m. These are the forward-direction reference values.
  - Plan: transcribe a subset of table cells per species into a comparison CSV and
    assert `si_chenklinka1998()` reproduces them within ~0.1 m (rounding of the
    published table). At A = 50 every cell must equal its column's S exactly.
- **Cross-check (always):** compare against existing BC interior `si_*` models for
  overlapping species (e.g. lodgepole pine / spruce site-index models such as
  `si_thrower1994`, `si_nigh*`) — expect similar magnitude and monotonic height
  increase with age; report divergences.
