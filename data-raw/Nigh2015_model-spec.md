# Nigh (2015) — Years-to-breast-height model — model spec

## Source

Nigh, Gordon D. (2015). Years-to-breast-height model for Engelmann spruce in the
Engelmann Spruce -- Subalpine Fir biogeoclimatic zone. Province of British
Columbia, Victoria, B.C. Extension Note 115. (`sources/site_index/Nigh2015_EN115.pdf`)

Note: the citation block on p.4 of the PDF self-references as "Exten. Note 114"
and URL En114.htm, but the running header and content title are "115 Extension
Note"; filename is EN115. Treated as Extension Note 115.

## Family / function

- Family: `ytbh_` (years-to-breast-height)
- Function: `ytbh_nigh2015`
- Species: Engelmann spruce (*Picea engelmannii*), NFI code `PICE.ENG`
- Region: Engelmann Spruce -- Subalpine Fir (ESSF) biogeoclimatic zone, British
  Columbia. May be extrapolated to neighbouring zones for true Engelmann spruce;
  use white spruce models for the white x Engelmann cross.

## Model form

Two candidate forms were fit (p.2, Methods):

- Model 1 (linear):  ytbh_i = a0 + a1 * si_i + e_i
- Model 2 (inverse): ytbh_i = a0 + a1 / si_i + e_i

Model 2 was selected (lower MSE, 25.14 vs 31.10). Final model (p.2 Results, p.3
Discussion):

    ytbh = 4.465 + 154.6 / si

where `ytbh` = years to breast height (yr) and `si` = site index (m).

## Variables / units

- `ytbh`: years to breast height (years). Breast height = 1.3 m above the high
  side; breast height assumed reached midway through a growing season.
- `si`: site index in metres, defined as the height of the 50th pith node above
  breast height (base age 50, breast-height age basis).

## Parameters (verified from rendered page images at 300 dpi)

| Param | Value | SE    | Source locator |
|-------|-------|-------|----------------|
| a0    | 4.465 | 1.694 | p.2 Results; p.3 Discussion equation |
| a1    | 154.6 | 18.78 | p.2 Results; p.3 Discussion equation |

IMPORTANT: the raw PDF text layer dropped digits (rendered a1 as "54.6" and the
equation as "4.465 + 54.6/si"). The rendered page images confirm the true
coefficient is 154.6. Value 154.6 is used.

Fit statistics: MSE = 25.14 (model 2); Shapiro-Wilk W = 0.958 (p = 0.010),
slight departure from normality per Q-Q plot; residuals judged homoscedastic.
n = 79 trees (92 plots, ESSF zone).

## Validation tier

Plausibility only — no worked reference table or benchmark output exists in the
source (no source benchmark). Figure 1 provides visual anchors: si ~ 5 -> ytbh
~ 35, si ~ 24 -> ytbh ~ 11, matching the model. Cross-checked against
`ytbh_thrower1994` white spruce (`PICE.GLA`), the model this one replaces; the
paper states differences are small except at the extremes of the site-index
range.

## Implementation notes

- Single species, two hardcoded coefficients — no CSV / `sysdata.rda` change
  needed (mirrors `ytbh_nigh2000`).
- ytbh models are not registered in any registry (only SI models are, in
  `si_model_registry`), so no registry wiring.
