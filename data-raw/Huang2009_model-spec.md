# Model spec: si_huang2009

> Human-reviewed extraction artifact for the Huang, Meng & Yang (2009) GYPSY
> top-height / site-index models for four Alberta tree species.

## Source

- **Citation:** Huang, S., Meng, S.X., and Yang, Y. 2009. A Growth and Yield
  Projection System (GYPSY) for Natural and Post-harvest Stands in Alberta.
  Technical Report Pub. No. T/216. Forest Management Branch, Alberta Sustainable
  Resource Development, Edmonton, Alberta. ISBN 978-0-7785-8486-5 (on-line).
  (Confirmed from the rendered title page.)
- **BibTeX key:** @Huang2009gypsy
- **Document in `sources/`:** `sources/site_index/GYPSY-Natural-PostHarvestStands-Alberta-May21-2009.pdf`
- **Model family:** si
- **Target function:** `si_huang2009`

## Scope / domain of applicability

- **Jurisdiction / region:** Alberta.
- **Species covered (source label -> NFI code):**
  - Aspen (AW) -> `POPU.TRE` (trembling aspen)
  - Black spruce (SB) -> `PICE.MAR`
  - Lodgepole pine (PL) -> `PINU.CON`
  - White spruce (SW) -> `PICE.GLA`
- **Age basis:** **total age** (from germination), years. `SIt` is total-age
  based site index; the model also produces `SIbh` (breast-height-age based site
  index, currently used in Alberta) via an internal years-to-breast-height (Y2BH)
  conversion.
- **Base age:** 50 years. `SIt` = top height at 50 years **total** age;
  `SIbh` = top height at 50 years **breast-height** age.
- **Stands:** natural and post-harvest stands in Alberta; developed for pure and
  mixed stands of the four main Alberta species.

## Variables and units

| Symbol | Meaning | Units |
|--------|---------|-------|
| Htop | top height (avg. height of 100 largest-DBH trees/ha) | m |
| SIt | total-age site index (top height at 50 yr total age) | m |
| SIbh | breast-height-age site index (top height at 50 yr bhage) | m |
| totage | total age from point of germination | yr |
| bhage | breast-height age | yr |
| Y2BH | years to reach breast height (1.3 m) from germination | yr |
| b1..b4 | model coefficients | - |

## Model form(s) (Section 4, p. 5; eqs. [1]-[3])

All forms are `Htop = SIt * (numerator / denominator)`, differing in how age and
`ln(SIt)` enter. Let `S = SIt`, `A = totage`.

**Aspen (AW), model form [1]:**
```
Htop = S * ( 1 + exp(b1 + b2*ln(1 + 50)  + b3*[ln(S)]^2 + b4*sqrt(50)) )
          / ( 1 + exp(b1 + b2*ln(1 + A)   + b3*[ln(S)]^2 + b4*sqrt(50)) )
```

**Black spruce (SB) and lodgepole pine (PL), model form [2]:**
```
Htop = S * ( 1 + exp(b1 + b2*ln(1 + 50)  + b3*ln(S) + b4*sqrt(50)) )
          / ( 1 + exp(b1 + b2*ln(1 + A)   + b3*ln(S) + b4*sqrt(50)) )
```

**White spruce (SW), model form [3]:**
```
Htop = S * ( 1 + exp(b1 + b2*ln(1 + 50^2) + b3*[ln(S)]^2 + b4*sqrt(50)) )
          / ( 1 + exp(b1 + b2*ln(1 + A^2)  + b3*[ln(S)]^2 + b4*sqrt(50)) )
```

Note SW squares the age inside the log (`ln(1 + A^2)`) in both numerator and
denominator; AW/SB/PL use `ln(1 + A)`.

### Directions

- **Predict Htop** from (totage, SIt): direct evaluation of eq. [1]/[2]/[3].
- **Predict SIt** from (totage, Htop): not closed-form invertible; the source SAS
  (Appendix 1) uses a damped fixed-point iteration:
  ```
  si0 = 10
  repeat until |si0 - si1| < 1e-8:
    x10 = 1 + exp(b1 + b2*sqrt(log(A_term + 1)) + b3*log(si0)^p + b4*sqrt(50))
    x20 = 1 + exp(b1 + b2*sqrt(log(50_term + 1)) + b3*log(si0)^p + b4*sqrt(50))
    si1 = topht * x10 / x20
    si0 = (si0 + si1) / 2
  SIt = si1
  ```
  where `p = 1` for SB/PL (form [2]) and `p = 2` for AW/SW (forms [1]/[3]), and
  the age terms are `A` / `50` for AW/SB/PL and `A^2` / `50^2` for SW.

### Years-to-breast-height (Y2BH) and SIbh

From the SAS (Appendix 1), with `k1`, `k2`, `k3` computed at the solved `SIt`:
```
k1 = exp(b1 + b2*sqrt(log(50_term + 1)) + b3*log(SIt)^p + b4*sqrt(50))
k2 = SIt^(b3)              [SB, PL]   or  SIt^(b3*log(SIt))   [AW, SW]
k3 = (SIt*(1 + k1)/1.3 - 1) / (exp(b1)*exp(b4*sqrt(50))*k2)
Y2BH = exp((log(k3)/b2)^2) - 1                 [AW, SB, PL]
Y2BH = sqrt(exp((log(k3)/b2)^2) - 1)           [SW]
```
Then
```
SIbh = SIt * ( 1 + exp(b1 + b2*sqrt(log(50_term        + 1)) + b3*log(SIt)^p + b4*sqrt(50)) )
            / ( 1 + exp(b1 + b2*sqrt(log((50+Y2BH)_term + 1)) + b3*log(SIt)^p + b4*sqrt(50)) )
```
where `50_term` and `(50+Y2BH)_term` are squared for SW.

Note: the SAS writes the numerator/denominator with `b2*sqrt(log(...))`, whereas
Section 4 writes `b2*ln(...)`. These are consistent: the fitted `b2` differs by
the reparameterization, and the coefficients in Table 1 are those used by the SAS
`sqrt(log(...))` form. **Implement the SAS form**, since that is the executable
authority and it reproduces the printed worked-example output.

## Parameters (Table 1, p. 5; confirmed against rendered image and SAS program)

| species | nfi | b1 | b2 | b3 | b4 | form | p | age_sq |
|---------|-----|-----|-----|-----|-----|------|---|--------|
| Aspen (AW)         | POPU.TRE | 9.908888 | -3.92451 | -0.32778 | 0.134376 | [1] | 2 | no  |
| Black spruce (SB)  | PICE.MAR | 14.56236 | -6.04705 | -1.53715 | 0.240174 | [2] | 1 | no  |
| Lodgepole pine (PL)| PINU.CON | 12.84571 | -5.73936 | -0.91312 | 0.150668 | [2] | 1 | no  |
| White spruce (SW)  | PICE.GLA | 12.14943 | -3.77051 | -0.28534 | 0.165483 | [3] | 2 | yes |

Table 1 values verified digit-for-digit against the rendered page-8 image
(`tmp/gypsy_table1_crop.png`) AND the Appendix-1 SAS constants (they agree
exactly). No flagged/uncertain values.

Age-to-total-age conversion factors (Section 4, p. 5), for reference only (not
part of the fitted model):
```
AW: totage = stumpage + 0.5;  totage = bhage + 4
PL: totage = stumpage + 3;    totage = bhage + 8
SW: totage = stumpage + 4;    totage = bhage + 12
SB: totage = stumpage + 5;    totage = bhage + 15
```

## Benchmark plan

- **Fidelity (Tier 1):** the SAS worked-example output printed in Appendix 1
  (p. 22) is the gold standard. For `topht = 20`:

  | species | totage | Y2BH | SIt | SIbh | ht (projected) |
  |---------|--------|------|------|-------|------|
  | AW | 50 | 2.62064 | 20.0000 | 20.5285 | 20.0000 |
  | AW | 60 | 3.02971 | 18.1340 | 18.7356 | 20.0000 |
  | SB | 50 | 5.95482 | 20.0000 | 21.3726 | 20.0000 |
  | SB | 70 | 7.39775 | 16.1033 | 17.7449 | 20.0000 |
  | PL | 50 | 5.59552 | 20.0000 | 21.3314 | 20.0000 |
  | PL | 80 | 7.11354 | 15.0905 | 16.5410 | 20.0000 |
  | SW | 50 | 6.27265 | 20.0000 | 21.7585 | 20.0000 |
  | SW | 90 | 9.54306 | 12.2179 | 14.3948 | 20.0000 |

  Reproduce SIt from (totage, topht) and match to 1e-3. At totage = 50 the model
  returns SIt = topht exactly (all rows show 20.0000), a strong self-consistency
  check. "ht (projected)" = 20.0000 for every row because these are projections
  back to totage = 50 (base age), i.e. Htop(50, SIt) = SIt.

- **Self-consistency:** at totage = 50, Htop(50, SIt) == SIt exactly; round-trip
  predict Htop from (totage, SIt) then recover SIt from (totage, Htop).

- **Cross-check (Tier-1 complement):** compare against existing Alberta / same-
  species SI functions (`si_huang1994` covers PL/SW/SB/AW in Alberta) over an
  overlapping (age, SI) grid, and report magnitude/direction of any divergence.
  These are different model fits, so expect similar shape, not identity.
