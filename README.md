
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CTAE: Canadian tree allometric equations

CTAE is an R package that brings together a collection of published
allometric models developed for Canadian forests. The package provides a
unified interface to estimate a range of tree- and stand-level
attributes, based on models published in the Canadian forestry
literature. The focus of CTAE is on standardization: models are
implemented as faithfully as possible to their original formulations,
with transparent parameter tables, consistent inputs/outputs, and
jurisdiction-appropriate assumptions (e.g. merchantability rules).

CTAE currently includes allometric models to:

- Estimate tree-level volume (total and merchantable)
- Estimate aboveground biomass (AGB)
- Convert volume to biomass
- Convert total volume to merchantable volume
- Apply simple growth models to estimate changes in attributes over time

## Included models

### Aboveground biomass

- Canadian national tree aboveground biomass equations (Lambert et
  al. 2005, Ung et al. 2008).

### Volume (total and merchantable)

- `vol_fortin2007`. Provincial merchantable volume model for Quebec..
  Coverage: QC. 26 species. Fortin et al. 2007
- `vol_galbella94`. Provincial taper model for Saskatchewan based on the
  Kozak variable-exponent form.. Coverage: SK. 12 species. Gal & Bella
  1994
- `vol_honer83`. Regional volume models for central and eastern Canada,
  applicable across multiple provinces.. Coverage: NB, NL, NS, ON, PE,
  QC. 21 species. Honer et al. 1983
- `vol_huang94`. Provincial taper model for Alberta based on the Kozak
  variable-exponent form; applicable at the province level or by Alberta
  subregions.. Coverage: AB. 13 species. Huang 1994
- `vol_klos2007`. Provincial taper model for Manitoba based on the Kozak
  variable-exponent form; applicable at the province level or by
  ecozone.. Coverage: MB. 5 species. Klos et al. 2007
- `vol_kozak94`. Provincial taper model for British Columbia; requires
  BEC zone as a subregion input.. Coverage: BC. 16 species. Kozak 1994
- `vol_sharma2021`. Regional volume models for central and eastern
  Canada, applicable across multiple provinces.. Coverage: NB, NL, NS,
  ON, PE, QC. 25 species. Sharma 2021
- `vol_ung2013`. National taper model for Canada, available in two
  variants: DBH-only and DBH with total height.. Coverage: Canada
  (national). 34 species. Ung et al. 2013
- `vol_zakrzewski2013`. Provincial taper model for Ontario.. Coverage:
  ON. 24 species. Zakrzewski & Penner 2013

#### `vol()`: automatic tree volume estimation across multiple models

`vol()` is a convenience wrapper for estimating total and merchantable
tree volume using the volume models implemented in `CTAE`. Rather than
requiring users to select a specific model, `vol()` consults an internal
model registry and automatically determines which models are applicable
for each tree based on:

- species availability in model parameter tables
- geographic scope (province / jurisdiction)
- required inputs (e.g. total height, subregion such as BEC zone)
- model ranking (regional models preferred over national where
  available)

Example:

    #> # A tibble: 5 × 8
    #>     DBH height species  jurisdiction subregion     vol_total vol_merchantable vol_model         
    #>   <dbl>  <dbl> <chr>    <chr>        <chr>             <dbl>            <dbl> <chr>             
    #> 1    18     15 PICE.MAR AB           <NA>             0.1581           0.1448 vol_huang94       
    #> 2    22     18 BETU.PAP ON           <NA>             0.2910           0.2326 vol_zakrzewski2013
    #> 3    30     22 POPU.TRE QC           <NA>            NA                0.6735 vol_fortin2007    
    #> 4    26     20 PSEU.MEN BC           CWH              0.3871           0.3048 vol_kozak94       
    #> 5    20     20 PINU.BAN MB           Boreal Plains    0.2905           0.2744 vol_klos2007

#### Merchantability criteria

Merchantable volume in CTAE is defined using jurisdiction-specific
merchantability rules, following the officially adopted criteria in each
province or territory (e.g., minimum top diameter, minimum DBH, and
stump height). For most models, these rules are applied dynamically
based on the provided jurisdiction.

Some volume models, however, have merchantability criteria fixed within
the model formulation itself, reflecting how the original equations were
developed and calibrated. In these cases (e.g., `vol_honer83`,
`vol_sharma2021`), merchantable volume is computed using the
model-specific, hard-coded criteria and does not vary by jurisdiction or
species.

The jurisdiction-specific merchantability criteria used by CTAE can be
inspected in the internal dataset `merchcrit`, which documents the
values applied for each jurisdiction (and, where applicable, by species
or subregion).

### Other

- National volume-to-biomass conversions models (Boudewyn et al. 2007)
- National total volume to merchantable volume conversions models
  (Boudewyn et al. 2007)
- National growth and yield model (H, BA, V) (Ung et al. 2009)

## Installation

You can install the most recent version of the package by executing the
code below:

``` r
devtools::install_github("ptompalski/CTAE")
library(CTAE)
```

## References

Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007).
Model-based, volume-to-biomass conversion for forested and vegetated
land in Canada. Natural Resources Canada, Canadian Forest Service,
Pacific Forestry Centre, Victoria, BC. Information Report BC-X-411. 112
p.

Fortin, M., DeBlois, J., Bernier, S., Blais, G., 2007. Mise au point
d’un tarif de cubage général pour les forêts québécoises : une approche
pour mieux évaluer l’incertitude associée aux prévisions. The Forestry
Chronicle 83, 754–765. <https://doi.org/10.5558/tfc83754-5>

Gal, J., & Bella, I.E. (1994). New stem taper functions for 12
Saskatchewan timber species. Natural Resources Canada, Canadian Forest
Service, Northwest Region, Information Report NOR-X-338. Table 5.

Honer, T.G.; Ker, M.F.; Alemdag, I.S. 1983. Metric timber tables for the
commercial tree species of central and eastern Canada. Environ. Can.,
Can. For. Serv., Maritimes For. Res. Cent., Fredericton, NB. Inf.
Rep. M-X-140. <https://ostrnrcan-dostrncan.canada.ca/handle/1845/239814>

Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation
for Major Alberta Tree Species. Report 1 - Individual tree volume
estimation procedures for Alberta: Methods of Formulation and
Statistical Foundations. Alberta Environmental Protection, Land and
Forest Service, Forest Management Division, Edmonton, AB.

Kozak, A. (1988). A variable-exponent taper equation. Canadian Journal
of Forest Research, 18, 1363–1368

Kozak, A. (1994). Development of Taper Equations by BEC Zones and
Species. Province of British Columbia, Ministry of Forests (report).

Klos, R. J., Wang, G. G., Dang, Q.-L., & East, E. W. (2007). Taper
equations for five major commercial tree species in Manitoba, Canada.
Western Journal of Applied Forestry, 22(3), 163–170.

Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree
aboveground biomass equations. Canadian Journal of Forest Research,
35(8), 1996–2018. <https://doi.org/10.1139/x05-112>

Sharma, M. (2021). Total and Merchantable Volume Equations for 25
Commercial Tree Species Grown in Canada and the Northeastern United
States. Forests, 12, 1270.

Ung, C.H., Bernier, P., & Guo, X.-J. (2008). Canadian national biomass
equations: new parameter estimates that include British Columbia data.
Canadian Journal of Forest Research, 38(5), 1123–1132.
<https://doi.org/10.1139/X07-224>

Ung, C.H., Bernier, P.Y., Guo, X.J., Lambert, M.-C., 2009. A simple
growth and yield model for assessing changes in standing volume across
Canada’s forests. The Forestry Chronicle 85, 57–64.
<https://doi.org/10.5558/tfc85057-1>

Ung, C.H., Guo, X.J., Fortin, M., 2013. Canadian national taper models.
Forestry Chronicle 89, 211–224. <https://doi.org/10.5558/tfc2013-040>

Zakrzewski, W.T., Penner, M., 2013. A comparison of tree stem taper
models for use in Ontario. Ontario Forest Research Institute, Report
176.
