
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

## Models included so far

### Aboveground biomass

- Canadian national tree aboveground biomass equations (Lambert et
  al. 2005, Ung et al. 2008).

### Volume (total and merchantable)

#### Regional models

- Alberta - Individual tree volume equations for major Alberta tree
  species (Huang 1994)
- British Columbia - Kozak 1994 Taper Equations
- Ontario - Zakrzewski et al. (2013) taper/volume model for Ontario

#### National models

- Ung et al. 2013 Canadian national taper models

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

Honer, T.G.; Ker, M.F.; Alemdag, I.S. 1983. Metric timber tables for the
commercial tree species of central and eastern Canada. Environ. Can.,
Can. For. Serv., Maritimes For. Res. Cent., Fredericton, NB. Inf.
Rep. M-X-140. <https://ostrnrcan-dostrncan.canada.ca/handle/1845/239814>

Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation
for Major Alberta Tree Species. Report 1 - Individual tree volume
estimation procedures for Alberta: Methods of Formulation and
Statistical Foundations. Alberta Environmental Protection, Land and
Forest Service, Forest Management Division, Edmonton, AB.

Kozak, A. (1994). Development of Taper Equations by BEC Zones and
Species. Province of British Columbia, Ministry of Forests (report).

Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree
aboveground biomass equations. Canadian Journal of Forest Research,
35(8), 1996–2018. <https://doi.org/10.1139/x05-112>

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
