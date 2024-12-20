
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CTAE - Canadian tree allometric equations

A collection of tools to calculate tree- or stand-level attributes
developed for Canadian forests

## Models included so far

- Canadian national tree aboveground biomass equations (Lambert et
  al. 2005, Ung et al. 2008).

- Individual tree volume equations for major Alberta tree species (Huang
  1994)

- Volume-to-biomass conversions models (Boudewyn et al. 2007)

- Total volume to merchantable volume conversions models (Boudewyn et
  al. 2007)

- Simple growth and yield model (H, BA, V) (Ung et al. 2009)

Updated model parameters for models developed by Boudewyn et al (2007)
downloaded from <https://nfi.nfis.org/en/biomass_models>.

Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007).
Model-based, volume-to-biomass conversion for forested and vegetated
land in Canada. Natural Resources Canada, Canadian Forest Service,
Pacific Forestry Centre, Victoria, BC. Information Report BC-X-411. 112
p.

Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation
for Major Alberta Tree Species. Report 1 - Individual tree volume
estimation procedures for Alberta: Methods of Formulation and
Statistical Foundations. Alberta Environmental Protection, Land and
Forest Service, Forest Management Division, Edmonton, AB.

Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree
aboveground biomass equations. Canadian Journal of Forest Research,
35(8), 1996–2018. <https://doi.org/10.1139/x05-112>

Ung, C.-H., Bernier, P., & Guo, X.-J. (2008). Canadian national biomass
equations: new parameter estimates that include British Columbia data.
Canadian Journal of Forest Research, 38(5), 1123–1132.
<https://doi.org/10.1139/X07-224>

Ung, C.-H., Bernier, P.Y., Guo, X.J., Lambert, M.-C., 2009. A simple
growth and yield model for assessing changes in standing volume across
Canada’s forests. The Forestry Chronicle 85, 57–64.
<https://doi.org/10.5558/tfc85057-1>

## Installation

You can install the most recent version of the package by executing the
code below:

``` r
devtools::install_github("ptompalski/CTAE")
library(CTAE)
```

# Additional details

## Alberta Natural Subregion Codes

<table>
<thead>
<tr>
<th style="text-align:left;">
Code
</th>
<th style="text-align:left;">
Subregion name
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CM
</td>
<td style="text-align:left;">
Central Mixedwood
</td>
</tr>
<tr>
<td style="text-align:left;">
DMW
</td>
<td style="text-align:left;">
Dry Mixedwood
</td>
</tr>
<tr>
<td style="text-align:left;">
NM
</td>
<td style="text-align:left;">
Northern Mixedwood (Wetland Mixedwood)
</td>
</tr>
<tr>
<td style="text-align:left;">
BSA
</td>
<td style="text-align:left;">
Boreal Subarctic
</td>
</tr>
<tr>
<td style="text-align:left;">
PAD
</td>
<td style="text-align:left;">
Peace-Athabasca Delta (Peace River Lowlands)
</td>
</tr>
<tr>
<td style="text-align:left;">
LBH
</td>
<td style="text-align:left;">
Lower Boreal Highlands (Boreal Highlands)
</td>
</tr>
<tr>
<td style="text-align:left;">
UB
</td>
<td style="text-align:left;">
Upper Boreal Highlands (Boreal Highlands)
</td>
</tr>
<tr>
<td style="text-align:left;">
AP
</td>
<td style="text-align:left;">
Athabasca Plain
</td>
</tr>
<tr>
<td style="text-align:left;">
ALP
</td>
<td style="text-align:left;">
Alpine
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:left;">
Subalpine
</td>
</tr>
<tr>
<td style="text-align:left;">
M
</td>
<td style="text-align:left;">
Montane
</td>
</tr>
<tr>
<td style="text-align:left;">
UF
</td>
<td style="text-align:left;">
Upper Foothills
</td>
</tr>
<tr>
<td style="text-align:left;">
LF
</td>
<td style="text-align:left;">
Lower Foothills
</td>
</tr>
<tr>
<td style="text-align:left;">
KU
</td>
<td style="text-align:left;">
Kazan Upland
</td>
</tr>
<tr>
<td style="text-align:left;">
FP
</td>
<td style="text-align:left;">
Foothills Parkland
</td>
</tr>
<tr>
<td style="text-align:left;">
PRP
</td>
<td style="text-align:left;">
Peace River Parkland
</td>
</tr>
<tr>
<td style="text-align:left;">
CP
</td>
<td style="text-align:left;">
Central Parkland
</td>
</tr>
<tr>
<td style="text-align:left;">
DMG
</td>
<td style="text-align:left;">
Dry Mixedgrass
</td>
</tr>
<tr>
<td style="text-align:left;">
FF
</td>
<td style="text-align:left;">
Foothills Fescue
</td>
</tr>
<tr>
<td style="text-align:left;">
NF
</td>
<td style="text-align:left;">
Northern Fescue
</td>
</tr>
<tr>
<td style="text-align:left;">
MG
</td>
<td style="text-align:left;">
Mixedgrass
</td>
</tr>
</tbody>
</table>

## Huang et al. model availability (`V_Huang()`)

<table>
<thead>
<tr>
<th style="text-align:left;">
species
</th>
<th style="text-align:left;">
Province
</th>
<th style="text-align:left;">
ALP
</th>
<th style="text-align:left;">
AP
</th>
<th style="text-align:left;">
BSA
</th>
<th style="text-align:left;">
CM
</th>
<th style="text-align:left;">
CP
</th>
<th style="text-align:left;">
DMW
</th>
<th style="text-align:left;">
FP
</th>
<th style="text-align:left;">
KU
</th>
<th style="text-align:left;">
LBH
</th>
<th style="text-align:left;">
LF
</th>
<th style="text-align:left;">
M
</th>
<th style="text-align:left;">
NM
</th>
<th style="text-align:left;">
PAD
</th>
<th style="text-align:left;">
PRP
</th>
<th style="text-align:left;">
SA
</th>
<th style="text-align:left;">
UF
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ABIE.BAL
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
<tr>
<td style="text-align:left;">
BETU.PAP
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
LARI.LAR
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
PICE.ENG
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
PICE.GLA
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
<tr>
<td style="text-align:left;">
PICE.MAR
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
<tr>
<td style="text-align:left;">
PINU.BAN
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
PINU.CON
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
<tr>
<td style="text-align:left;">
POPU.BAL
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
<tr>
<td style="text-align:left;">
POPU.TRE
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
<tr>
<td style="text-align:left;">
PSEU.MEN
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
UNKN.HWD
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
<tr>
<td style="text-align:left;">
UNKN.SWD
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
<td style="text-align:left;">
✅
</td>
</tr>
</tbody>
</table>
<!-- ## Ecozone Codes -->
<!-- ```{r, echo=T} -->
<!-- CTAE::CodesEcozones -->
<!-- # CodesEcozones %>% -->
<!--   # select(`Ecozone code`=EcozoneCode, `Ecozone name` = EcozoneName) %>% -->
<!--   # kable(format = "html") -->
<!-- ``` -->
