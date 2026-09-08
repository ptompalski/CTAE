# Tree Volume Models

## Tree volume models included in the package

There are several individual tree volume allometric models designed for
Canadian forests. Most were developed at the jurisdiction level and
require DBH, height, and species as inputs.

The package currently includes:

- [`vol_fortin2007()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_fortin2007.md).
  Provincial merchantable volume model for Quebec. Coverage: QC. 26
  species
- [`vol_galbella94()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_galbella94.md).
  Provincial taper model for Saskatchewan based on the Kozak
  variable-exponent form. Coverage: SK. 12 species
- [`vol_honer83()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_honer83.md).
  Regional volume models for central and eastern Canada, applicable
  across multiple provinces. Coverage: NB, NL, NS, ON, PE, QC. 21
  species
- [`vol_huang94()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_huang94.md).
  Provincial taper model for Alberta based on the Kozak
  variable-exponent form; applicable at the province level or by Alberta
  subregions. Coverage: AB. 13 species
- [`vol_klos2007()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_klos2007.md).
  Provincial taper model for Manitoba based on the Kozak
  variable-exponent form; applicable at the province level or by
  ecozone. Coverage: MB. 5 species
- [`vol_kozak94()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_kozak94.md).
  Provincial taper model for British Columbia; requires BEC zone as a
  subregion input. Coverage: BC. 16 species
- [`vol_nigh2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_nigh2016.md).
  Total and merchantable volume equations for BC. Coverage: BC. 18
  species
- [`vol_nl()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_NL.md).
  Total and merchantable volume for Newfoundland and Labrador. Coverage:
  NL. 12 species
- [`vol_sharma2021()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_sharma2021.md).
  Regional volume models for central and eastern Canada, applicable
  across multiple provinces. Coverage: NB, NL, NS, ON, PE, QC. 25
  species
- [`vol_ung2013()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_ung2013.md).
  National taper model for Canada, available in two variants: DBH-only
  and DBH with total height. Coverage: Canada (national). 34 species
- [`vol_zakrzewski2013()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_zakrzewski2013.md).
  Provincial taper model for Ontario. Coverage: ON. 24 species

## `vol()`: automatic tree volume estimation across multiple models

[`vol()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol.md)
is a convenience wrapper for estimating total and merchantable tree
volume using the volume models implemented in `CanadaForestAllometry`.
Rather than requiring users to select a specific model,
[`vol()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol.md)
uses the internal model registry and automatically determines which
models are applicable for each tree based on:

- species availability in model parameter tables
- geographic scope (province/jurisdiction)
- required inputs (e.g. total height, subregion such as BEC zone)
- model ranking (regional models preferred over national where
  available)

Example:

``` r

trees <- tibble::tibble(
  DBH = c(18, 22, 30, 26, 20),
  height = c(15, 18, 22, 20, 20),
  species = c("PICE.MAR", "BETU.PAP", "POPU.TRE", "PSEU.MEN", "PINU.BAN"),
  jurisdiction =  c("AB", "ON", "QC", "BC", "MB"),
  subregion = c(NA, NA, NA, "CWH", "Boreal Plains")
)

trees |>
  dplyr::mutate(
    vol(
      DBH = DBH,
      height = height,
      species = species,
      jurisdiction = jurisdiction,
      subregion = subregion,
      keep_model_id = TRUE
    )
  )
#> # A tibble: 5 × 8
#>     DBH height species  jurisdiction subregion     vol_total vol_merchantable
#>   <dbl>  <dbl> <chr>    <chr>        <chr>             <dbl>            <dbl>
#> 1    18     15 PICE.MAR AB           <NA>              0.158            0.145
#> 2    22     18 BETU.PAP ON           <NA>              0.291            0.233
#> 3    30     22 POPU.TRE QC           <NA>             NA                0.674
#> 4    26     20 PSEU.MEN BC           CWH               0.387            0.305
#> 5    20     20 PINU.BAN MB           Boreal Plains     0.291            0.274
#> # ℹ 1 more variable: vol_model <chr>
```

## Merchantability criteria

Merchantable volume in `CanadaForestAllometry` is defined using
jurisdiction-specific merchantability rules, following the officially
adopted criteria in each province or territory (e.g., minimum top
diameter, minimum DBH, and stump height). For most models, these rules
are applied dynamically based on the provided jurisdiction.

Some volume models, however, have merchantability criteria fixed within
the model formulation itself, reflecting how the original equations were
developed and calibrated. In these cases (e.g.,
[`vol_honer83()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_honer83.md),
[`vol_sharma2021()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_sharma2021.md),
[`vol_fortin2007()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_fortin2007.md),
[`vol_nigh2016()`](https://ptompalski.github.io/CanadaForestAllometry/reference/vol_nigh2016.md)),
merchantable volume is computed using the model-specific, hard-coded
criteria and does not vary by jurisdiction or species.

The jurisdiction-specific merchantability criteria used by
`CanadaForestAllometry` can be inspected in the built-in dataset
`merchcrit`, which documents the values applied for each jurisdiction
(and, where applicable, by species or subregion).

## Comparing Tree Volume Models

Tree stem volume is estimated in Canada using a variety of model
families, ranging from national equations (e.g., Ung et al.
([2013](#ref-Ung2013))) to province-specific taper and volume systems
(e.g., Kozak ([1994](#ref-Kozak1994)), Huang et al.
([1994](#ref-Huang1994)), Zakrzewski and Penner
([2013](#ref-Zakrzewski2013)), Sharma ([2021](#ref-Sharma2021))). These
models differ in their functional form, calibration data, and
jurisdictional scope.

The `CanadaForestAllometry` package implements these models under a
unified interface, allowing consistent input formatting and transparent
model selection. However, different equations may yield systematically
different predictions for the same tree dimensions.

The figure below shows total and merchantable volume calculated using
all implemented models (x-axis) across species. It provides a quick
overview of species coverage (i.e., which species are covered by each
model) and highlights differences among model outputs, with tree DBH and
height held constant for every calculation.

![](Tree-Volume-Models_files/figure-html/volume%20by%20model%20and%20species-1.png)

## References

Huang, S., Titus, S., Lakusta, S., Held, R., 1994. Ecologically Based
Individual Tree Volume Estimation for Major Alberta Tree Species, … to
Alberta Environmental Protection, Land ….

Kozak, A., 1994. Development of Taper Equations by BEC Zones and
Species. Province of British Columbia, Ministry of Forests.

Sharma, M., 2021. Total and Merchantable Volume Equations for 25
Commercial Tree Species Grown in Canada and the Northeastern United
States. Forests 12. <https://doi.org/10.3390/f12091270>

Ung, C.H., Guo, X.J., Fortin, M., 2013. Canadian national taper models.
Forestry Chronicle 89, 211–224. <https://doi.org/10.5558/tfc2013-040>

Zakrzewski, W.T., Penner, M., 2013. A comparison of tree stem taper
models for use in Ontario.
