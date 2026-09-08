# Lafleche et al. (2013) ecological-site IQS curves for southern Quebec

Implementation of the fixed ecological-site height curves published by
Lafleche et al. (2013) for major commercial tree species in southern
Quebec. Unlike the polymorphic `si_*` models in this package, these
curves are selected by ecological key and use fixed coefficients for
each species-region-(subregion)-type combination.

## Usage

``` r
si_lafleche2013(
  age,
  species,
  ecological_region,
  ecological_type,
  ecological_subregion = NULL,
  curve_set = "potential",
  base_age = 50,
  include_metadata = FALSE
)
```

## Arguments

- age:

  Numeric vector. Corrected age above 1 m (years), with `age > 0`.

- species:

  Character vector of NFI species codes (for example `"PICE.GLA"`).

- ecological_region:

  Character vector of Quebec ecological-region codes (French source
  term: `region_ecologique`; for example `"4f"`).

- ecological_type:

  Character vector of ecological-type codes (French source term:
  `type_ecologique`; for example `"MS22"`).

- ecological_subregion:

  Optional character vector of ecological-subregion codes (French source
  term: `subregion_ecologique`) when needed, for example `"5eS"`.
  Defaults to `NULL`, which is treated as missing.

- curve_set:

  Character scalar. One of `"potential"` or `"observed"`. Defaults to
  `"potential"`. These map internally to the source-table curve sets
  `IQSstation` and `IQSobserved`, respectively.

- base_age:

  Positive numeric scalar. IQS reference age (years above 1 m). Defaults
  to `50`.

- include_metadata:

  Logical scalar. If `TRUE`, append lookup metadata and fitted-curve
  metadata to the returned tibble.

## Value

A tibble with columns:

- height:

  Predicted height (m) at `age`.

- si:

  Fixed-curve IQS value (m) at `base_age`.

When `include_metadata = TRUE`, additional columns describing the
matched ecological key and parameter row are included.

## Details

**Species coverage:** `ABIE.BAL`, `BETU.PAP`, `PICE.GLA`, `PICE.MAR`,
`PICE.RUB`, `PINU.BAN`, `PINU.STR`, `POPU.GRA`, `POPU.TRE`, `THUJ.OCC`.

**Geographic use:** ecological types in southern Quebec.

**Age definition note:** `age` is corrected age above 1 m height
(years).

**Height definition note:** the source curves predict stand height in
metres, constrained to pass through `(age = 0, height = 1)`.

**Base-age note:** IQS is defined as predicted height at 50 years above
1 m. `base_age` defaults to `50`, but any positive scalar can be
supplied.

This function always predicts height from `age`; it also returns the
corresponding fixed-curve IQS value evaluated at `base_age`.

`curve_set` chooses between the two curve families reported in the
source: `"potential"` corresponds to the fitted potential-height curves
used to derive `IQSstation` in the report, whereas `"observed"`
corresponds to the separate curves fitted to observed height growth
(`IQSobserved`). In the source tables these appear as `IQSstation` and
`IQSobserved`, respectively.

At present, `CR`, `WE`, and `LIN` equation forms are implemented
directly from the publication and validated against the published
`IQSstation` summaries. The single `LOGIST3` row is kept in the
parameter data but raises an error here because its printed
equation/coefficients have not yet been reconciled with the published
table values.

## References

Lafleche, V., S. Bernier, J.-P. Saucier, and C. Gagne. 2013. *Indices de
qualite de station des principales essences commerciales en fonction des
types ecologiques du Quebec meridional*. Quebec, ministere des
Ressources naturelles, Direction des inventaires forestiers, 115 p.

## Examples

``` r
si_lafleche2013(
  age = 50,
  species = "PICE.GLA",
  ecological_region = "2a",
  ecological_type = "MJ11"
)
#> # A tibble: 1 × 2
#>   height    si
#>    <dbl> <dbl>
#> 1   19.6  19.6

si_lafleche2013(
  age = c(25, 50),
  species = "ABIE.BAL",
  ecological_region = "3c",
  ecological_type = "MJ12",
  curve_set = "potential",
  include_metadata = TRUE
)
#> # A tibble: 2 × 15
#>   height    si curve_set  species_qc Species  ecological_region
#>    <dbl> <dbl> <chr>      <chr>      <chr>    <chr>            
#> 1   9.32  17.5 IQSstation SAB        ABIE.BAL 3c               
#> 2  17.5   17.5 IQSstation SAB        ABIE.BAL 3c               
#> # ℹ 9 more variables: ecological_region_description <chr>,
#> #   ecological_subregion <chr>, ecological_type <chr>,
#> #   ecological_type_description_fr <chr>, ecological_type_description_en <chr>,
#> #   equation_used <chr>, n_trees <int>, n_observations <int>, pseudo_r2 <dbl>

# Vectorized use with the pipe operator
trees_h <- tibble::tibble(
  species = c("ABIE.BAL", "PICE.GLA"),
  age = c(25, 50),
  ecological_region = c("3c", "2a"),
  ecological_type = c("MJ12", "MJ11")
)

trees_h |>
  dplyr::mutate(
    iqs_pred = si_lafleche2013(
      age = age,
      species = species,
      ecological_region = ecological_region,
      ecological_type = ecological_type
    )
  ) |>
  tidyr::unnest(iqs_pred)
#> # A tibble: 2 × 6
#>   species    age ecological_region ecological_type height    si
#>   <chr>    <dbl> <chr>             <chr>            <dbl> <dbl>
#> 1 ABIE.BAL    25 3c                MJ12              9.32  17.5
#> 2 PICE.GLA    50 2a                MJ11             19.6   19.6
```
