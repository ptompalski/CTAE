# Calculate tree-level AGB using Lambert & Ung national biomass equations

Unified interface for the Lambert et al. (2005) and Ung et al. (2008)
Canadian national tree aboveground biomass equations. Two equation sets
are available: dbh-only and dbh+height. By default
(`equation_set = "auto"`), the dbh+height set is used when valid
`height` is provided, otherwise the dbh-only set is used.

## Usage

``` r
agb_lambert_ung(
  DBH,
  height = NA_real_,
  species = NA,
  equation_set = c("auto", "dbh", "dbh_height"),
  keep_model_id = FALSE
)
```

## Arguments

- DBH:

  Tree DBH (cm). Numeric vector.

- height:

  Tree height (m). Numeric vector. Optional for the dbh-only equation
  set.

- species:

  Tree species code in the NFI standard (e.g. `"POPU.TRE"`). Can be a
  scalar (recycled) or a vector matching `DBH`.

- equation_set:

  Which Lambert & Ung equation set to use. One of `"auto"`, `"dbh"`,
  `"dbh_height"`. `"auto"` makes a per-tree choice based on whether
  `height` is valid.

- keep_model_id:

  Logical. If `TRUE`, include a `model_id` column indicating which
  equation set was used (`"DBH"` or `"DBHHT"`).

## Value

A tibble with AGB components: `Bwood`, `Bbark`, `Bstem`, `Bfoliage`,
`Bbranches`, `Bcrown`, `Btotal`. If `keep_model_id = TRUE`, includes
`model_id`.

## Examples

``` r

# Auto-selection (equation_set = "auto" by default):
# height missing -> DBH-only equations
agb_lambert_ung(
  DBH = 20,
  species = "PICE.MAR"
)
#> # A tibble: 1 × 7
#>   Bwood Bbark Bstem Bfoliage Bbranches Bcrown Btotal
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>  <dbl>  <dbl>
#> 1  89.0  12.5  102.     11.6      14.6   26.1   128.

# height provided -> DBH + height equations
agb_lambert_ung(
  DBH = 20,
  height = 17,
  species = "PICE.MAR"
)
#> # A tibble: 1 × 7
#>   Bwood Bbark Bstem Bfoliage Bbranches Bcrown Btotal
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>  <dbl>  <dbl>
#> 1  99.4  13.4  113.     9.60      12.0   21.5   134.

# Show which equation set was used (model_id appended as the last column)
agb_lambert_ung(
  DBH = 20,
  species = "PICE.MAR",
  keep_model_id = TRUE
)
#> # A tibble: 1 × 8
#>   Bwood Bbark Bstem Bfoliage Bbranches Bcrown Btotal model_id
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>  <dbl>  <dbl> <chr>   
#> 1  89.0  12.5  102.     11.6      14.6   26.1   128. DBH     

# Force a specific equation set
agb_lambert_ung(
  DBH = 20,
  height = 17,
  species = "PICE.MAR",
  equation_set = "dbh"
)
#> # A tibble: 1 × 7
#>   Bwood Bbark Bstem Bfoliage Bbranches Bcrown Btotal
#>   <dbl> <dbl> <dbl>    <dbl>     <dbl>  <dbl>  <dbl>
#> 1  89.0  12.5  102.     11.6      14.6   26.1   128.

# Multiple rows
trees <- tibble::tibble(
 tree_id = 1:4,
 DBH = c(22, 30, 18, 35),
 height = c(18, 24, NA, 27),
 species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL", "PINU.BAN")
)

trees |>
 dplyr::mutate(
   agb = agb_lambert_ung(
     DBH = DBH,
     height = height,
     species = species,
     keep_model_id = TRUE
   )
 ) |>
 tidyr::unnest(agb)
#> # A tibble: 4 × 12
#>   tree_id   DBH height species  Bwood Bbark Bstem Bfoliage Bbranches Bcrown
#>     <int> <dbl>  <dbl> <chr>    <dbl> <dbl> <dbl>    <dbl>     <dbl>  <dbl>
#> 1       1    22     18 PICE.GLA 113.   15.1 128.      13.5      17.4   30.9
#> 2       2    30     24 PICE.GLA 261.   31.6 293.      20.8      32.8   53.6
#> 3       3    18     NA ABIE.BAL  55.5  10.2  65.7     10.5      10.8   21.3
#> 4       4    35     27 PINU.BAN 488.   29.6 518.      18.7      38.4   57.1
#> # ℹ 2 more variables: Btotal <dbl>, model_id <chr>
```
