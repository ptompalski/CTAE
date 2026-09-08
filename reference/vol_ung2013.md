# Canadian national taper volume model (Ung et al. 2013)

Uses the national Ung et al. (2013) taper model to estimate total and
merchantable volume. If `height` is not provided (or is all `NA`), the
DBH-only variant is used. If `height` is provided, the DBH+height
variant is used.

## Usage

``` r
vol_ung2013(DBH, height = NULL, species, jurisdiction)
```

## Arguments

- DBH:

  Numeric vector of diameter at breast height (cm).

- height:

  Optional numeric vector of total height (m). If `NULL` (default) or
  all `NA`, the DBH-only model is used.

- species:

  Character vector of species codes (e.g., "PICE.GLA").

- jurisdiction:

  Character vector of jurisdiction codes (e.g., "BC", "ON"). Required to
  assign jurisdiction-specific merchantability criteria

## Value

A tibble with columns `vol_merchantable` and `vol_total` (m^3).

## References

Ung, C.H., Guo, X.J., Fortin, M. (2013). Canadian national taper models.
Forestry Chronicle 89, 211–224. https://doi.org/10.5558/tfc2013-040

## Examples

``` r
# --- DBH-only model (height not provided) ---
vol_ung2013(
  DBH = c(20, 30, 40),
  species = c("PICE.GLA", "ABIE.BAL", "PICE.GLA"),
  jurisdiction = "ON"
)
#> # A tibble: 3 × 2
#>   vol_merchantable vol_total
#>              <dbl>     <dbl>
#> 1            0.242     0.258
#> 2            0.618     0.628
#> 3            1.29      1.29 

# --- DBH + height model ---
vol_ung2013(
  DBH = c(20, 30, 40),
  height = c(18, 24, 30),
  species = c("PICE.GLA", "ABIE.BAL", "PICE.GLA"),
  jurisdiction = "ON"
)
#> # A tibble: 3 × 2
#>   vol_merchantable vol_total
#>              <dbl>     <dbl>
#> 1            0.235     0.249
#> 2            0.730     0.739
#> 3            1.53      1.53 

# --- Vector recycling (common in dplyr pipelines) ---
vol_ung2013(
  DBH = c(25, 35),
  height = 22,
  species = "PICE.GLA",
  jurisdiction = "QC"
)
#> # A tibble: 2 × 2
#>   vol_merchantable vol_total
#>              <dbl>     <dbl>
#> 1            0.459     0.471
#> 2            0.917     0.923

# --- Using inside mutate(): DBH-only fallback ---
trees <- tibble::tibble(
  DBH = c(22, 30, 18),
  species = c("PICE.GLA", "PICE.GLA", "ABIE.BAL")
)

trees |>
  dplyr::mutate(
    vol = vol_ung2013(DBH, species = species, jurisdiction = "NB")
  ) |>
tidyr::unnest(vol)
#> # A tibble: 3 × 4
#>     DBH species  vol_merchantable vol_total
#>   <dbl> <chr>               <dbl>     <dbl>
#> 1    22 PICE.GLA            0.321     0.330
#> 2    30 PICE.GLA            0.672     0.677
#> 3    18 ABIE.BAL            0.199     0.213
```
