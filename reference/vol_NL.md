# Estimate tree volume for Newfoundland

Implements the Newfoundland individual-tree volume equations used in the
Open Stand Model (OSM) API, based on Honer’s refitted formulations.

## Usage

``` r
vol_nl(DBH, height, species, subregion = "Province", keep_net = FALSE)
```

## Arguments

- DBH:

  Numeric vector. Diameter at breast height (cm).

- height:

  Numeric vector. Total tree height (m).

- species:

  Character vector of NFI species codes (e.g., `"PICE.MAR"`).

- subregion:

  Character or numeric vector. Either:

  - `"Province"` (default; uses province-wide equations), or

  - District IDs `1-24`. Only District `2` and `4-18` use district-level
    equations; all others fall back to province-wide equations.

- keep_net:

  Logical. If `TRUE`, additional columns `vol_merchantable_gross` and
  `vol_merchantable_net` are returned.

## Value

A tibble with:

- `vol_total`: Total stem volume inside bark (m³).

- `vol_merchantable`: Gross merchantable volume inside bark (m³).

If `keep_net = TRUE`, also returns:

- `vol_merchantable_gross`

- `vol_merchantable_net`

## Details

Model structure:

- District-level equations (NX-242) are used only for District 2 and
  4–18 and only for balsam fir (`ABIE.BAL`) and black spruce
  (`PICE.MAR`).

- All other districts (including District 19 and 1–24 outside 2, 4–18)
  fall back to province-wide species equations (NX-122 total + NX-67
  merchantable).

The original OSM implementation excluded District 19 because it produced
unstable or unrealistic results. This function reproduces that behavior
for consistency.

Net merchantable volume:

- Net merchantable volume equations are available only for NX-242
  district models.

- When available, net merchantable volume volume is constrained to be
  between 95% and 100% of gross merchantable volume, matching the
  original OSM implementation

- By default, the function returns gross merchantable volume to remain
  consistent with other volume models in the package.

Merchantable volume is computed between jurisdictional stump height and
top diameter limits obtained from `get_merch_criteria("NL")`. Trees
below the minimum DBH threshold return zero merchantable volume, but
total volume is still computed.

## References

Honer, T.G. (1967). Standard volume tables and merchantable conversion
factors. Canadian Forest Service Information Report N-X-67.

Ker, M.F. 1974. Metric Tree Volume Tables for Newfoundland. Newfoundland
Forest Research Centre, St. Johns, NF. Information Report N-X-122.
https://ostrnrcan-dostrncan.canada.ca/handle/1845/238893

Warren, G.R. & Meades, J.P. (1986). Wood defect and density studies II:
Total and net volume equations for Newfoundland’s forest management
units. Canadian Forest Service Information Report N-X-242.

## Examples

``` r
# Province-wide model (default)
vol_nl(20, 20, "PICE.MAR")
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.249            0.229

# District-level model (NX-242 used for District 2)
vol_nl(20, 20, "PICE.MAR", subregion = 2)
#> # A tibble: 1 × 2
#>   vol_total vol_merchantable
#>       <dbl>            <dbl>
#> 1     0.306            0.288

# Return net merchantable volume (if available)
vol_nl(20, 20, "PICE.MAR", subregion = 2, keep_net = TRUE)
#> # A tibble: 1 × 4
#>   vol_total vol_merchantable vol_merchantable_gross vol_merchantable_net
#>       <dbl>            <dbl>                  <dbl>                <dbl>
#> 1     0.306            0.288                  0.288                0.281

# Tidyverse workflow example
trees <- tibble::tibble(
  DBH = c(15, 20, 25, 30),
  height = c(15, 20, 21, 25),
  species = c("PICE.MAR", "ABIE.BAL","PICE.MAR", "BETU.PAP"),
  subregion = c("Province", 2, 17, "Province")
)

trees |>
  dplyr::mutate(vol = vol_nl(DBH, height, species, subregion)) |>
  tidyr::unnest(vol)
#> # A tibble: 4 × 6
#>     DBH height species  subregion vol_total vol_merchantable
#>   <dbl>  <dbl> <chr>    <chr>         <dbl>            <dbl>
#> 1    15     15 PICE.MAR Province      0.118            0.102
#> 2    20     20 ABIE.BAL 2             0.284            0.270
#> 3    25     21 PICE.MAR 17            0.353            0.337
#> 4    30     25 BETU.PAP Province      0.661            0.626
```
