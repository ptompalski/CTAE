# Get merchantability criteria for a jurisdiction/species (BEC-aware for BC)

Convenience helper to fetch CBM-style merchantability criteria (stump
height, top diameter, and minimum DBH) from the internal `merchcrit`
lookup table.

## Usage

``` r
get_merch_criteria(
  jurisdiction,
  species = NA_character_,
  BEC_zone = NA_character_,
  verbose = TRUE
)
```

## Arguments

- jurisdiction:

  Province/Territory code (e.g., `"BC"`, `"AB"`).

- species:

  Species code. Optional for `"BC"`; if missing, falls back to `"ALL"`
  with a warning.

- BEC_zone:

  Optional BEC zone code (e.g., `"CWH"`, `"ICH"`). If missing for
  `"BC"`, conservative criteria are taken from `BEC_group == "UNKNOWN"`.

- verbose:

  If TRUE, warns when BC falls back to genus-level, UNKNOWN BEC, or ALL.

## Value

A one-row tibble with columns: `jurisdiction`, `species`, `stumpht_m`,
`topdbh_cm`, `mindbh_cm`.

## Details

Before lookup, inputs are standardized using internal helpers:

- [`standardize_jurisdiction_code()`](https://ptompalski.github.io/CanadaForestAllometry/reference/standardize_jurisdiction_code.md)
  for `jurisdiction`

- [`standardize_species_code()`](https://ptompalski.github.io/CanadaForestAllometry/reference/standardize_species_code.md)
  for `species` (keeps `"ALL"` as-is)

Rules:

- For British Columbia (`"BC"`), `species` is OPTIONAL:

  - If `species` is missing, the function falls back to
    `Species == "ALL"` and warns.

  - If `BEC_zone` is provided and can be mapped, criteria are looked up
    using (Province + Species + BEC_group).

  - If `BEC_zone` is missing/unknown, criteria fall back to a
    conservative BC BEC-independent layer using `BEC_group == "UNKNOWN"`
    (species-specific if present, otherwise `Species == "ALL"`).

  - If no exact species match is found, the function falls back to
    genus-level rules (e.g., `PICE.GLA` -\> `PICE.SPP`) and then to
    `Species == "ALL"`.

- For other jurisdictions, criteria are jurisdiction-level (Province)
  only and species/BEC are ignored.

Stump height is returned in metres (converted from centimetres).

## Examples

``` r
get_merch_criteria("ON")
#> # A tibble: 1 × 5
#>   jurisdiction species stumpht_m topdbh_cm mindbh_cm
#>   <chr>        <chr>       <dbl>     <dbl>     <dbl>
#> 1 ON           ALL           0.3      13.1         9
get_merch_criteria("PEI")                        # standardized to "PE"
#> # A tibble: 1 × 5
#>   jurisdiction species stumpht_m topdbh_cm mindbh_cm
#>   <chr>        <chr>       <dbl>     <dbl>     <dbl>
#> 1 PE           ALL          0.15         8         9
get_merch_criteria("BC", "PSEU.MEN", "CWH")       # BEC-aware
#> # A tibble: 1 × 5
#>   jurisdiction species  stumpht_m topdbh_cm mindbh_cm
#>   <chr>        <chr>        <dbl>     <dbl>     <dbl>
#> 1 BC           PSEU.MEN       0.3        15      17.5
get_merch_criteria("BC", "PICE.GLA", "CWH")       # genus fallback to PICE.SPP if present
#> # A tibble: 1 × 5
#>   jurisdiction species  stumpht_m topdbh_cm mindbh_cm
#>   <chr>        <chr>        <dbl>     <dbl>     <dbl>
#> 1 BC           PICE.GLA       0.3        15      17.5
get_merch_criteria("BC")                          # falls back to BC ALL + UNKNOWN (warns)
#> Warning: For jurisdiction 'BC', `species` was not provided; using BC fallback Species='ALL'.
#> Warning: BEC_zone missing; using conservative BC 'UNKNOWN' utilization.
#> # A tibble: 1 × 5
#>   jurisdiction species stumpht_m topdbh_cm mindbh_cm
#>   <chr>        <chr>       <dbl>     <dbl>     <dbl>
#> 1 BC           ALL           0.3        15      17.5
```
