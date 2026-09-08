# Carmean (1996) site index models for northwest Ontario

Implementation of the northwest Ontario site-index equations documented
in Carmean (1996) and the appendix equations transcribed from that
report.

## Usage

``` r
si_carmean1996(age, height = NULL, si = NULL, species)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years), with `age > 0`.

- height:

  Optional numeric vector. Total tree height (m). If provided, `si` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at
  breast-height age). If provided, `height` is predicted.

- species:

  Character vector of species codes (e.g., `"PINU.BAN"`).

## Value

A tibble with columns:

- height:

  Predicted total tree height (m), returned when input `si` is provided.

- si:

  Predicted site index (m), returned when input `height` is provided.

## Details

**Species coverage:** `PINU.BAN`, `PICE.MAR`, `PICE.GLA`, `ABIE.BAL`,
`POPU.TRE`, `BETU.PAP`, `LARI.LAR`.

**Geographic use:** northwest Ontario.

**Age definition note:** `age` is breast-height age (years).

**Base-age note:** site index is defined as total height (m) at 50 years
breast-height age.

**Forward/inverse consistency note:** for `PICE.MAR`, `ABIE.BAL`, and
`BETU.PAP`, the published site-index prediction equations used for
`height -> si` are separate fitted equations from Appendix IV rather
than exact algebraic inverses of the forward height-curve equations. As
a result, predictions from `height -> si` may differ slightly from the
input height even when `age = 50`.

Provide exactly one of `height` or `si`:

- If `height` is provided, the function predicts `si`.

- If `si` is provided, the function predicts `height`.

Inputs and outputs are metric (m). Some source equations were published
in mixed unit form and are converted internally using the
species-specific metadata stored in the internal parameter table.

## References

Carmean, W. H. (1996). Site-quality evaluation, site-quality
maintenance, and site-specific management for forest land in northwest
Ontario. Ontario Ministry of Natural Resources, Northwest Science and
Technology, Technical Report TR-105.

## Examples

``` r
si_carmean1996(
  age = c(20, 40, 60),
  si = c(10, 14, 18),
  species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
)
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   5.29
#> 2  12.0 
#> 3  20.5 

si_carmean1996(
  age = c(20, 40, 60),
  height = c(6, 12, 19),
  species = c("PINU.BAN", "PICE.MAR", "PICE.GLA")
)
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  11.3
#> 2  14.0
#> 3  16.5
```
