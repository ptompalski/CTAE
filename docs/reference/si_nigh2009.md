# Nigh, Thomas, Yearsley and Wang (2009) paper birch height-age (site index) model for British Columbia

Unified, vectorized implementation of the log-logistic height-age (site
index) model in Nigh, Thomas, Yearsley and Wang (2009) for paper birch
(*Betula papyrifera*) in British Columbia.

## Usage

``` r
si_nigh2009(age, height = NULL, si = NULL, model = 1, bec_zone = NULL)
```

## Arguments

- age:

  Numeric vector. Breast-height age (years).

- height:

  Optional numeric vector. Site height (m). If provided, \`si\` is
  predicted.

- si:

  Optional numeric vector. Site index (m, base age 50 years at breast
  height). If provided, \`height\` is predicted.

- model:

  Model variant: \`1\` (base, default), \`2\` (operational), or \`3\`
  (zonal). See Details.

- bec_zone:

  Optional character vector selecting the zonal (\`model = 3\`)
  coefficients. One of \`"ICH"\`, \`"IDF"\`, \`"SBS"\`. Required when
  \`model = 3\` and ignored otherwise.

## Value

A tibble with a single column:

- height:

  Predicted site height (m), returned when \`si\` is provided.

- si:

  Predicted site index (m), returned when \`height\` is provided.

## Details

**Model scope (species coverage):** paper birch, NFI code `BETU.PAP`.

**Age definition note:** \`age\` is breast-height age (BHA, years). The
model is conditioned to return \`height = si\` exactly at BHA 50.

**Base-age note:** site index is site height at breast-height age 50.

**Model variants:** the paper fits three variants of one log-logistic
form (eq. 1) \$\$HT = 1.3 + (SI - 1.3) \times \frac{1 + e^{a_0 + a_1
\ln(49.5) + a_2 \ln(SI - 1.3)}} {1 + e^{a_0 + a_1 \ln(BHA - 0.5) + a_2
\ln(SI - 1.3)}}\$\$ differing only in their coefficients:

- \`model = 1\`:

  Base log-logistic fit (Table 2). Default.

- \`model = 2\`:

  Operational form of the mixed model. Recommended by the authors when
  the biogeoclimatic zone is unknown or is not ICH/IDF/SBS.

- \`model = 3\`:

  Zonal indicator-variable model. Only \\a_1\\ carries a significant
  zone effect (SBS vs. ICH/IDF). Supply \`bec_zone\` (one of \`"ICH"\`,
  \`"IDF"\`, \`"SBS"\`); recommended for those zones.

Because \\SI\\ appears both as a multiplier and inside a logarithm, eq.
1 has no closed-form inverse in \\SI\\; when predicting site index the
equation is solved numerically.

Provide exactly one of \`height\` or \`si\`:

- If \`si\` is provided, the function predicts \`height\`.

- If \`height\` is provided, the function predicts \`si\`.

## References

Nigh, G.D., Thomas, K.D., Yearsley, K., and Wang, J. (2009).
Site-dependent height-age models for paper birch in British Columbia.
Northwest Science 83(3): 253–261.
[doi:10.3955/046.083.0308](https://doi.org/10.3955/046.083.0308)

## Examples

``` r
# Base model: predict height from age + site index
si_nigh2009(age = c(25, 50, 80), si = c(12, 18, 24))
#> # A tibble: 3 × 1
#>   height
#>    <dbl>
#> 1   7.18
#> 2  18   
#> 3  28.5 

# Base model: predict site index from age + height
si_nigh2009(age = c(25, 50, 80), height = c(8, 18, 26))
#> # A tibble: 3 × 1
#>      si
#>   <dbl>
#> 1  13.2
#> 2  18  
#> 3  21.5

# Operational model (recommended when zone is unknown)
si_nigh2009(age = 60, si = 18, model = 2)
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   19.8

# Zonal model
si_nigh2009(age = 60, si = 18, model = 3, bec_zone = "SBS")
#> # A tibble: 1 × 1
#>   height
#>    <dbl>
#> 1   19.7
```
