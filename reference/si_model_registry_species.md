# Site-index model registry with species coverage

Site-index model registry with species coverage

## Usage

``` r
si_model_registry_species()
```

## Value

A tibble like
[`si_model_registry()`](https://ptompalski.github.io/CanadaForestAllometry/reference/si_model_registry.md)
plus:

- `species` (list-column of character vectors)

- `n_species` (integer)

- `species_text` (collapsed string for printing)
