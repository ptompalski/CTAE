# AGENTS.md — CanadaForestAllometry

Guidance for AI agents and developers working in this repository. This file
covers **project-specific** structure, conventions, and workflows. For general
R coding style (native pipe, tidyverse idioms, testthat 3e, roxygen2), see
[`.codex_instructions`](.codex_instructions) — that guidance still applies here.

## What this package is

`CanadaForestAllometry` provides a unified interface to published allometric
models for Canadian forests: tree **volume** (total and merchantable),
**aboveground biomass (AGB)**, **volume-to-biomass** conversion, **site index /
productivity**, and supporting utilities. Models are implemented faithfully to
their source publications and exposed through a consistent, vectorized API.

- **Version**: see [DESCRIPTION](DESCRIPTION) (currently 0.8.5)
- **License**: LGPL (>= 3); Copyright His Majesty the King in Right of Canada (NRCan)
- **R**: >= 4.1.0
- **Imports**: dplyr, tibble, tidyr, stringr, cli, rlang, purrr, stats, stringi, glue
- **Docs site**: https://ptompalski.github.io/CanadaForestAllometry/

## Repository layout

| Path | Purpose |
|------|---------|
| `R/` | Source code (~55 files): model functions, registries, helpers |
| `R/sysdata.rda` | Compiled internal parameter tables (all model coefficients) |
| `data/` | Public datasets: `merchcrit`, `species_code_lookup`, `species_dictionary` |
| `data-raw/` | Source CSVs of parameters + `preprocess_data.R` build script |
| `tests/testthat/` | Test suite (~41 files), incl. `_snaps/` snapshots |
| `tmp/` | Scripts + CSVs generating external comparison/benchmark values |
| `vignettes/articles/` | Quarto (`.qmd`) articles for the pkgdown website |
| `man/` | roxygen2-generated docs — **never edit by hand** |
| `_packageSetup.R` | Scratch script of dev workflow commands |

## Naming conventions

Function names encode model lineage as `prefix_[author][year]`:

| Prefix | Meaning | Example |
|--------|---------|---------|
| `vol_` | Tree volume model | `vol_kozak88`, `vol_ung2013` |
| `si_` | Site index / productivity | `si_thrower1994`, `si_huang1994` |
| `agb_` | Aboveground biomass | `agb_lambert_ung` |
| `ytbh_` | Year-to-breast-height age helper | `ytbh_thrower1994` |
| `get_` | Lookup / retrieval | `get_volume_params`, `get_merch_criteria` |
| `assert_` | Internal input validation | `assert_numeric_vec` |
| `*_model_registry` | Metadata table for a model family | `volume_model_registry` |

Internal per-element helpers use a leading dot / `_one` suffix (e.g.
`.thrower1994_height_one`).

## API conventions

- **Vectorized**: user-facing model functions operate element-wise and are safe
  inside `dplyr::mutate()`. Inputs are recycled to a common length; incompatible
  lengths raise an explicit error. No `rowwise()`/`pmap()` wrappers required.
- **Return tibbles** with snake_case columns (e.g. `vol_total`, `vol_merchantable`).
- **Stable public API** — preserve signatures and return types unless a change is
  explicitly requested.
- **Species codes**: NFI format `GENUS.SPEC` (e.g. `PICE.MAR` = black spruce);
  compact `PICEGLA` is accepted and normalized; genus-level `PICE.SPP` supported.
  Use `translate_species_code()` to convert between NFI, CANFI, jurisdiction, and
  common/scientific names.
- **Jurisdiction / subregion**: 2-letter province codes (AB, BC, ON, QC, ...);
  parameter lookups apply subregion fallback logic where models require it.
- **Validation**: user-facing functions validate early with cli/rlang-style
  structured errors; internal helpers may assume validated inputs.

## Parameters and internal data

- Model coefficients live in `data-raw/*.csv`, are transformed by
  `data-raw/preprocess_data.R`, and compiled into `R/sysdata.rda`.
- To change parameters: edit the CSV(s), re-run `source("data-raw/preprocess_data.R")`,
  then **restart R** and `devtools::load_all()`. Do not hand-edit `sysdata.rda`.
- Public datasets in `data/` are documented via roxygen blocks in `R/data.R`.

## Adding a new model

When implementing a model from a source publication, follow the
`add-allometric-model` skill (`.claude/skills/add-allometric-model/`), which stages
extraction (with human review) before code.

1. Create `R/<prefix>_<author><year>.R` following the naming pattern above.
2. Add parameters to `data-raw/` (CSV) and rebuild `sysdata.rda` if needed.
3. Write a full roxygen block (`@title`, `@description`, `@param`, `@return`,
   `@examples`, `@references`, `@export`).
4. Register the model in the relevant `*_model_registry()` table.
5. Add `tests/testthat/test-<prefix>_<author><year>.R` covering normal cases,
   edge cases, invalid inputs, vectorization/recycling, and — where a published
   benchmark exists — numeric comparison against reference values.
6. Add `REFERENCES.bib` entry and mention in the appropriate vignette if relevant.
7. Update `NEWS.md`.

### Comparison / benchmark values

`tmp/` holds scripts (e.g. `generate_*_comparison_values.R`) that produce CSVs of
expected outputs used to validate model fidelity against original publications or
external implementations (Java/C#/C#). When implementing a model with an external
reference, add a comparison CSV and test against it.

## Development workflow

Prefer `devtools::load_all()` over `library()` during development. Common commands
(see [`_packageSetup.R`](_packageSetup.R)):

```r
source("data-raw/preprocess_data.R")  # rebuild internal data, then restart R
devtools::document()                  # regenerate roxygen docs + NAMESPACE
devtools::load_all()                  # reload package in dev
devtools::test()                      # run testthat suite
devtools::check()                     # full R CMD check
covr::package_coverage()              # coverage report
pkgdown::build_site()                 # build docs website
usethis::use_version("patch")         # bump version
```

Prefer **pak** for installing dependencies.

## Pre-release checklist

- `devtools::document()`
- `devtools::test()`
- `devtools::check()`
- `covr::package_coverage()`
- `urlchecker::url_check()`
- `spelling::spell_check_package()`
- Update `NEWS.md`

## Guardrails specific to this package

- **Scientific fidelity first**: do not alter a model's numerical behavior unless
  explicitly asked. Faithfulness to the source publication is the priority.
- Keep the public API and return types stable.
- Never edit `man/*.Rd` or `sysdata.rda` directly.
- Never hardcode absolute paths; use package-safe access patterns.
- Add dependencies only with clear justification; the current dependency set is
  intentionally lean.
