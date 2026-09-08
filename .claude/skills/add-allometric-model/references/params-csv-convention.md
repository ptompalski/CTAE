# Parameters CSV convention

Model coefficients live as CSV files in `data-raw/`, are assembled by
`data-raw/preprocess_data.R`, and are compiled into `R/sysdata.rda` (internal data).
User-facing functions read them via accessors like `get_volume_params()` or a
model-specific `.<model>_prepare()` helper.

## CSV structure

- **One row per parameter set.** For most models this is one row per species (and,
  where relevant, per subregion/jurisdiction or model variant).
- **Species identified by NFI code** in a column (e.g. `nfi_species` = `PICE.MAR`).
  Keep the source's own species label in a separate column too, so the mapping is
  auditable.
- **One column per coefficient**, named to match the model's own symbols where
  reasonable (e.g. `b1`, `b2`, `a0`; or `b1_const`, `b1_lnS_coef` for structured
  forms). Keep names stable and lowercase/snake_case.
- **Provenance columns** are encouraged and already used in existing files:
  a `page` column and a `source_short` (or similar) column. See
  `data-raw/Thrower1994_parameters.csv` for a worked example.
- Empty cells are allowed for coefficients that don't apply to a given row (the
  Thrower1994 table uses this heavily across its several model forms).

## Look at an existing example first

Before creating a new CSV, read a comparable existing one to match style:

- Structured multi-form SI table: `data-raw/Thrower1994_parameters.csv`
- Simple per-species coefficient table: `data-raw/Huang1994_parameters.csv`,
  `data-raw/Payandeh_1974_parameters.csv`
- Volume/taper coefficients: `data-raw/kozak1994_parameters.csv`

## Wiring into preprocess_data.R

`data-raw/preprocess_data.R` reads each CSV and writes internal data. The pattern
(see the Thrower1994 block around line 908) is:

```r
parameters_<Model> <- readr::read_csv(
  "data-raw/<Model>_parameters.csv",
  show_col_types = FALSE
)
# ...any light post-processing (type coercion, code standardization)...
usethis::use_data(parameters_<Model>, overwrite = TRUE, internal = TRUE)
```

Note: `usethis::use_data(..., internal = TRUE)` writes **all** internal objects to a
single `R/sysdata.rda`. When run, it must see every object intended for internal
data in the session, so `preprocess_data.R` is run top-to-bottom. Follow the file's
existing structure; add your block alongside the others rather than calling
`use_data(internal = TRUE)` in isolation.

## After editing parameters

1. `source("data-raw/preprocess_data.R")`
2. **Restart R** (internal data is loaded at package load).
3. `devtools::load_all()`

Never hand-edit `R/sysdata.rda`.
