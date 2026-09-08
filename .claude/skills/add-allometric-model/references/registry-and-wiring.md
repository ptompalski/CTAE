# Registry and wiring

After the parameters exist and the R function is written, wire the model into the
package's discovery and documentation surfaces.

## Model registry

Add a row to the family's registry so the model is discoverable and so the
comparison-value generator can find it.

- Site index: `R/si_model_registry.R` (`si_model_registry()`)
- Volume: `R/volume_model_registry.R` (`volume_model_registry()`)

Read the existing registry function and add your model consistently across **all**
parallel columns (they are constructed as parallel vectors, so every vector must gain
one element in the same position). Typical columns for the SI registry:

| Column | Meaning |
|--------|---------|
| `model_id` | Stable id used by metadata/helpers (e.g. `"thrower1994"`; append a variant suffix like `_si50` when a function has variants) |
| `reference` | BibTeX key, e.g. `"@Thrower1994"` — must exist in `REFERENCES.bib` |
| `engine` | The R function name that implements it (e.g. `"si_thrower1994"`) |
| `age_basis` | `"total"` or `"breast_height"` |
| `fixed_args` | Named list of args identifying a variant, or empty |
| `species_manual` | Character vector of covered NFI codes, when listed directly |

Match the exact column set of the registry you are editing; do not invent columns.

## Bibliography

Add an entry to `REFERENCES.bib` keyed to match the registry `reference` column
(e.g. `@Thrower1994`). Include full citation details from the source document.

## Vignette (optional)

If the model belongs in a family vignette, mention it in the relevant
`vignettes/articles/*.qmd` (e.g. `Site-Index-Models.qmd`, `Tree-Volume-Models.qmd`).

## NEWS.md

Add a **single brief sentence** under the current development version naming the new
model. Keep it to one sentence; do not enumerate parameters, species, or validation
details there (record those in the model spec instead). If the model shipped with
only a plausibility check, note "no source benchmark" in the model spec — not NEWS.

Example: `* Added \`si_nigh2002\`, a site index model for interior BC lodgepole pine.`

## Documentation build

Run `devtools::document()` to regenerate `man/*.Rd` and `NAMESPACE` from the roxygen
block. Never edit `man/*.Rd` by hand.

## Verification checklist

- [ ] `source("data-raw/preprocess_data.R")` run, R restarted, `load_all()` clean
- [ ] `devtools::document()` regenerates docs without warnings
- [ ] `devtools::test()` passes, including the new test file
- [ ] Coverage of the new `R/<fn>.R` is 100% (`covr::file_coverage()`); unreachable lines marked `# nocov`
- [ ] New model compared against similar existing function(s); discrepancies reported
- [ ] Registry row present and internally consistent
- [ ] `REFERENCES.bib` entry added
- [ ] `NEWS.md` updated with a single brief sentence
- [ ] `devtools::check()` run if the change is substantial
