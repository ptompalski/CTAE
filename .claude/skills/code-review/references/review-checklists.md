# Review checklists

Apply only the layers relevant to a changed file's bucket (Stage 1). For model
functions, run **Fidelity** first — it outranks every other layer.

## Fidelity (model functions — highest priority)

- [ ] Every equation traces to the cited source or `data-raw/*_model-spec.md`.
- [ ] Coefficients are pulled from `R/sysdata.rda` / a `data-raw/` CSV, **not**
      hardcoded as literals in the function body.
- [ ] Units are correct and consistent (cm vs m, dm³ vs m³); conversions explicit.
- [ ] Bark basis and total-vs-merchantable are the ones the source specifies.
- [ ] Invertible models (e.g. height ↔ SI) implement both directions consistently.
- [ ] Age basis / base age matches the source (SI models).
- [ ] Edge cases handled: 0 / negative DBH, height below breast height, out-of-domain
      inputs, missing/unsupported species or jurisdiction.
- [ ] A benchmark comparison exists (`tmp/generate_*_comparison_values.R`) and the
      test compares against it; fidelity vs. plausibility tiers not conflated.

## API conformance

- [ ] Vectorized / element-wise; safe inside `dplyr::mutate()`.
- [ ] Inputs recycled to common length; incompatible lengths raise an explicit error.
- [ ] Returns a **tibble** with **snake_case** columns matching the family convention.
- [ ] Public signature and return type unchanged (any change → at least Major).
- [ ] Species codes normalized (NFI `GENUS.SPEC`, compact, genus-level) via the
      standard translator; jurisdiction/subregion fallback applied where required.

## Validation & errors

- [ ] User-facing functions validate inputs early with `assert_*` helpers.
- [ ] Errors use cli / rlang structured signalling (`cli::cli_abort` / `rlang::abort`).
- [ ] Internal helpers may assume validated input — no redundant re-validation.

## Naming & structure

- [ ] Function named `prefix_[author][year]`; file placed correctly in `R/`.
- [ ] Internal per-element helpers use `.one` suffix / leading-dot naming.
- [ ] Model registered in the relevant `*_model_registry()` table; registry
      `reference` key matches a `REFERENCES.bib` entry.

## Tests

- [ ] Covers normal cases, edge cases, invalid inputs, and vectorization/recycling.
- [ ] Numeric benchmark comparison present where a published reference exists.
- [ ] New branches (each error path, species/variant, both directions) are covered;
      check with `covr::file_coverage()`. Unreachable lines marked `# nocov` w/ reason.
- [ ] Snapshot (`_snaps/`) changes are intentional and justified, not blind accepts.

## Style & housekeeping

- [ ] Native pipe `|>`; tidyverse idioms; testthat 3e.
- [ ] Full roxygen block: `@param`, `@return`, `@examples`, `@references`, `@export`.
- [ ] `man/*.Rd` regenerated via `devtools::document()`, **not hand-edited**.
- [ ] `R/sysdata.rda` rebuilt via `preprocess_data.R`, **not hand-edited**.
- [ ] `NEWS.md` has a single brief sentence under the dev version.
- [ ] No new dependency without justification; no hardcoded absolute paths.

## Params / data changes

- [ ] Coefficients in the CSV match the source digit-for-digit (spot-check flagged ones).
- [ ] `preprocess_data.R` wires the CSV in; rebuild path documented (rerun + restart R).
- [ ] Downstream functions/tests updated if column shapes changed.

## Infra (light pass only)

- [ ] Flag new dependencies (DESCRIPTION `Imports`), absolute paths, obvious breakage.
- [ ] Do not deep-review CI/tooling unless asked.
