# Audit dimensions

Scan each dimension across the audit surface. Every finding must cite evidence
(files:lines or an inventory excerpt), not impressions.

## 1. Duplication

- **Structural duplication** — functions/blocks with the same control flow that could
  share an internal helper (e.g. repeated recycling+validation preamble, repeated
  species-code normalization, repeated tibble assembly).
- **Helper reuse gaps** — a `.one`/dot-prefixed helper that several models
  reimplement inline instead of calling.
- **Numerical vs structural** — *always separate these*. Same equation form with
  different coefficients is **not** a merge candidate on its own; it is a fidelity
  question. Only structural duplication behind an unchanged API is safe to consolidate.
- Report duplication as: what repeats, where (list all sites), and the proposed shared
  internal — with explicit fidelity risk if any numeric logic is involved.

## 2. Parameter-storage consistency

- Which models read coefficients from `sysdata`/`data-raw` CSV vs. **hardcode**
  literals in the function body?
- Single-species / single-row models are the usual offenders — flag any that hardcode
  when the family convention is sysdata.
- Recommend moving hardcoded params to the `data-raw/` → `preprocess_data.R` → sysdata
  pipeline for consistency and auditability (note the rebuild + restart-R step).

## 3. Validation & error consistency

- Divergent input-validation idioms across a family (`assert_*` vs ad-hoc `stopifnot`
  vs none); inconsistent error signalling (cli vs rlang vs base `stop`).
- Recommend standardizing on the package's `assert_*` + cli/rlang pattern.

## 4. Return-shape consistency

- Same-family functions returning differently named/typed columns, or tibble vs
  data.frame, for equivalent quantities.
- Flag deviations from snake_case tibble outputs.

## 5. Dead / unused code

- Internal functions with no call sites; unreachable branches; commented-out blocks;
  params tables in sysdata not referenced by any function.
- Confirm "unused" with a real search before recommending removal (exports and
  test-only usage count as used).

## 6. Naming & structure drift

- Functions/helpers not following `prefix_[author][year]` / `.one` conventions.
- Files misplaced relative to `R/` layout; multiple unrelated models in one file.

## 7. Dependency hygiene

- Imports declared but unused; heavy use of a dependency where a lighter/base option
  exists; any de-facto dependency not declared.
- A refactor should never *add* a dependency.
