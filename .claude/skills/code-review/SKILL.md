---
name: code-review
description: >-
  Review code changes in the CanadaForestAllometry R package and report findings
  grouped by severity. Use when reviewing a diff, a pull request, a branch, or new
  or modified model functions (vol_/si_/agb_/ytbh_), helpers, registries, params
  (data-raw/), tests, or docs. Prioritizes scientific fidelity and API stability.
  Runs targeted tests/benchmarks as evidence. Reports only — does not patch unless
  the user asks.
---

# Review code in CanadaForestAllometry

This skill reviews a **set of changes** against this package's standards and emits
findings **grouped by severity**, each tied to `file:line` with a concrete fix.

The governing principle of the package is **scientific fidelity**: the top question
for any model code is *does it reproduce the source publication exactly?* A sign
error, a wrong coefficient, or a unit/bark/merchantability mismatch is **critical**
even when the code reads cleanly. Read `AGENTS.md` at the repo root first — it
defines the naming, API, and data-flow conventions this review enforces. The sibling
`add-allometric-model` skill defines what a correct change *produces*; review checks
that those artifacts are present and correct.

**This skill reports only.** It may run tests and benchmarks, but it does not modify
source. Offer to patch findings only if the user asks.

## Overview

```
Stage 0  Scope      files the user names; else `git diff` (working tree, then vs main)
Stage 1  Classify   bucket each changed file (model fn / helper / registry / data-raw / test / docs / infra)
Stage 2  Review      apply the checklists in references/review-checklists.md; fidelity pass for model code
Stage 3  Verify      RUN targeted tests / benchmark comparisons as evidence
Stage 4  Report      findings grouped by severity, must-fix vs. consider (references/severity-and-report.md)
```

## Stage 0 — Scope

1. If the user named files or a PR/branch, review exactly those.
2. Otherwise run `git diff` on the **working tree**. If that is empty, compare the
   current branch against **`main`** (`git diff main...HEAD`). If `main` does not
   exist, ask for the base.
3. List the changed files back to the user before reviewing, so the scope is agreed.
   Never expand to the whole package when a change set is implied.

## Stage 1 — Classify

Bucket each changed file — the bucket selects which checklist layers apply:

| Bucket | Signals | Primary concerns |
|--------|---------|------------------|
| Model function | `R/{vol,si,agb,ytbh}_*.R` | **Fidelity**, API, validation |
| Helper | `.one`/dot-prefixed, internal utils | Correctness, may assume validated input |
| Registry | `*_model_registry.R` | Row present, keys match `REFERENCES.bib` |
| Params / data | `data-raw/*.csv`, `preprocess_data.R` | Coefficient integrity, rebuild path |
| Test | `tests/testthat/test-*.R`, `_snaps/` | Coverage of new branches, benchmark tier |
| Docs | roxygen, `man/`, `NEWS.md`, vignettes | Regenerated (not hand-edited), one-line NEWS |
| Infra | CI, DESCRIPTION, tooling | Light touch — flag deps, obvious breakage |

Infrastructure changes get a **light pass**: flag new dependencies, absolute paths,
or obvious breakage, but do not deep-review tooling.

## Stage 2 — Review

Apply the per-bucket checklists in `references/review-checklists.md`. For any model
code, run the **fidelity pass** first — it outranks every style concern:

- Equations trace to the cited source / `data-raw/*_model-spec.md`.
- Coefficients come from `sysdata.rda` / CSV, **never hardcoded** in the function.
- Units and bark / total-vs-merchantable conventions are correct.
- Edge cases handled: 0 / negative DBH, height below breast height, missing params.
- A benchmark comparison exists (`tmp/generate_*_comparison_values.R`) and passes.

Then check API conformance (vectorization + explicit recycling errors, snake_case
tibble output, **stable public signatures**, species-code normalization, subregion
fallback), validation/errors, naming/structure, tests, and style/housekeeping.

## Stage 3 — Verify (run code)

Prefer evidence over reading. `devtools::load_all()`, then for changed files run
targeted checks — `testthat::test_file()`, `covr::file_coverage()` — and re-run any
relevant `tmp/generate_*_comparison_values.R` benchmark. Report results as evidence.
Running checks is fine; **do not modify source** to make them pass.

## Stage 4 — Report

Group findings by severity (Critical / Major / Minor / Nit; see
`references/severity-and-report.md`). Each finding:
`file:line — [Severity] problem → suggested fix`. Close with a count and an explicit
**must-fix vs. consider** split. Report only; offer patches on request.

## Guardrails

- Fidelity outranks style — never let clean code mask a wrong number.
- Report only. Do not edit source; offer patches only when asked.
- Run checks, never modify source to make them pass.
- Flag any change to a public signature or return type as at least Major.
- Treat hand-edited `man/*.Rd` or `R/sysdata.rda` as Critical.
- Flag new dependencies and any absolute path.
- Keep scope to the change set; do not review the whole package.
