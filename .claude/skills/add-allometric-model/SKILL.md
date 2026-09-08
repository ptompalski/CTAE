---
name: add-allometric-model
description: >-
  Implement a new allometric model in the CanadaForestAllometry R package from a
  source publication (PDF/report describing the model form and parameters). Use
  when adding a new volume (vol_), site index (si_), aboveground biomass (agb_),
  or year-to-breast-height (ytbh_) model, extracting equations and coefficients
  from a source document, or wiring a new model into the package's params tables,
  registries, tests, and docs. Staged workflow: extract and STOP for human review
  of parameters before writing any R code.
---

# Add a new allometric model to CanadaForestAllometry

This skill turns a **source document** (peer-reviewed paper, technical report)
into a faithful, tested R function that follows this package's conventions.

The governing principle of this package is **scientific fidelity**: the
implementation must match the source publication exactly. LLMs are good at
reconstructing equations but **unreliable at transcribing numbers**, so this
workflow treats parameter extraction as a *verification problem* and **stops for
human review before any R code is written**.

Read `AGENTS.md` at the repo root first — it defines naming, API, and data-flow
conventions this skill assumes.

## Overview

```
Stage 0    Intake       confirm the source document + model family
Stage 0.5  Triage       fast skim: CAN this be built from this document?
                        verdict GO / GO WITH GAPS / NO-GO; on GO, name the function
                        ==> consult user if not GO
Stage 1    Extraction   read source -> model-spec.md + candidate params CSV
                        + benchmark plan   ==> STOP, human verifies numbers
Stage 2    Implement    params -> sysdata; R function; registry; tests; docs; NEWS
```

Do **not** proceed from Stage 0.5 to Stage 1 unless the verdict is GO (or the user
accepts documented gaps). Do **not** proceed from Stage 1 to Stage 2 until the user
has approved the extracted parameters and equations.

## Stage 0 — Intake

1. Confirm the source document is in `sources/` (already gitignored, and already
   contains per-family subfolders: `site_index/`, `volume_models/`,
   `biomass_equations/`, `height-diameter/`). If the user has not placed it, ask.
2. Identify the **family** (`vol_` / `si_` / `agb_` / `ytbh_`). This is enough to
   begin triage; defer committing to a function name until feasibility is confirmed.

## Stage 0.5 — Feasibility triage (then name, or consult if not GO)

Before investing in full extraction, do a **fast skim** to decide whether the model
can be faithfully implemented *from this document*. This is a go/no-go check, not
extraction — keep it shallow and cheap. See `references/feasibility-triage.md`.

Confirm, at a glance:

1. **Equations present** — the model form(s), and both directions if the model is
   invertible (e.g. height↔SI).
2. **Parameters present and complete** — coefficient tables actually appear *in this
   document* (not "see [other report]"), covering the species/regions claimed.
3. **Units + variable definitions** are stated.
4. **Age basis / base age** given (SI models).
5. **Validation feasible** — a worked example / reference table (fidelity), or a
   viable same-family plausibility fallback.
6. **Legibility** — text vs. scanned; tables readable when rendered (see the
   extraction pipeline).

Emit a verdict with a one-line justification each:

- **GO** — everything needed is present and legible. **Now name the function**
  using the `AGENTS.md` pattern `prefix_<author><year>` (e.g. `si_nigh2016`),
  confirming the citation from the rendered text (filenames can be wrong), then
  proceed to Stage 1.
- **GO WITH GAPS** — implementable but something is missing (e.g. a companion
  parameter report, no fidelity benchmark). List the gaps and **consult the user**
  on how to resolve them before naming the function or proceeding.
- **NO-GO** — cannot be faithfully implemented from this document (e.g. parameters
  not published here, model form incomplete). Explain why and **stop**.

Only a clean GO proceeds automatically.

## Stage 1 — Extraction (then STOP)

Extract from the source with the pipeline in `references/extraction-rules.md`.
Key point learned from validation: the `read` tool does **not** open PDFs in this
environment, and `pdftotext` can silently **corrupt parameter tables** even when
the text layer looks healthy. So:

- Use `pdftotext -layout` (or `pdftools::pdf_text()`) for **prose** (equations,
  units, scope, species names).
- For **parameter tables**, always **render the page to an image** at ~300 dpi
  (`pdftools::pdf_render_page()`) and read the *image* to transcribe coefficients.
  Never trust the raw text layer for coefficient values.
- OCR (R `tesseract` package) is the fallback for scanned image-only PDFs.

Produce, **without writing any R model code**:

1. **`data-raw/<Model>_model-spec.md`** — from `assets/model-spec-template.md`.
   (Write it under `data-raw/`, which is version-controlled; the whole `sources/`
   tree is gitignored, so a spec placed there would not be tracked.)
   Capture every equation (both directions if the model is invertible), the exact
   model form, variable definitions with **units**, age basis / base age, domain of
   applicability, species coverage, and per-species notes.

2. **Candidate parameters CSV** in the `data-raw/` convention
   (see `references/params-csv-convention.md`). Copy every coefficient
   **digit-for-digit** with a **source locator** (table/page) per value. Any value
   that is illegible or ambiguous must be **flagged, never guessed**.

3. **Benchmark plan** (see `references/validation-tiers.md`): list any worked
   examples or reference tables in the source (→ *fidelity* tier). If none exist,
   say so explicitly and name the existing same-family functions you will use for a
   *plausibility* check.

Then **STOP and ask the user to verify** the params CSV and equations against the
source document. This is the single most important step; do not skip it.

## Stage 2 — Implementation (only after approval)

Follow `references/registry-and-wiring.md` for exact wiring details.

1. **Parameters → internal data.** Add the CSV to `data-raw/`, wire it into
   `data-raw/preprocess_data.R`, run `source("data-raw/preprocess_data.R")`,
   then **restart R** and `devtools::load_all()`. Never hand-edit `R/sysdata.rda`.
2. **Function.** Write `R/<fn>.R` from the appropriate family template in `assets/`
   (`function-template-si.R`, `function-template-vol.R`, or
   `function-template-generic.R`). Templates are **scaffolding to adapt**, not molds
   to force the model into. Full roxygen block; vectorized with length-recycling and
   explicit `cli::cli_abort`/`rlang::abort`; standardized species/jurisdiction codes;
   snake_case tibble output.
3. **Register.** Add a row to `R/si_model_registry.R` or `R/volume_model_registry.R`.
   Add a `REFERENCES.bib` entry keyed to match the registry `reference` column.
4. **Comparison values.** Write `tmp/generate_<fn>_comparison_values.R` from
   `assets/comparison-generator-template.R` and generate the CSV.
5. **Compare against existing similar functions.** Always compare the new model
   against the closest already-implemented same-family function(s) for overlapping
   species/regions, and **report any discrepancies** to the user — magnitude,
   direction, and where they diverge (see `references/validation-tiers.md`). Do this
   even when a fidelity benchmark exists; it is a cross-check, not a substitute.
6. **Tests (target 100% coverage).** Write `tests/testthat/test-<fn>.R` from
   `assets/test-template.R`: structural + input-validation tests **plus** the
   applicable validation tier (fidelity vs plausibility — never conflate them; see
   `references/validation-tiers.md`). Aim for **100% line coverage** of the new
   function and its internal helpers. Check with
   `covr::function_coverage()` / `covr::file_coverage()`; add tests for every branch
   (each error path, each species/variant, both prediction directions). If a line is
   genuinely unreachable, mark it `# nocov` with a one-line reason rather than leaving
   it uncovered.
7. **Verify.** `devtools::document()`, `devtools::test()`, confirm coverage of the
   new file is 100%, and run targeted `devtools::check()` if warranted.
8. **NEWS.** Add a **single brief sentence** to `NEWS.md` under the current dev
   version. If the model shipped with only a plausibility check, note "no source
   benchmark" in the model spec (keep the NEWS line itself to one sentence).
9. **Report.** Summarize what was implemented, which validation tier applied, the
   comparison against existing functions (including any discrepancies), the coverage
   result, and any flagged/uncertain parameters for the user's final review.

## Guardrails

- Never paraphrase or "clean up" a coefficient; copy exactly, cite its location,
  flag anything uncertain rather than guessing.
- Never present a plausibility check as proof of fidelity.
- Target 100% test coverage of every implemented function and its helpers.
- Always cross-check a new model against similar existing functions and report
  discrepancies, even when a fidelity benchmark exists.
- Keep `NEWS.md` entries to a single brief sentence.
- Preserve the public API and return-type conventions (snake_case tibble outputs).
- Never hand-edit `man/*.Rd` or `R/sysdata.rda`; rebuild via `preprocess_data.R`.
- Respect the existing lean dependency set; justify any addition.
- Do not hardcode absolute paths.
