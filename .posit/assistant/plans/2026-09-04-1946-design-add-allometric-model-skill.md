# Plan: `add-allometric-model` skill + templates

## Goal

Create a reusable, agent-loadable **skill** (plus supporting templates and a
`sources/` folder convention) that turns a source document (PDF/report describing
an allometric model) into a faithful, tested R function following this package's
established conventions.

The workflow is **staged**: the agent stops after extraction for human review of
parameters and equations *before* writing any R code.

## Decisions locked in (from discussion)

1. **Staged, not one-shot.** Agent extracts params + model spec + benchmark plan,
   then **pauses** for the user to verify numbers against the PDF before implementing.
2. **Two tiers of validation, never conflated:**
   - **Fidelity benchmark (exact):** worked examples / reference tables from the
     source publication → tight-tolerance numeric test. Preferred whenever the
     paper provides them.
   - **Plausibility check (approximate):** when the paper has no reference values,
     compare the new function against **already-implemented functions in the same
     family** (e.g. a new `si_*` vs. existing `si_*` for the same species/region).
     Expect *similar-but-not-identical* results. This is a guardrail against gross
     errors (unit/sign/transposition), **not** proof of correctness. Any model
     shipped with only a plausibility check must be **flagged** in NEWS and the
     model spec as "no source benchmark".
3. **Deliverable:** a `SKILL.md` skill + templates (function, test, model-spec,
   comparison-value generator, params-CSV convention doc).

## What I learned from the codebase (grounds the templates)

- **Function file pattern** (`R/<prefix>_<author><year>.R`): full roxygen block with
  `@param`, `@return` (tibble, snake_case cols), `@references`, `@examples`, `@export`;
  a thin exported wrapper delegating to an internal engine / `.<model>_*_one()` helpers;
  vectorization with length-recycling and explicit `cli::cli_abort` / `rlang::abort`
  on incompatible lengths; standardized species/jurisdiction codes via `utils.R` helpers.
  Reference examples: `R/vol_kozak88.R`, `R/si_thrower1994.R`.
- **Parameters** live as CSV/xlsx in `data-raw/`, are assembled by
  `data-raw/preprocess_data.R` (tribble/`read_csv` → transform → `usethis::use_data(..., internal = TRUE)`),
  and compiled into `R/sysdata.rda`. Rebuild requires **restart R** + `load_all()`.
  Example param files: `data-raw/Thrower1994_parameters.csv`, `Huang1994_parameters.csv`.
- **Registries** must be updated: `R/si_model_registry.R` (columns incl. `model_id`,
  `reference`, `engine`, `age_basis`, `fixed_args`, `species_manual`) and
  `R/volume_model_registry.R`.
- **Tests** live in `tests/testthat/test-<prefix>_<author><year>.R`, testthat 3e;
  cover normal cases, recycling, invalid inputs, error messages, output structure,
  and numeric benchmark comparison where available.
- **Comparison-value generators** live in `tmp/generate_<fn>_comparison_values.R`
  and write self-describing CSVs (`digits = 17`, per-row `status`/`error_message`).
  Reference: `tmp/generate_si_thrower1994_comparison_values.R`.
- **Bibliography**: `REFERENCES.bib` entry keyed like `@Thrower1994`; registry
  `reference` column points at it. **NEWS.md** updated per model.

## Skill directory layout (decisions locked in)

Location: **`.claude/skills/add-allometric-model/`** (project-scoped, portable
convention; folder name == skill name; `SKILL.md` carries YAML frontmatter).

Source PDFs: **gitignored** under `sources/`. Only the reviewed, derived artifacts
(`model-spec.md`, candidate params CSV) are committed.

PDF reading: agent reads PDFs **directly** by default. OCR / manual text conversion
is a documented **fallback only** for scanned image-only PDFs or garbled parameter
tables — not a routine step.

Templates: **family-specific**, treated as scaffolding to adapt, not fill-in molds.

```
.claude/skills/add-allometric-model/
  SKILL.md                          # the workflow the agent follows
  references/
    extraction-rules.md             # digit-for-digit copying, flag-don't-guess, units, OCR fallback
    feasibility-triage.md           # Stage 0.5 go/no-go gate before extraction
    params-csv-convention.md        # how data-raw CSVs map to sysdata + get_*_params
    validation-tiers.md             # fidelity vs plausibility, tolerances, flagging
    registry-and-wiring.md          # registry cols, preprocess_data, REFERENCES.bib, NEWS
  assets/
    model-spec-template.md          # extraction output the human reviews
    function-template-si.R          # invertible single-file SI skeleton (cf. si_thrower1994)
    function-template-vol.R         # engine + thin-wrapper skeleton (cf. vol_kozak88)
    function-template-generic.R     # fallback for agb/ytbh/new families
    test-template.R                 # testthat 3e skeleton
    comparison-generator-template.R
```

## The workflow SKILL.md will encode

**Stage 0 — Intake.** User drops the source doc in `sources/` and names the target
model (`prefix`, author, year, family: volume | si | agb | ytbh). Agent confirms
the function name and family.

**Stage 1 — Extraction (STOP for review).** Agent reads the document and produces,
*without writing R code*:
1. `sources/<model>/model-spec.md` — every equation (both directions if invertible),
   exact model form, variable definitions, **units**, base age / age basis, domain of
   applicability, species coverage, and any per-species notes.
2. A candidate params table in the **data-raw CSV convention** (not yet added to
   `preprocess_data.R`), coefficients copied **digit-for-digit** with a source
   locator (table/page) per value; illegible/ambiguous values **flagged, never guessed**.
3. A **benchmark plan**: list any worked examples / reference tables from the paper
   (→ fidelity tier). If none exist, state so explicitly and propose a plausibility
   comparison against named existing functions in the same family.

   → **Agent pauses here.** User verifies the CSV and spec against the PDF.

**Stage 2 — Implementation (after approval).**
4. Add params to `data-raw/` + wire into `preprocess_data.R`; rebuild `sysdata.rda`;
   restart R; `load_all()`.
5. Write `R/<fn>.R` from the function template (roxygen, vectorization, validation,
   tibble output); register in the appropriate `*_model_registry()`; add
   `REFERENCES.bib` entry.
6. Write `tmp/generate_<fn>_comparison_values.R` and the comparison CSV.
7. Write `tests/testthat/test-<fn>.R`: structural + input-validation tests **plus**
   - fidelity test vs. paper values (tight tolerance) when available, **or**
   - plausibility test vs. existing same-family functions (loose tolerance,
     documented as a sanity check, model flagged "no source benchmark").
8. `devtools::document()`; `devtools::test()`; targeted `devtools::check()` if warranted.
9. Update `NEWS.md`. Summarize what was implemented, which validation tier applied,
   and any flagged/uncertain parameters for the user's final review.

## Key guardrails baked into the skill

- Never paraphrase or "clean up" a coefficient; copy exactly, cite source location,
  flag anything uncertain rather than guessing.
- Never let a plausibility check be presented as proof of fidelity.
- Preserve public API / return-type conventions; snake_case tibble outputs.
- Never hand-edit `man/*.Rd` or `sysdata.rda`; rebuild via `preprocess_data.R`.
- Respect the existing dependency set; justify any addition.

## Resolved (previously open) questions

1. **Skill location** → `.claude/skills/add-allometric-model/`.
2. **`sources/` PDFs** → gitignored; commit only derived spec + params CSV.
3. **Extraction reader** → read PDFs directly; OCR only as a fallback.
4. **Templates** → family-specific (`si`, `vol`, generic), used as adaptable scaffolding.

## Out of scope for this task

- Actually implementing a specific new model (the skill is the deliverable).
- Restructuring existing models, registries, or `preprocess_data.R` beyond what the
  skill documents.
