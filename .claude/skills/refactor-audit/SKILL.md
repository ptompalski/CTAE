---
name: refactor-audit
description: >-
  Audit the whole CanadaForestAllometry package (not a diff) for duplication and
  code-quality issues — repeated logic across model families that could become shared
  internals, inconsistent parameter storage (sysdata/CSV vs hardcoded), divergent
  validation or return-shape patterns, dead code, and convention drift. Use when the
  user asks for a codebase audit, refactoring opportunities, technical-debt scan, or
  consistency review across si_/vol_/agb_/ytbh_ functions. Reports opportunities only;
  refactors on request. Preserves scientific fidelity and the public API.
---

# Refactor audit for CanadaForestAllometry

This is a **whole-package**, non-diff review that surfaces **duplication** and
**code-quality / consistency** opportunities across the codebase. It is the
package-wide counterpart to the diff-scoped `code-review` skill: use `code-review`
for a change set, use this to audit the package as a whole.

Read `AGENTS.md` at the repo root first for naming, API, and data-flow conventions.

Two hard constraints frame every recommendation:

- **Scientific fidelity is inviolable.** Consolidating duplicated logic must not
  change any model's numerical output. Two functions that *look* similar may encode
  genuinely different published equations — near-duplication is a *candidate*, never
  a proof. Flag, explain, and let the user decide; never merge on suspicion.
- **The public API is stable.** Prefer refactors that extract shared **internal**
  helpers behind unchanged public signatures and return types.

**This skill reports opportunities only.** It does not refactor unless the user asks.

## Overview

```
Stage 0  Scope       agree the audit surface (default: all of R/ + data-raw/)
Stage 1  Inventory   map functions by family, params source, validation, return shape
Stage 2  Scan        run the audit dimensions in references/audit-dimensions.md
Stage 3  Assess      rank findings by impact x effort x fidelity risk
Stage 4  Report      grouped opportunities, evidence, and a recommendation each
```

## Stage 0 — Scope

Default surface is `R/` plus `data-raw/`. Confirm with the user if they want a
narrower focus (e.g. "just the si_ family") or a single dimension (e.g. only
parameter-storage consistency). A focused audit is usually more actionable than an
everything-at-once sweep — offer that.

## Stage 1 — Inventory

Before judging anything, build a factual map (use `search`/`grep`, and R
introspection where useful):

- **Function inventory** by family (`vol_`/`si_`/`agb_`/`ytbh_`/`get_`/helpers).
- **Parameter source** per model: read from `sysdata`/CSV, or hardcoded literals?
- **Validation pattern**: which `assert_*` / cli / rlang idioms each function uses.
- **Return shape**: tibble column names/types per family.
- **Internal helpers**: `.one`/dot-prefixed helpers and where each is (re)used.

Present the inventory (or a compact summary) before opinions — findings must rest on
it, not on impressions.

## Stage 2 — Scan

Work through the dimensions in `references/audit-dimensions.md`: duplication,
parameter-storage consistency, validation/error consistency, return-shape
consistency, dead/unused code, naming/structure drift, and dependency hygiene.

For **duplication**, distinguish *structural* similarity (same control flow / helper
opportunity) from *numerical* identity. Only structural duplication behind an
unchanged API is a safe consolidation candidate; anything touching coefficients or
equation form is a fidelity question for the user.

## Stage 3 — Assess

Rank each opportunity on three axes and state them:

- **Impact** — how much duplication/inconsistency it removes.
- **Effort** — lines/files touched, test churn, snapshot updates.
- **Fidelity risk** — could a mistake change a model's output? High-risk items are
  *proposals to discuss*, not recommendations to act on.

Lead with high-impact / low-effort / low-risk items.

## Stage 4 — Report

Group by dimension. For each opportunity give: the pattern, the **evidence**
(files:lines, an inventory excerpt), the impact/effort/fidelity-risk read, and a
concrete recommendation (extract helper X / standardize on pattern Y / move params to
sysdata / remove dead code). See `references/report-format.md`.

Report only. Offer to implement selected items on request — one at a time, verifying
tests and (for model code) benchmarks after each.

## Guardrails

- Fidelity first: never propose merging logic that could alter numerical output
  without flagging it explicitly as a fidelity decision for the user.
- Preserve public signatures and return types; refactor behind internal helpers.
- Similarity is a candidate, not proof — the user decides what actually merges.
- Report only; implement on request, incrementally, re-running tests/benchmarks.
- Do not hand-edit `man/*.Rd` or `R/sysdata.rda`; rebuild via `preprocess_data.R`.
- Respect the lean dependency set; a refactor should not add dependencies.
