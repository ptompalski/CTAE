# Validation tiers

Every new model must be validated, but there are **two distinct tiers** that serve
different purposes. Never let one masquerade as the other.

## Tier 1 — Fidelity benchmark (exact)

**Use whenever the source provides reference values.** Most allometry papers include
at least one of:

- a worked example ("for a tree of DBH = 30 cm, height = 22 m, volume = 0.68 m³"),
- a table of predicted values across a grid of inputs,
- a figure you can read points off (with a stated reading tolerance).

These become the gold-standard test: reproduce the paper's inputs and assert the
function's output matches the published value within a **tight tolerance**
(e.g. `tolerance = 1e-3`, or matching the source's reported significant figures).

Store the reference grid via a `tmp/generate_<fn>_comparison_values.R` script and a
committed CSV, following `tmp/generate_si_thrower1994_comparison_values.R`.

A passing fidelity test is genuine evidence the implementation matches the source.

## Tier 2 — Plausibility check (approximate)

**Use only when the source has no usable reference values.** Compare the new
function's output against **already-implemented functions in the same family** for
the same species/region:

- a new `si_*` model vs. existing `si_*` models for overlapping species,
- a new `vol_*` model vs. existing `vol_*` models for comparable trees.

Expect results that are **similar in magnitude and shape but not identical** — they
are different models. A reasonable assertion is agreement within a loose relative
tolerance (e.g. ±15–25%, chosen with judgment) and consistent monotonic behavior
(volume increases with DBH; height increases with age; etc.).

**What this tier does and does not tell you:**

- ✔ Catches gross errors: wrong units, a transposed digit, a sign flip, a misread
  coefficient — these usually produce outputs off by orders of magnitude or with the
  wrong shape.
- ✗ Does **not** prove the implementation is faithful to the source. Two wrong
  implementations can still agree to ±20%.

## Rules

1. **Prefer Tier 1.** Always search the source for worked examples / tables / figures
   before falling back to Tier 2.
2. **Label tests clearly.** Name and comment plausibility tests as sanity checks, not
   fidelity tests.
3. **Flag Tier-2-only models.** If a model ships with only a plausibility check, state
   "no source benchmark" in the model spec so future maintainers know its validation
   is weaker (keep the `NEWS.md` line itself to a single sentence).
4. **Report the tier** to the user when summarizing the completed implementation.
5. **Always cross-check against existing similar functions — even under Tier 1.**
   Independent of which tier you test with, compare the new model to the closest
   already-implemented same-family function(s) for overlapping species/regions and
   **report discrepancies** (magnitude, direction, where they diverge). A fidelity
   test proves faithfulness to the source; the cross-check catches cases where the
   source itself, or your reading of it, disagrees with the rest of the package. The
   two are complementary — do both.

## Coverage

Target **100% line coverage** of the new function and its internal helpers. Add a
test for every branch: each error/validation path, each species or model variant,
and both prediction directions (e.g. height→si and si→height). Verify with
`covr::file_coverage("R/<fn>.R", "tests/testthat/test-<fn>.R")`. Mark any genuinely
unreachable line `# nocov` with a one-line reason rather than leaving it uncovered.
