# Report format

Report opportunities grouped by dimension. Lead with high-impact / low-effort /
low-fidelity-risk items so the user sees the easy wins first.

## Per-opportunity block

```
[Dimension] Short title
  Pattern:   what repeats / is inconsistent / is dead.
  Evidence:  file:line sites (list all), or an inventory excerpt.
  Impact:    how much duplication/inconsistency this removes.
  Effort:    files/lines touched, test + snapshot churn.
  Fidelity risk:  None / Low / High — does a mistake risk changing model output?
  Recommendation: extract helper X / standardize on Y / move params to sysdata /
                  remove dead code Z.
```

## Structure

1. **Scope & inventory** — the audit surface and a compact map (families, param
   sources, validation/return patterns).
2. **Opportunities by dimension** — duplication, parameter storage, validation,
   return shape, dead code, naming/structure, dependencies. Omit empty dimensions.
3. **Prioritized shortlist** — the top few "do these first" items (high impact, low
   effort, low fidelity risk).
4. **Fidelity-sensitive proposals** — items that *could* change numerical output,
   listed separately and explicitly as decisions for the user, never as safe wins.

## Rules

- Every finding cites evidence; no impressions.
- Similarity is a candidate, not proof — say so where numeric logic is involved.
- No patches in the report; offer to implement selected items on request, one at a
  time, re-running tests and (for model code) benchmarks after each.
