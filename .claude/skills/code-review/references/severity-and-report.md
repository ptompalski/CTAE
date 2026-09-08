# Severity levels and report format

## Severity

| Severity | Use for |
|----------|---------|
| **Critical** | Wrong numerical results; fidelity violation (bad coefficient/unit/sign, wrong bark or merch basis); public API or return-type break; hand-edited `man/*.Rd` or `R/sysdata.rda`. |
| **Major** | Missing input validation or error paths; broken/absent vectorization or recycling; model not registered, or registry key mismatched to `REFERENCES.bib`; expected benchmark test absent. |
| **Minor** | Naming-convention drift; incomplete roxygen; missing `NEWS.md` or `REFERENCES.bib` entry; thin test coverage of new branches. |
| **Nit** | Style, comments, non-native pipe, wording, formatting. |

When unsure between two levels, pick the higher one and say why. Anything touching a
number a user would consume, or a published signature, is at least Major.

## Finding format

One line per finding:

```
path/to/file.R:123 — [Critical] Coefficient b2 hardcoded as 1.204 instead of read
   from sysdata; source Table 3 gives 1.240 → pull from get_volume_params().
```

- Always include `file:line`.
- State the problem, then a concrete suggested fix after `→`.
- Cite evidence (source table, test output, benchmark diff) when available.

## Report structure

1. **Scope** — files reviewed and how they were selected (named / working tree / vs main).
2. **Verification run** — what was executed (tests, coverage, benchmark) and the result.
3. **Findings by severity** — Critical → Major → Minor → Nit, each as above.
   Omit empty sections.
4. **Summary** — counts per severity, then an explicit **Must fix** vs. **Consider**
   split so the user knows the minimum bar to merge.

Keep the report scannable. Do not include patches; offer to produce them on request.
