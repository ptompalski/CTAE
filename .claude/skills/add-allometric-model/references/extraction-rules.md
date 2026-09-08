# Extraction rules

The extraction stage is where scientific fidelity is won or lost. Follow these
rules strictly.

## Reading the source

The `read` tool **cannot open PDFs** in this environment (they come back as
binary). Use the pipeline below. Available tooling: `pdftotext` (CLI),
`pdftools`, `magick`, and the R `tesseract` package. There is **no system
`tesseract`** — OCR must go through the R package.

### Prose (equations, units, scope, species names)

- Extract with `pdftotext -layout <pdf> <out.txt>` or `pdftools::pdf_text()`.
- This is fine for reading the narrative, model form, and definitions.

### Parameter tables — render to image, do NOT trust the text layer

A healthy-looking text layer does **not** mean the tables extracted correctly.
Validated example (Payandeh 1974): `pdftotext` returned 291 lines of plausible
text, yet the coefficient tables were badly mangled — species labels merged
(`"WRehditsepsrpucriecbe"`), rows shifted against their coefficients, and one
whole table's numbers missing. An agent trusting that text would transcribe
**misaligned coefficients while believing extraction succeeded.**

So for every parameter table:

1. Render the page(s) to an image at ~300 dpi:
   `pdftools::pdf_render_page(pdf, page = p, dpi = 300)` (or
   `magick::image_read_pdf(pdf, pages = p, density = 300)`), optionally cropping
   to the table with `magick::image_crop()`.
2. Read the **rendered image** to transcribe coefficients. The image is reliable
   even when the text layer is corrupt.
3. Save crops used for transcription under `tmp/` as evidence.

Note: ImageMagick may warn `No display font for 'Symbol'/'ArialUnicode'` — this is
harmless for numeric tables but flag it on pages with heavy math notation.

### Scanned / image-only PDFs

If `pdftotext` returns almost nothing (e.g. only a watermark repeated per page —
in the validated case, ~37 chars/page), the PDF is scanned. Render pages to images
and OCR with the R `tesseract` package (`tesseract::ocr()` on a rendered PNG).

Use OCR as a **locator and prose reader**, not as the source of truth for numbers.
Validated on a scanned CJFR paper: OCR recovered the narrative and the *structure*
of equations well, but reliably corrupted the things that matter for fidelity:

- Equation sub/superscripts and Greek symbols came through wrong (`c₃`→`¢,`,
  `b₃`→`bs?`); the `No display font for 'Symbol'` warning fires on these pages.
- Numeric tables had misread digits (letter `l`/`I` for `1`, `]` for a digit),
  merged/dropped cells, and shifted columns (`−0.62` read as `−9.62`).

Therefore: after OCR, **transcribe every coefficient from the rendered image**
(zoom/crop as needed), or have the human do it. Never ship a coefficient read only
from OCR text.

Also **verify the citation from the rendered text, not the filename** — a source
file's name can be wrong (validated: a file named "alemdag-2011…" was actually
Alemdag, CJFR Vol. 21, 1991).

State clearly which pages/tables required rendering or OCR.

## Self-check before presenting to the human

After transcribing a parameter table, **spot-check at least two coefficients per
table against the rendered image** (pick different rows/columns). Confirm row-to-
species alignment explicitly — misalignment, not misreading a single digit, is the
dominant failure mode here.

## Copying coefficients

- Copy every numeric coefficient **digit-for-digit**, including sign, exponent,
  and all significant figures shown. Do not round, reformat, or "tidy" values.
- Preserve the source's precision exactly. If a value is given as `-3.0492e-4`,
  keep it as `-3.0492e-4` (or `-0.00030492`), not `-0.0003`.
- Record a **source locator** for every value: table number, row/column, and page.
  Put this in the model spec and, where practical, as a comment/column in the CSV.

## When a value is uncertain

- If a digit is illegible, a table cell is ambiguous, or a symbol is unclear:
  **flag it, do not guess.** Mark it in the spec (e.g. `⚠ TABLE 3, row 4: last
  digit illegible — 0.0451? or 0.0457?`) and leave the CSV cell empty or clearly
  marked so the human review catches it.
- Never infer a coefficient from a fitted curve or figure unless the source
  explicitly provides it as a value and you note that provenance.

## Equations and structure

- Transcribe the model form exactly, including which variables are inputs vs.
  outputs and the direction(s) the model supports (e.g. predict height from
  age+SI, and/or invert to predict SI from age+height).
- Record **units** for every variable (cm vs mm for diameter, m vs cm for height,
  years, m³, etc.). Unit mismatches are a common source of silent error.
- Record the **age basis** (total age vs breast-height age) and **base age** for
  SI models.
- Record the **domain of applicability**: species covered, region/jurisdiction,
  valid DBH/height/age ranges, and any stated caveats.

## Species codes

- Map the source's species names to NFI codes (`GENUS.SPEC`, e.g. `PICE.MAR`).
  Use `translate_species_code()` conventions; record the mapping in the spec so
  the human can verify it. Flag any species you could not confidently map.
