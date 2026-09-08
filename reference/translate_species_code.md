# Universal species translator across codes and names

Translate species identifiers across supported code systems and name
fields. The function can translate among NFI, CANFI, and
jurisdiction-specific species codes, and can also resolve scientific
names, English common names, and French common names to NFI codes, CANFI
codes, jurisdiction-specific codes, or supported name fields.

## Usage

``` r
translate_species_code(
  code,
  from = c("auto", "nfi", "canfi", "jurisdiction", "scientificname", "englishname",
    "frenchname"),
  jurisdiction = NULL,
  to = c("nfi", "canfi", "jurisdiction", "scientificname", "englishname", "frenchname"),
  multiple = c("error", "all", "first"),
  unmatched = c("error", "NA"),
  verbose = TRUE
)
```

## Arguments

- code:

  A character vector of species codes or names to translate.

- from:

  Source code system or name field: one of `"auto"`, `"nfi"`, `"canfi"`,
  `"jurisdiction"`, `"scientificname"`, `"englishname"`, or
  `"frenchname"`.

- jurisdiction:

  Jurisdiction for provincial or territorial codes. Required when
  `from = "jurisdiction"` and also when `to = "jurisdiction"` unless
  `from = "auto"` successfully infers a single shared jurisdiction. May
  be length 1 or the same length as `code`.

- to:

  Target field to return: one of `"nfi"`, `"canfi"`, `"jurisdiction"`,
  `"scientificname"`, `"englishname"`, or `"frenchname"`. Defaults to
  `"nfi"`.

- multiple:

  How to handle ambiguous matches: `"error"`, `"all"`, or `"first"`.

- unmatched:

  How to handle unmatched inputs: `"error"` or `"NA"`.

- verbose:

  Logical. If `TRUE` and `from = "auto"`, report the inferred input type
  once per function call. Defaults to `TRUE`.

## Value

A character vector when `multiple` is `"error"` or `"first"`. A list of
character vectors when `multiple = "all"`.

## Details

By default, translations return NFI codes, but the `to` argument can be
used to return CANFI codes, jurisdiction-specific codes, English common
names, French common names, or scientific names.

When `from = "auto"`, the function tries to infer the input type from
the supplied values. Numeric inputs are treated as CANFI codes,
NFI-formatted values are treated as NFI codes, and other code-like
inputs are treated as jurisdiction codes. For jurisdiction-style inputs,
the full input vector is used to infer a single shared jurisdiction when
possible. If the lookup does not support a unique interpretation, the
function errors and asks the user to supply `from` or `jurisdiction`
explicitly.

## Examples

``` r
# Translate NFI codes to English or French common names
translate_species_code("ABIE.BAL", from = "nfi", to = "englishname")
#> [1] "balsam fir"
translate_species_code("ABIE.BAL", from = "nfi", to = "frenchname")
#> [1] "sapin baumier"

# Translate NFI codes to CANFI or jurisdiction-specific codes
translate_species_code("ABIE.BAL", from = "nfi", to = "canfi")
#> [1] "302"
translate_species_code("ABIE.BAL", from = "nfi", to = "jurisdiction", jurisdiction = "ON")
#> [1] "BF"

# Translate from scientific, English, or French names
translate_species_code("Picea mariana", from = "scientificname")
#> [1] "PICE.MAR"
translate_species_code("black spruce", from = "englishname")
#> [1] "PICE.MAR"
translate_species_code("epinette noire", from = "frenchname")
#> [1] "PICE.MAR"

# Translate from jurisdiction, CANFI, or auto-detected inputs
translate_species_code("BF", from = "jurisdiction", jurisdiction = "ON")
#> [1] "ABIE.BAL"
translate_species_code("302", from = "canfi")
#> [1] "ABIE.BAL"
translate_species_code("302", from = "auto")
#> Auto-detected input type: canfi
#> [1] "ABIE.BAL"

# Translate between name fields
translate_species_code("black spruce", from = "englishname", to = "frenchname")
#> [1] "épinette noire"
translate_species_code("Picea mariana", from = "scientificname", to = "englishname")
#> [1] "black spruce"
translate_species_code("PICE.GLA", from = "auto", to = "scientificname")
#> Auto-detected input type: nfi
#> [1] "Picea glauca"

# Partial argument values also work via match.arg()
# Here, `to = "e"` resolves to `englishname`
translate_species_code(c("ABIE.BAL", "PICE.MAR", "PINU.CON"), to = "e")
#> Auto-detected input type: nfi
#> [1] "balsam fir"     "black spruce"   "lodgepole pine"

# Auto-detect a shared jurisdiction from a vector of input codes
translate_species_code(c("BF", "PJ", "SB"), from = "auto")
#> Auto-detected input type: jurisdiction (on)
#> [1] "ABIE.BAL" "PINU.BAN" "PICE.MAR"
translate_species_code(
  "SW",
  from = "jurisdiction",
  jurisdiction = "BC",
  to = "scientificname"
)
#> [1] "Picea glauca"

# Ambiguous CANFI code (several matches): errors by default
try(
  translate_species_code("104", from = "canfi")
)
#> Error in translate_species_code("104", from = "canfi") : 
#>   Ambiguous species code.
#> ✖ Species code `104` maps to multiple values.
#> ℹ Requested field: NFI_code
#> ℹ Matches: PICE.ENG, PICE.ENG.GLA, PICE.ENG.SIT

# Several matches - return the first match
translate_species_code("104", from = "canfi", multiple = "first")
#> [1] "PICE.ENG"

# Several matches - return all matches as a list
translate_species_code("104", from = "canfi", multiple = "all")
#> [[1]]
#> [1] "PICE.ENG"     "PICE.ENG.GLA" "PICE.ENG.SIT"
#> 

# Vectorized input with several matches
translate_species_code(
  c("302", "104"),
  from = "canfi",
  multiple = "first"
)
#> [1] "ABIE.BAL" "PICE.ENG"

translate_species_code(
  c("302", "104"),
  from = "canfi",
  multiple = "all"
)
#> [[1]]
#> [1] "ABIE.BAL"
#> 
#> [[2]]
#> [1] "PICE.ENG"     "PICE.ENG.GLA" "PICE.ENG.SIT"
#> 
```
