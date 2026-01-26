# This script is a starting point of extracting model parameters from Kozak 1994 BC Report,
# and is kept here as a documentation for anyone who wishes to recreate the parameter dataset.
# The extracted data is about 75% complete and required some manual edits
# that were done in excel.

library(pdftools)
library(magick)
library(tesseract)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

pdf_file <- "volume_models/VolumeReferences/Kozak1994_BCReport.pdf"

# These are the scanned coefficient-table pages near the end.
# If your PDF viewer shows different numbering, adjust this vector.
table_pages <- 19:27

# Helper: parse a BEC line like "CWH 117 1.101792 969435 1.000429 ... "
parse_line <- function(species, line) {
  # normalize
  line2 <- line |>
    str_replace_all("[—–−]", "-") |>
    str_replace_all("[=~_“”]", " ") |>
    str_squish()

  # BEC zone + sample size
  m <- str_match(
    line2,
    "^([A-Z]{2,5})(?:\\s*\\(([A-Z]{2,5})\\))?\\s+([0-9Il]+)\\b"
  )
  if (is.na(m[1, 1])) {
    return(NULL)
  }

  zone <- m[1, 2]
  if (!is.na(m[1, 3])) {
    zone <- paste0(zone, "(", m[1, 3], ")")
  }

  n_sample <- m[1, 4] |>
    str_replace_all("[Il]", "1") |>
    as.integer()

  # all numeric tokens AFTER the zone
  nums <- str_extract_all(line2, "[-+]?\\d*\\.\\d+|[-+]?\\d+")[[1]]
  if (length(nums) == 0) {
    return(NULL)
  }

  # Drop first numeric token(s) which correspond to n_sample but may be split
  nums <- nums[-1]
  if (
    length(nums) >= 2 &&
      !str_detect(nums[1], "\\.") &&
      as.numeric(nums[1]) < 10 &&
      str_detect(nums[2], "\\.")
  ) {
    nums <- nums[-1]
  }

  vals <- suppressWarnings(as.numeric(nums[1:11]))
  if (length(vals) < 11) {
    vals <- c(vals, rep(NA_real_, 11 - length(vals)))
  }

  # Fix scan artifacts:
  # a0/a1/a2 often appear as 926472 meaning 0.926472
  for (i in 1:3) {
    if (
      !is.na(vals[i]) &&
        abs(vals[i]) >= 10 &&
        abs(vals[i]) < 2e6 &&
        vals[i] == floor(vals[i])
    ) {
      vals[i] <- vals[i] / 1e6
    }
  }
  # b3 sometimes appears as 718474 meaning 71.8474
  if (
    !is.na(vals[7]) &&
      abs(vals[7]) >= 1000 &&
      abs(vals[7]) < 1e7 &&
      vals[7] == floor(vals[7])
  ) {
    vals[7] <- vals[7] / 1e4
  }
  # b6 sometimes appears as 7771 meaning 0.0007771
  if (
    !is.na(vals[10]) &&
      abs(vals[10]) >= 1000 &&
      abs(vals[10]) < 1e7 &&
      vals[10] == floor(vals[10])
  ) {
    vals[10] <- vals[10] / 1e7
  }
  # log-bias factor should be ~1 and positive
  if (
    !is.na(vals[11]) &&
      vals[11] < 0 &&
      abs(vals[11]) >= 0.9 &&
      abs(vals[11]) <= 1.2
  ) {
    vals[11] <- abs(vals[11])
  }

  tibble(
    species = species,
    bec_zone = zone,
    n_sample = n_sample,
    a0 = vals[1],
    a1 = vals[2],
    a2 = vals[3],
    b0 = vals[4],
    b1 = vals[5],
    b2 = vals[6],
    b3 = vals[7],
    b4 = vals[8],
    b5 = vals[9],
    b6 = vals[10],
    log_bias_factor = vals[11]
  )
}

# OCR each page image, rotate, crop margins, then parse
eng <- tesseract("eng")

out <- map_dfr(table_pages, function(p) {
  img <- pdf_render_page(pdf_file, page = p - 1, dpi = 200) |> image_read()
  img <- image_rotate(img, -90) |>
    image_crop(geometry = "90%x90%+80+160") |>
    image_convert(colorspace = "gray") |>
    image_contrast(sharpen = 1)

  txt <- ocr(img, engine = eng)

  # Split into lines, track species headings like "Balsam - B", "Douglas-fir - F", etc.
  lines <- txt |> str_split("\n") |> unlist() |> str_squish()
  lines <- lines[lines != ""]

  species <- NA_character_
  rows <- list()

  for (ln in lines) {
    # Species heading: must contain " - " (not minus signs)
    if (
      str_detect(ln, " - ") &&
        !str_detect(ln, "^[A-Z]{2,5}(?:\\s*\\([A-Z]{2,5}\\))?\\s+[0-9]")
    ) {
      species <- str_split(ln, " - ", simplify = TRUE)[1]
      next
    }
    if (!is.na(species)) {
      r <- parse_line(species, ln)
      if (!is.null(r)) rows <- append(rows, list(r))
    }
  }

  bind_rows(rows) |> mutate(source_page = p)
})

# Optional: fix common OCR species-name glitches
out <- out |>
  mutate(
    species = str_squish(species),
    species = case_when(
      # Yellow cedar (Chamaecyparis nootkatensis)
      str_detect(
        species,
        regex("^llow\\s+Cedar$", ignore_case = TRUE)
      ) ~ "Yellow Cedar",
      # Other common Kozak94 OCR issues (safe to include)
      str_detect(species, regex("^3alsam$", ignore_case = TRUE)) ~ "Balsam",
      str_detect(species, regex("^sedar$", ignore_case = TRUE)) ~ "Cedar",
      str_detect(
        species,
        regex("^ted\\s+Alder$", ignore_case = TRUE)
      ) ~ "Red Alder",
      str_detect(
        species,
        regex("^Nhite\\s+Bark\\s+Pine$", ignore_case = TRUE)
      ) ~ "White Bark Pine",
      str_detect(
        species,
        regex("^odgepole\\s+Pine$", ignore_case = TRUE)
      ) ~ "Lodgepole Pine",

      TRUE ~ species
    )
  )

is_bec <- function(x) {
  str_detect(
    str_squish(x),
    "^[A-Z]{2,5}(\\s*\\([A-Z]{2,5}\\))?$"
  )
}

out <- out |>
  arrange(source_page) |>
  mutate(
    species = str_squish(species),
    species = if_else(is_bec(species), NA_character_, species)
  ) |>
  tidyr::fill(species, .direction = "down")


write_csv(out, "data-raw/kozak1994_parameters.csv") # open in e.g. excel and edit there
