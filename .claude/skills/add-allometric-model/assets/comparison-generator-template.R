# Generate comparison values for <fn_name>.
#
# Writes a self-describing CSV of model outputs over a grid of inputs. Used as a
# reference for tests and for cross-language / cross-implementation checks.
# Reference to adapt: tmp/generate_si_thrower1994_comparison_values.R

library(CanadaForestAllometry)
library(dplyr)
library(tibble)

# ---- settings ----
fn_name    <- "<fn_name>"
output_csv <- file.path("tmp", "<fn_name>_comparison_values.csv")

# Input grid (edit ranges to the model's domain). Pull species coverage from the
# relevant registry (si_model_registry() / volume_model_registry()) where possible.
input_grid <- expand.grid(
  species = c("<PICE.MAR>", "<...>"),
  # e.g. age = c(10, 20, 30, 50, 80), si = c(8, 12, 16, 20),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) |>
  mutate(row_id = row_number(), .before = 1)

# ---- run, one row at a time (record errors rather than stopping) ----
fn <- get(fn_name, envir = asNamespace("CanadaForestAllometry"))

run_one <- function(i) {
  args <- list(<build args from input_grid[i, ]>)
  tryCatch(
    do.call(fn, args) |> mutate(status = "ok", error_message = NA_character_),
    error = function(e) {
      tibble(status = "error", error_message = conditionMessage(e))
    }
  )
}

out <- lapply(seq_len(nrow(input_grid)), run_one) |> bind_rows()
comparison_values <- bind_cols(input_grid, out)

# ---- write with high precision ----
dir.create(dirname(output_csv), showWarnings = FALSE, recursive = TRUE)
old <- getOption("digits"); options(digits = 17)
utils::write.csv(comparison_values, output_csv, row.names = FALSE, na = "",
                 quote = TRUE, fileEncoding = "UTF-8")
options(digits = old)

cat("Wrote", nrow(comparison_values), "rows to", output_csv, "\n")
