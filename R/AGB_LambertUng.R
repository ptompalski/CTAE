# internal helper: validate lengths (recycling) and basic numeric checks
.lu_validate_inputs <- function(DBH, height = NULL) {
  if (!is.numeric(DBH)) {
    rlang::abort("`DBH` must be numeric.")
  }
  if (any(is.na(DBH))) {
    rlang::abort("`DBH` contains NA values.")
  }
  if (any(DBH <= 0)) {
    rlang::abort("`DBH` must be > 0.")
  }

  if (!is.null(height)) {
    if (!is.numeric(height)) {
      rlang::abort("`height` must be numeric.")
    }
    if (any(is.na(height))) {
      rlang::abort("`height` contains NA values.")
    }
    if (any(height <= 0)) rlang::abort("`height` must be > 0.")
  }

  # recycling sanity check (base R will recycle silently otherwise)
  if (!is.null(height)) {
    n1 <- length(DBH)
    n2 <- length(height)
    if (!(n1 == n2 || n1 == 1L || n2 == 1L)) {
      rlang::abort(
        "`DBH` and `height` must have the same length, or one must be length 1."
      )
    }
  }

  invisible(TRUE)
}

# internal helper: normalize + validate species, and fetch coefficients for a model
.lu_get_coefs <- function(params, model, species) {
  if (length(species) != 1L) {
    rlang::abort("`species` must be a single species code (length 1).")
  }

  if (is.na(species)) {
    rlang::warn("No species provided. Using coefficients for `UNKN.SPP`.")
    species <- "UNKN.SPP"
  }

  # normalize input
  species_in <- stringr::str_trim(toupper(species))
  model_in <- stringr::str_trim(model)

  # normalize table keys (protects against trailing spaces in the data)
  params2 <- params %>%
    dplyr::mutate(
      model = stringr::str_trim(.data$model),
      species = stringr::str_trim(toupper(.data$species)),
      parameter = stringr::str_trim(.data$parameter)
    )

  params_m <- dplyr::filter(params2, .data$model == model_in)

  ok_species <- unique(params_m$species)
  if (!species_in %in% ok_species) {
    rlang::abort(paste0(
      "Unknown species: `",
      species_in,
      "` for model `",
      model_in,
      "`."
    ))
  }

  B <- dplyr::filter(params_m, .data$species == species_in)

  # sanity: required parameters
  required <- if (model_in == "DBH") {
    c(
      "bwood1",
      "bwood2",
      "bbark1",
      "bbark2",
      "bfoliage1",
      "bfoliage2",
      "bbranches1",
      "bbranches2"
    )
  } else if (model_in == "DBHHT") {
    c(
      "bwood1",
      "bwood2",
      "bwood3",
      "bbark1",
      "bbark2",
      "bbark3",
      "bfoliage1",
      "bfoliage2",
      "bfoliage3",
      "bbranches1",
      "bbranches2",
      "bbranches3"
    )
  } else {
    rlang::abort("Unknown model (expected `DBH` or `DBHHT`).")
  }

  missing <- setdiff(required, B$parameter)
  if (length(missing) > 0) {
    rlang::abort(paste0(
      "Coefficient table is missing required parameters for model `",
      model_in,
      "` and species `",
      species_in,
      "`: ",
      paste(missing, collapse = ", ")
    ))
  }

  stats::setNames(B$estimate, B$parameter)
}


# internal helper: compute components given coefs and predictors
.lu_agb_components_dbh <- function(DBH, coefs) {
  with(as.list(coefs), {
    Ywood <- bwood1 * DBH^bwood2
    Ybark <- bbark1 * DBH^bbark2
    Yfoliage <- bfoliage1 * DBH^bfoliage2
    Ybranches <- bbranches1 * DBH^bbranches2

    tibble::tibble(
      Bwood = Ywood,
      Bbark = Ybark,
      Bstem = Ywood + Ybark,
      Bfoliage = Yfoliage,
      Bbranches = Ybranches,
      Bcrown = Yfoliage + Ybranches,
      Btotal = Ywood + Ybark + Yfoliage + Ybranches
    )
  })
}

.lu_agb_components_dbhht <- function(DBH, height, coefs) {
  with(as.list(coefs), {
    Ywood <- bwood1 * DBH^bwood2 * height^bwood3
    Ybark <- bbark1 * DBH^bbark2 * height^bbark3
    Yfoliage <- bfoliage1 * DBH^bfoliage2 * height^bfoliage3
    Ybranches <- bbranches1 * DBH^bbranches2 * height^bbranches3

    tibble::tibble(
      Bwood = Ywood,
      Bbark = Ybark,
      Bstem = Ywood + Ybark,
      Bfoliage = Yfoliage,
      Bbranches = Ybranches,
      Bcrown = Yfoliage + Ybranches,
      Btotal = Ywood + Ybark + Yfoliage + Ybranches
    )
  })
}

#' Calculate tree-level AGB using Canadian national tree aboveground biomass equations using tree DBH as input
#'
#' @description Calculates tree-level aboveground biomass [kg] using methods presented in Lambert et al. (2005) and Ung et al. (2008).
#' @param DBH Tree DBH
#' @param species Tree species code in the NFI standard (e.g. POPU.TRE)
#' @return A list containing aboveground biomass values for tree components: wood, bark, stem, foliage, branches, crown,
#' as well as the total tree aboveground biomass.
#'
#' @references
#' Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree aboveground biomass equations. Canadian Journal of Forest Research, 35(8), 1996–2018. https://doi.org/10.1139/x05-112
#'
#' Ung, C.-H., Bernier, P., & Guo, X.-J. (2008). Canadian national biomass equations: new parameter estimates that include British Columbia data. Canadian Journal of Forest Research, 38(5), 1123–1132. https://doi.org/10.1139/X07-224
#'
#' @examples
#'
#' AGB_LambertUngDBH(20)
#' AGB_LambertUngDBH(20, species="PINU.CON")
#' AGB_LambertUngDBH(10:30, species="PINU.CON")
#'
#' @export
AGB_LambertUngDBH <- function(DBH, species = NA) {
  .lu_validate_inputs(DBH)

  coefs <- .lu_get_coefs(
    params = parameters_LambertUng,
    model = "DBH",
    species = species
  )

  .lu_agb_components_dbh(DBH = DBH, coefs = coefs)
}

#' Calculate tree-level AGB using Canadian national tree aboveground biomass equations using tree DBH and height as input
#'
#' @description Calculates tree-level aboveground biomass [kg] using methods presented in Lambert et al (2005) and Ung et al (2008).
#' @inheritParams AGB_LambertUngDBH
#' @param height Tree height
#' @return A list containing aboveground biomass values for tree components: wood, bark, stem, foliage, branches, crown,
#' as well as the total tree aboveground biomass.
#'
#' @references
#' Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree aboveground biomass equations. Canadian Journal of Forest Research, 35(8), 1996–2018. https://doi.org/10.1139/x05-112
#'
#' Ung, C.-H., Bernier, P., & Guo, X.-J. (2008). Canadian national biomass equations: new parameter estimates that include British Columbia data. Canadian Journal of Forest Research, 38(5), 1123–1132. https://doi.org/10.1139/X07-224
#'
#'
#' @examples
#'
#' AGB_LambertUngDBHHT(20, 17)
#' AGB_LambertUngDBHHT(20, 17, species="PINU.CON")
#' AGB_LambertUngDBHHT(DBH = runif(5, 10, 30), height=runif(5, 10, 30), species="PINU.CON")
#'
#' @export
AGB_LambertUngDBHHT <- function(DBH, height, species = NA) {
  .lu_validate_inputs(DBH, height)

  coefs <- .lu_get_coefs(
    params = parameters_LambertUng,
    model = "DBHHT",
    species = species
  )

  .lu_agb_components_dbhht(DBH = DBH, height = height, coefs = coefs)
}

# #' Calculate tree-level AGB using Canadian national tree aboveground biomass equations using tree DBH as input
# #'
# #' @description Calculates tree-level aboveground biomass [kg] using methods presented in Lambert et al. (2005) and Ung et al. (2008).
# #' @param DBH Tree DBH
# #' @param species Tree species code in the NFI standard (e.g. POPU.TRE)
# #' @return A list containing aboveground biomass values for tree components: wood, bark, stem, foliage, branches, crown,
# #' as well as the total tree aboveground biomass.
# #'
# #' @references
# #' Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree aboveground biomass equations. Canadian Journal of Forest Research, 35(8), 1996–2018. https://doi.org/10.1139/x05-112
# #'
# #' Ung, C.-H., Bernier, P., & Guo, X.-J. (2008). Canadian national biomass equations: new parameter estimates that include British Columbia data. Canadian Journal of Forest Research, 38(5), 1123–1132. https://doi.org/10.1139/X07-224
# #'
# #' @examples
# #'
# #' AGB_LambertUngDBH(20)
# #' AGB_LambertUngDBH(20, species="PINU.CON")
# #' AGB_LambertUngDBH(10:30, species="PINU.CON")
# #'
# #' @export

# AGB_LambertUngDBH <- function(DBH, species=NA){

#   params <- parameters_LambertUng |> dplyr::filter(model == "DBH")

#   if (is.na(species)) {
#     warning("No species provided. Using coefficients for all species.")
#     species <- "UNKN.SPP"
#   }

#   #species to capital letters
#   species <- toupper(species)

#   #check if species is included in the coefficients table
#   if (!(species %in% unique(params$species))) {
#     stop("Wrong species")
#   }

#   #subset parameters for the current species
#   B <- params[params$species==species,]

#   #assign values to coefficients
#   for (i in 1:nrow(B)) {
#     assign(x = B$parameter[i], B$estimate[i])
#   }

#   Ywood     <- bwood1 * DBH ^ bwood2
#   Ybark     <- bbark1 * DBH ^ bbark2
#   Ystem     <- Ywood + Ybark
#   Yfoliage  <- bfoliage1 * DBH ^ bfoliage2
#   Ybranches <- bbranches1 * DBH ^ bbranches2
#   Ycrown    <- Yfoliage + Ybranches
#   Ytotal    <- Ywood + Ybark + Yfoliage + Ybranches

#   R <- list(
#     Bwood = Ywood,
#     Bbark = Ybark,
#     Bstem = Ystem,
#     Bfoliage = Yfoliage,
#     Bbranches = Ybranches,
#     Bcrown = Ycrown,
#     Btotal = Ytotal
#   )

#   # R <- R |> as_tibble()

#   return(R)
# }

# #' Calculate tree-level AGB using Canadian national tree aboveground biomass equations using tree DBH and height as input
# #'
# #' @description Calculates tree-level aboveground biomass [kg] using methods presented in Lambert et al (2005) and Ung et al (2008).
# #' @inheritParams AGB_LambertUngDBH
# #' @param height Tree height
# #' @return A list containing aboveground biomass values for tree components: wood, bark, stem, foliage, branches, crown,
# #' as well as the total tree aboveground biomass.
# #'
# #' @references
# #' Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree aboveground biomass equations. Canadian Journal of Forest Research, 35(8), 1996–2018. https://doi.org/10.1139/x05-112
# #'
# #' Ung, C.-H., Bernier, P., & Guo, X.-J. (2008). Canadian national biomass equations: new parameter estimates that include British Columbia data. Canadian Journal of Forest Research, 38(5), 1123–1132. https://doi.org/10.1139/X07-224
# #'
# #'
# #' @examples
# #'
# #' AGB_LambertUngDBHHT(20, 17)
# #' AGB_LambertUngDBHHT(20, 17, species="PINU.CON")
# #' AGB_LambertUngDBHHT(DBH = runif(5, 10, 30), height=runif(5, 10, 30), species="PINU.CON")
# #'
# #' @export

# AGB_LambertUngDBHHT <- function(DBH, height, species=NA){

#   params <- parameters_LambertUng |> dplyr::filter(model == "DBHHT")

#   if (is.na(species)) {
#     warning("No species provided. Using coefficients for all species.")
#     species <- "UNKN.SPP"
#   }

#   #species to capital letters
#   species <- toupper(species)

#   #check if species is included in the coefficients table
#   if (!(species %in% unique(params$species))) {
#     stop("Wrong species")
#   }

#   B <- params[params$species==species,]

#   #assign values to coefficients
#   for (i in 1:nrow(B)) {
#     assign(x = B$parameter[i], B$estimate[i])
#   }

#   Ywood     <- bwood1 * DBH ^ bwood2 * height ^ bwood3
#   Ybark     <- bbark1 * DBH ^ bbark2 * height ^ bbark3
#   Ystem     <- Ywood + Ybark
#   Yfoliage  <- bfoliage1 * DBH ^ bfoliage2 * height ^ bfoliage3
#   Ybranches <- bbranches1 * DBH ^ bbranches2 * height ^ bbranches3
#   Ycrown    <- Yfoliage + Ybranches
#   Ytotal    <- Ywood + Ybark + Yfoliage + Ybranches

#   R <- list(
#     Bwood = Ywood,
#     Bbark = Ybark,
#     Bstem = Ystem,
#     Bfoliage = Yfoliage,
#     Bbranches = Ybranches,
#     Bcrown = Ycrown,
#     Btotal = Ytotal
#   )

#   # R <- R |> as_tibble()

#   return(R)

# }
