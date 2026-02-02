#' Calculate AGB proportion of different tree organs using multinomial logit
#' proportion equations using tree volume as input
#'
#' @description Calculates proportions of calculated aboveground biomass for
#' different tree organs using multinomial logit equations present in Boudewyn
#' et al (2007). It takes volume (or alternatively biomass) as main input but requires species, province
#' jurisdiction and ecozone for properly fitting the logit model. This routine
#' is the same as the one used in V2B() but returns only the proportions and not
#' the biomass values e.g. Pstemwood = 0.77, Pstembark = 0.099, Pbranches = 0.073,
#' Pfoliage = 0.058. Proportion values always add up to 1.
#' @param value_input Gross merchantable volume/ha (net in BC) or Above ground biomass (stem wood + stem bark + branches + foliage as per Boudewyn et al (2007)) in tonnes/ha of all live trees
#' @param value_type Specify whether using biomass or volume as input. Accepts partials 'vol' or 'bio'.
#' @param species Species code in the NFI standard (e.g. POPU.TRE)
#' @param jurisdiction A two-letter code depicting jurisdiction (e.g. "AB")
#' @param ecozone ecozone number (1-15). Call 'CodesEcozones' for a list of ecozones names and codes - also available in table 2 of appendix 7 of Boudewyn et al (2007).
#'
#' @return A list containing aboveground biomass proportion (0-1) for the tree
#' components: stemwood, stembark, branches, foliage. A warning is displayed if
#' the input volume is above (below) the maximum (minimum) volume used to
#' calibrate the equations parameters.
#' \describe{
#' \item{\code{Pstemwood}}{Biomass proportion allocated to the stem wood of merchantable-sized live trees (biomass includes stumps and tops).}
#' \item{\code{Pbark}}{Biomass proportion allocated to the bark of live, merchantable-sized live trees}
#' \item{\code{Pbranches}}{Biomass proportion allocated to the branches of live, merchantable-sized live trees}
#' \item{\code{Pfoliage}}{Biomass proportion allocated to the foliage of live, merchantable-sized live trees}
#' }
#'
#' @references
#' Boudewyn et al (2007). Model Based Volume-to-biomass Conversion for Forested and Vegetated Land in Canada. In Forestry. Pacific Forestry Centre. http://sbisrvntweb.uqac.ca/archivage/030078750.pdf
#'
#' @examples
#' # Using a single value (volume)
#' AGB_prop(350, value_type = "vol", species = "PINU.CON",jurisdiction = "BC", ecozone=4)
#'
#' # Using a data frame and volume as example
#' tb <- tibble::tibble(VOL = rnorm(150, 250, 60),
#'              TYP = rep("vol"),
#'              SPE = rep("PINU.CON", 150),
#'              JUR = rep("BC", 150),
#'              ECO = rep(4, 150))
#'
#' tb |>
#' dplyr::mutate(
#'   purrr::pmap_dfr(list(VOL, TYP, SPE, JUR, ECO), AGB_prop)
#'   )
#'
#' @export
AGB_prop <- function(value_input, value_type, species, jurisdiction, ecozone) {
  # Checking input
  if (!is.numeric(value_input)) {
    stop("'value_input' must be type numeric")
  }
  if (!value_type %in% c("biomass", "volume", "bio", "vol")) {
    stop(
      "Must specify what is the type of input. Either 'biomass' or 'volume'."
    )
  }
  if (!is.character(species)) {
    stop("'species' must be type character")
  }
  if (!is.character(jurisdiction)) {
    stop("'jurisdiction' must be type character")
  }
  if (!is.numeric(ecozone)) {
    stop("'ecozone' must be type numeric")
  }

  # Split species into genus and species. Add a check.
  species <- stringr::str_split(species, "\\.")[[1]]
  if (length(species) >= 2) {
    genus <- species[1]
    spp <- species[2]
    variety <- ifelse(is.na(species[3]), NA, species[3])
  } else {
    stop("Wrong species format")
  }
  # Get paramaters
  B <- V2Bgetparams(
    genus = genus,
    species = spp,
    variety = variety,
    jurisdiction = jurisdiction,
    ecozone = ecozone
  )
  # table 6 with parameters, table 7 with model range and respective caps to be applied.

  if (value_type %in% c("biomass", "bio")) {
    B6 <- B$B6bio
    B7 <- B$B7bio
    value_max <- B7$biom_max
    value_min <- B7$biom_min
  } else if (value_type %in% c("volume", "vol")) {
    B6 <- B$B6vol
    B7 <- B$B7vol
    value_max <- B7$vol_max
    value_min <- B7$vol_min
  } else {
    stop("Something wrong with value_type specified.")
  }

  # Apply equations
  lvalue <- log(value_input + 5)
  p_a <- exp(B6$a1 + B6$a2 * value_input + B6$a3 * lvalue)
  p_b <- exp(B6$b1 + B6$b2 * value_input + B6$b3 * lvalue)
  p_c <- exp(B6$c1 + B6$c2 * value_input + B6$c3 * lvalue)
  p_abc <- 1 + p_a + p_b + p_c

  # Check whether volume is within modelled range. If not apply cap and warn.
  cap_check <- ifelse(
    (value_input > value_min & value_input < value_max),
    "good",
    ifelse(
      value_input < value_min,
      "below",
      "above"
    )
  )
  if (cap_check == "below") {
    Pstemwood = B7$p_sw_low
    Pbark = B7$p_sb_low
    Pbranches = B7$p_br_low
    Pfoliage = B7$p_fl_low
  } else if (cap_check == "above") {
    Pstemwood = B7$p_sw_high
    Pbark = B7$p_sb_high
    Pbranches = B7$p_br_high
    Pfoliage = B7$p_fl_high
  } else {
    Pstemwood = 1 / p_abc
    Pbark = p_a / p_abc
    Pbranches = p_b / p_abc
    Pfoliage = p_c / p_abc
  }
  p <- list(
    Pstemwood = Pstemwood,
    Pbark = Pbark,
    Pbranches = Pbranches,
    Pfoliage = Pfoliage
  )
  if (cap_check %in% c("below", "above")) {
    warning(paste0(
      "\n",
      stringr::str_to_title(value_type),
      " outside model range. \n",
      stringr::str_to_title(value_type),
      "value: ",
      value_input,
      ".",
      "\nModel range: ",
      round(value_min, 2),
      " - ",
      round(value_max, 2),
      "\nProportion was capped following publication instructions."
    ))
  }
  return(p)
}
