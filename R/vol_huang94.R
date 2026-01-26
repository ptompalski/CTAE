#' Calculate individual tree volume for major Alberta tree species
#'
#' @description Calculates tree-level merchantable and total volume using methods presented in Huang (1994).
#' @param DBH Tree DBH
#' @param height Tree height
#' @param species Tree species code in the NFI standard (e.g. POPU.TRE)
#' @param subregion code depicting natural subregion of Alberta (e.g. "LH" for Lower Foothills). Please see
#' \code{\link{AlbertaNaturalRegSubreg}} for a complete list of Alberta natural regions and subregions.
#' 'Province' (the default) indicates province-level parameters.
#'
#' @return A tibble with volumes (m^3): total and merchantable.
#'
#' @references
#' Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation for Major Alberta Tree Species. Report 1 - Individual tree volume estimation procedures for Alberta: Methods of Formulation and Statistical Foundations. Alberta Environmental Protection, Land and Forest Service, Forest Management Division, Edmonton, AB.

#' @examples
#' vol_huang94(20, 20, "PICE.GLA")
#' vol_huang94(20, 20, "PICE.GLA", subregion = "CP")
#'
#' @export

vol_huang94 <- function(DBH, height, species, subregion = "Province") {
  # ---- recycle scalars (common in usage) ----
  n <- max(length(DBH), length(height), length(species), length(subregion))
  if (
    length(DBH) %in%
      c(1L, n) &&
      length(height) %in% c(1L, n) &&
      length(species) %in% c(1L, n) &&
      length(subregion) %in% c(1L, n)
  ) {
    DBH <- rep(DBH, length.out = n)
    height <- rep(height, length.out = n)
    species <- rep(species, length.out = n)
    subregion <- rep(subregion, length.out = n)
  } else {
    rlang::abort(
      "DBH, height, species, and subregion must have compatible lengths (each length 1 or the same length)."
    )
  }

  species_std <- standardize_species_code(species)
  subr_std <- stringr::str_squish(stringr::str_to_upper(subregion))

  # Huang 1994 note: UB is treated as LBH in your legacy code
  subr_std[subr_std == "UB"] <- "LBH"

  # ---- constants from Huang 1994 SAS logic ----
  cons <- 0.00007854
  per <- (1.0 - sqrt(0.225)) # same constant used in your code

  # ---- internal: structured abort with context ----
  abort_i <- function(i, msg) {
    rlang::abort(paste0(
      "vol_huang94() failed for row ",
      i,
      " (species=",
      species_std[i],
      ", subregion=",
      subr_std[i],
      ", DBH_cm=",
      DBH[i],
      ", ht_m=",
      height[i],
      "): ",
      msg
    ))
  }

  # ---- merchantability criteria (AB is ALL; cache once) ----
  mc <- get_merch_criteria("AB") %>% dplyr::slice(1)

  req_mc <- c("stumpht_m", "topdbh_cm", "mindbh_cm")
  miss_mc <- setdiff(req_mc, names(mc))
  if (length(miss_mc) > 0) {
    rlang::abort(paste0(
      "get_merch_criteria('AB') missing columns: ",
      paste(miss_mc, collapse = ", ")
    ))
  }

  stumpht <- mc$stumpht_m[[1]]
  topdbh <- mc$topdbh_cm[[1]]
  mindbh <- mc$mindbh_cm[[1]]

  if (!is.finite(stumpht) || stumpht < 0) {
    rlang::abort("Invalid AB stumpht_m in merch criteria.")
  }
  if (!is.finite(topdbh) || topdbh <= 0) {
    rlang::abort("Invalid AB topdbh_cm in merch criteria.")
  }
  if (!is.finite(mindbh) || mindbh < 0) {
    rlang::abort("Invalid AB mindbh_cm in merch criteria.")
  }

  # ---- internal: pivot Huang params (long -> one-row wide) ----
  huang_params_wide <- function(p_long) {
    # expects columns parameter, estimate
    if (!all(c("parameter", "estimate") %in% names(p_long))) {
      return(NULL)
    }

    p_w <- p_long %>%
      dplyr::select(parameter, estimate) %>%
      dplyr::mutate(parameter = as.character(parameter)) %>%
      tidyr::pivot_wider(names_from = parameter, values_from = estimate)

    # required coefficients
    need <- c("a0", "a1", "a2", "b1", "b2", "b3", "b4", "b5")
    if (!all(need %in% names(p_w))) {
      return(NULL)
    }

    # ensure exactly one row
    if (nrow(p_w) != 1) {
      return(NULL)
    }

    p_w
  }

  # ---- internal: Huang DIB at height h (m), inside bark (cm) ----
  huang_dib <- function(h, DBH, HT, p) {
    if (!is.finite(h) || !is.finite(DBH) || !is.finite(HT)) {
      return(NA_real_)
    }
    if (HT <= 0) {
      return(NA_real_)
    }
    z <- h / HT
    if (!is.finite(z) || z <= 0) {
      return(NA_real_)
    }
    if (z > 1) {
      z <- 1
    }

    x <- (1 - sqrt(z)) / per
    if (!is.finite(x) || x < 0) {
      x <- 0
    }

    cc <- p$b1 + 0 * z # placeholder to keep structure; overwritten below
    cc <- p$b1 *
      (z^2) +
      p$b2 * log(z + 0.001) +
      p$b3 * sqrt(z) +
      p$b4 * exp(z) +
      p$b5 * (DBH / HT)

    ff <- p$a0 * (DBH^p$a1) * (p$a2^DBH)
    if (!is.finite(ff) || ff <= 0) {
      return(NA_real_)
    }

    dib <- ff * (x^cc)
    if (!is.finite(dib)) {
      return(NA_real_)
    }
    dib
  }

  # ---- internal: solve g (relative merch height) such that DIB == topdbh ----
  solve_g <- function(DBH, HT, p, topdbh) {
    g0 <- 0.9
    g1 <- 0
    maxiter <- 1000
    tol <- 1e-8

    iii <- 0
    while (abs(g0 - g1) > tol && iii <= maxiter) {
      cc <- p$b1 *
        (g0^2) +
        p$b2 * log(g0 + 0.001) +
        p$b3 * sqrt(g0) +
        p$b4 * exp(g0) +
        p$b5 * (DBH / HT)

      ff <- p$a0 * (DBH^p$a1) * (p$a2^DBH)
      if (!is.finite(cc) || cc == 0 || !is.finite(ff) || ff <= 0) {
        return(NA_real_)
      }

      g1 <- (1 - ((topdbh / ff)^(1 / cc)) * per)^2
      if (!is.finite(g1)) {
        return(NA_real_)
      }

      g0 <- (g0 + g1) / 2
      g0 <- max(min(g0, 1), 0)

      iii <- iii + 1
    }

    if (iii > maxiter) {
      return(NA_real_)
    }
    g0
  }

  # ---- outputs ----
  vol_total <- numeric(n)
  vol_merch <- numeric(n)

  for (i in seq_len(n)) {
    dbh <- DBH[i]
    HT <- height[i]

    if (is.na(dbh) || is.na(HT) || !is.finite(dbh) || !is.finite(HT)) {
      abort_i(i, "DBH and height must be finite numeric values (not NA/Inf).")
    }
    if (dbh <= 0) {
      abort_i(i, "DBH must be > 0.")
    }
    if (HT <= 0) {
      abort_i(i, "height must be > 0.")
    }
    if (HT < 1.3) {
      HT <- 1.3
    }

    # below minimum dbh is valid -> zeros
    if (dbh < mindbh) {
      vol_total[i] <- 0
      vol_merch[i] <- 0
      next
    }

    # ---- parameters: try subregion first; fallback to Province ----
    subr_req <- subr_std[i] # what the user asked for (after UB -> LBH mapping)
    subr_used <- subr_req

    p_long <- get_volume_params(
      model_id = "regional_huang94",
      species = species_std[i],
      subregion = subr_used
    )

    if (nrow(p_long) == 0) {
      # only warn if the user *actually* asked for a subregion (not Province)
      if (!identical(subr_req, "PROVINCE")) {
        rlang::warn(paste0(
          "vol_huang94(): no Huang94 parameters for (species=",
          species_std[i],
          ", subregion=",
          subr_req,
          "); using Province-wide parameters instead (row ",
          i,
          ")."
        ))
      }

      subr_used <- "Province"
      p_long <- get_volume_params(
        model_id = "regional_huang94",
        species = species_std[i],
        subregion = subr_used
      )
    }

    if (nrow(p_long) == 0) {
      abort_i(
        i,
        "No Huang94 parameters found (neither subregion nor Province)."
      )
    }

    p_w <- huang_params_wide(p_long)
    if (is.null(p_w)) {
      abort_i(
        i,
        "Returned Huang94 parameters could not be reshaped to required coefficients (a0,a1,a2,b1..b5)."
      )
    }

    # pull scalars
    p <- list(
      a0 = p_w$a0[[1]],
      a1 = p_w$a1[[1]],
      a2 = p_w$a2[[1]],
      b1 = p_w$b1[[1]],
      b2 = p_w$b2[[1]],
      b3 = p_w$b3[[1]],
      b4 = p_w$b4[[1]],
      b5 = p_w$b5[[1]]
    )
    if (any(!is.finite(unlist(p)))) {
      abort_i(
        i,
        paste0(
          "One or more Huang94 parameters are not finite (NA/Inf) for subregion=",
          subr_i,
          "."
        )
      )
    }

    # ---- solve merchantable relative height g and merchantable height hi ----
    g <- solve_g(dbh, HT, p, topdbh)
    if (!is.finite(g)) {
      abort_i(
        i,
        "Merchantable height solver failed to converge or produced non-finite value."
      )
    }

    hi <- g * HT
    if (!is.finite(hi)) {
      abort_i(i, "Computed merchantable height is non-finite.")
    }

    # if merchantable height is below stump, merchantable volume is 0,
    # and all above-stump volume becomes "top"
    if (hi <= stumpht) {
      dib_stump <- huang_dib(stumpht, dbh, HT, p)
      if (!is.finite(dib_stump) || dib_stump < 0) {
        abort_i(
          i,
          "Failed computing DIB at stump height (non-finite/negative)."
        )
      }

      volstp <- cons * dib_stump^2 * stumpht
      if (!is.finite(volstp) || volstp < 0) {
        abort_i(i, "Computed stump volume is invalid.")
      }

      # approximate top volume as cone from topdbh at hi to tip; if hi==stump, it’s entire stem above stump
      tipvol <- cons * (topdbh^2) * (HT - hi) / 3
      if (!is.finite(tipvol) || tipvol < 0) {
        abort_i(i, "Computed top volume is invalid.")
      }

      vol_merch[i] <- 0
      vol_total[i] <- volstp + tipvol
      next
    }

    # ---- compute diameters for Newton integration (20 sub-sections => 21 diameters) ----
    # points at stump + (1..20)*(hi-stump)/20 + stump, so that odd indexes are midpoints of 10 sections
    mlen <- (1:20) * (hi - stumpht) / 20 + stumpht
    z <- mlen / HT
    x <- (1 - sqrt(z)) / per

    # dib at stump
    dib_stump <- huang_dib(stumpht, dbh, HT, p)
    if (!is.finite(dib_stump) || dib_stump < 0) {
      abort_i(i, "Failed computing DIB at stump height (non-finite/negative).")
    }

    # dib at the 20 interior points (including hi)
    ff <- p$a0 * (dbh^p$a1) * (p$a2^dbh)
    if (!is.finite(ff) || ff <= 0) {
      abort_i(i, "Form factor is non-finite or <= 0 (check a0/a1/a2).")
    }

    cc_vec <- p$b1 *
      (z^2) +
      p$b2 * log(z + 0.001) +
      p$b3 * sqrt(z) +
      p$b4 * exp(z) +
      p$b5 * (dbh / HT)

    dibx <- ff * (x^cc_vec)
    dibm <- c(dib_stump, dibx)

    if (length(dibm) != 21) {
      abort_i(
        i,
        "Internal error: expected 21 diameters for Newton integration."
      )
    }
    if (any(!is.finite(dibm)) || any(dibm < 0)) {
      abort_i(i, "One or more predicted DIB values are non-finite/negative.")
    }

    # ---- Newton's formula for merchantable volume to topdbh ----
    # (hi-stump)/10 is length of each of 10 sections; dibm has start/mid/end points for each
    k <- cons * (((hi - stumpht) / 10) / 6)

    mvol <- 0
    for (seg in 1:10) {
      idx0 <- (seg - 1) * 2 + 1
      idxm <- idx0 + 1
      idx1 <- idx0 + 2
      mvol <- mvol + k * (dibm[idx0]^2 + 4 * dibm[idxm]^2 + dibm[idx1]^2)
    }

    if (!is.finite(mvol) || mvol < 0) {
      abort_i(i, "Computed merchantable volume is non-finite/negative.")
    }

    # stump volume (cylinder-ish like original)
    volstp <- cons * dibm[1]^2 * stumpht
    if (!is.finite(volstp) || volstp < 0) {
      abort_i(i, "Computed stump volume is non-finite/negative.")
    }

    # tip volume (cone from topdbh at hi to tip; same form as your legacy code)
    tipvol <- cons * (topdbh^2) * (HT - hi) / 3
    if (!is.finite(tipvol) || tipvol < 0) {
      abort_i(i, "Computed tip volume is non-finite/negative.")
    }

    vol_merch[i] <- mvol
    vol_total[i] <- mvol + tipvol + volstp

    if (!is.finite(vol_total[i]) || vol_total[i] < 0) {
      abort_i(i, "Computed total volume is non-finite/negative.")
    }
  }

  dplyr::tibble(
    vol_total = vol_total,
    vol_merchantable = vol_merch
  )
}

# vol_huang94 <- function(DBH, height, species, subregion = "Province") {
#   #model parameters stored in 'parameters_HuangV'

#   #checks-----
#   if (!is.numeric(DBH)) {
#     stop("'DBH' must be type numeric")
#   }
#   if (!is.numeric(height)) {
#     stop("'height' must be type numeric")
#   }
#   if (!is.character(species)) {
#     stop("'species' must be type character")
#   }
#   if (!is.character(subregion)) {
#     stop("'subregion' must be type character")
#   }

#   #check if species is included in the coefficients table
#   if (!(species %in% unique(parameters_HuangV$species))) {
#     stop(glue::glue("No model parameters available for {species}"))
#   }

#   #check species, subregion, and species+subregion.
#   #note that in Huang et al 1994, Boreal Highlands (code 6) are not split into Lower and Upper.
#   #In the current subregion classification, code 6 is "Lower Boreal Highlands (Boreal Highlands)" or "LBH",
#   # while code 21 is "Upper Boreal Highlands (Boreal Highlands)" or "UB".
#   # Because of this, if subregion is UB then it is replaced with LBH
#   if (subregion == "UB") {
#     subregion <- "LBH"
#   }

#   if (!(subregion %in% unique(parameters_HuangV$NaturalSubregionCode))) {
#     stop(
#       "Wrong subregion code. Subregion codes are listed in AlbertaNaturalRegSubreg dataset"
#     )
#   }

#   #check species and region combination - e.g. there are no parameters for Provincial models for sofwood or hardwood groups
#   if (subregion != "Province") {
#     isAvailable <- nrow(parameters_HuangV[
#       parameters_HuangV$species == species &
#         parameters_HuangV$NaturalSubregionCode == subregion,
#     ]) >
#       0

#     if (!isAvailable) {
#       warning(glue::glue(
#         "No model parameters available for {species} in {subregion}. Using Province-level parameters."
#       ))
#       subregion <- "Province"
#     }
#   }

#   #get parameters----
#   params <- parameters_HuangV |>
#     dplyr::filter(NaturalSubregionCode == subregion, species == !!species)

#   #check if number of parameters is correct (need to be 8)
#   if (nrow(params) != 8) {
#     stop("Error in parameter selection")
#   }

#   #assign parameter values
#   for (i in 1:nrow(params)) {
#     assign(x = params$parameter[i], params$estimate[i])
#   }

#   #calculations----
#   #Code adapted from the SAS code in Appendix 3 in Huang et al 1994. Original comments are included.

#   #Define g = h/height, set the initial value for g;
#   g0 = 0.9

#   #The following iteration process is repeated until the desired precision is obtained
#   #A 2.0 cm top diameter inside bark is assumed
#   #need to set initial value for g1 as well
#   g1 <- 0

#   iii <- 0 #counter to protect against endless loops
#   maxiter <- 1000 #maximum number of iterations

#   while (abs(g0 - g1) > 0.00000001 & iii <= maxiter) {
#     cc <- b1 *
#       (g0)**2 +
#       b2 * log(g0 + 0.001) +
#       b3 * sqrt(g0) +
#       b4 * exp(g0) +
#       b5 * (DBH / height)

#     g1 <- (1 -
#       ((2 / (a0 * DBH**a1 * a2**DBH))**(1 / cc)) * (1 - sqrt(0.225)))**2

#     g0 <- (g0 + g1) / 2

#     iii <- iii + 1
#   }

#   #if no convergence then assign NAs to results
#   if (iii > 1000 | !is.finite(g1) | !is.finite(g0)) {
#     tvol <- mvol <- NA
#   } else {
#     #Compute merchantable height (hi) and merchantable length (mlen);
#     #A stump height of 0.30 m is assumed;
#     hi = g0 * height
#     # mlen = hi - 0.3

#     # Divide merchantable length into 10 sections of equal length;
#     # Compute the height above the ground from the middle and the top of each section;
#     mlen <- 1:20 * (hi - 0.3) / 20 + 0.3

#     # Prediction of diameter inside bark at the middle and top of each section, using the taper equation;
#     # Diameter inside bark at stump height is also predicted with stump height = 0.3 meters;
#     z <- mlen / height

#     x <- (1 - sqrt(z)) / (1 - sqrt(.225))

#     dibm0 <-
#       (a0 * DBH^a1) *
#       (a2^DBH) *
#       ((1 - sqrt(0.3 / height)) / (1 - sqrt(.225)))^(b1 *
#         (0.3 / height)^2 +
#         b2 * log(0.3 / height + 0.001) +
#         b3 * sqrt(0.3 / height) +
#         b4 * exp(0.3 / height) +
#         b5 * DBH / height)

#     dibx <-
#       (a0 * DBH^a1) *
#       (a2^DBH) *
#       x^(b1 *
#         z^2 +
#         b2 * log(z + 0.001) +
#         b3 * sqrt(z) +
#         b4 * exp(z) +
#         b5 * DBH / height)

#     dibm <- c(dibm0, dibx)

#     # Compute the merchantable volume (mvol) of the tree to 2.0 cm top dib
#     # Calculate mvol in terms of cubic meter using Newton's formula

#     k <- 0.00007854 * (((hi - 0.3) / 10) / 6)

#     mvol <-
#       k *
#       (dibm[1]^2 + 4 * dibm[2]^2 + dibm[3]^2) +
#       k * (dibm[3]^2 + 4 * dibm[4]^2 + dibm[5]^2) +
#       k * (dibm[5]^2 + 4 * dibm[6]^2 + dibm[7]^2) +
#       k * (dibm[7]^2 + 4 * dibm[8]^2 + dibm[9]^2) +
#       k * (dibm[9]^2 + 4 * dibm[10]^2 + dibm[11]^2) +
#       k * (dibm[11]^2 + 4 * dibm[12]^2 + dibm[13]^2) +
#       k * (dibm[13]^2 + 4 * dibm[14]^2 + dibm[15]^2) +
#       k * (dibm[15]^2 + 4 * dibm[16]^2 + dibm[17]^2) +
#       k * (dibm[17]^2 + 4 * dibm[18]^2 + dibm[19]^2) +
#       k * (dibm[19]^2 + 4 * dibm[20]^2 + dibm[21]^2)

#     #Compute trees/m3 merchantable volume, tip volume, stump volume, and total volume;

#     # trees <- 1/mvol

#     tipvol <- 0.00007854 * dibm[21]^2 * (height - hi) / 3

#     volstp <- 0.00007854 * dibm[1]^2 * 0.3

#     tvol <- mvol + tipvol + volstp

#     # r <- cbind(mvol,tvol)
#     # names(r) <- c("v_merch","v_total")
#   }

#   r <- list(v_merch = mvol, v_total = tvol)

#   return(r)
# }
