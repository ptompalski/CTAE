#' Volume-to-biomass conversion
#'
#' @description Implementation of the model-based, volume-to-biomass conversion equations by Boudewyn et al. (2007).
#' Note - only scenarios 1 and 2 are currently implemented.
#' @param volume Gross merchantable volume/ha (net in BC) of all live trees
#' @param species Species code in the NFI standard (e.g. POPU.TRE) 
#' @param jurisdiction A two-letter code depicting jurisdiction (e.g. "AB")
#' @param ecozone ecozone number (1-15) 
#' 
#' @return A list containing aboveground biomass values. Column names correspond to variable names
#' used in Boudewyn et al. 2007: 
#' \describe{
#' \item{\code{b_m}}{Total stem wood biomass of merchantable-sized live trees (biomass includes stumps and tops), in metric tonnes per ha.}
#' \item{\code{b_n}}{stem wood biomass of live, nonmerchantable-sized trees (tonnes/ha)}
#' \item{\code{b_mn}}{b_m + b_n}
#' \item{\code{b_s}}{stem wood biomass of live, sapling-sized trees (tonnes/ha)}
#' \item{\code{b_total}}{Total tree biomass}
#' \item{\code{b_bark}}{Total bark biomass}
#' \item{\code{b_branches}}{Total branch biomass}
#' \item{\code{b_foliage}}{Total foliage biomass}
#' }
#' 
#' @references 
#' Boudewyn et al (2007). Model Based Volume-to-biomass Conversion for Forested and Vegetated Land in Canada. In Forestry. Pacific Forestry Centre. http://sbisrvntweb.uqac.ca/archivage/030078750.pdf
#' 
#' @examples
#' V2B(350, species = "PINU.CON",jurisdiction = "BC", ecozone=4)
#' 
#' @export

V2B <- function(volume, 
                species, 
                jurisdiction, 
                ecozone) {
  
  #checks
  
  if(!is.numeric(volume))         stop("'volume' must be type numeric")
  if(!is.character(species))      stop("'species' must be type character")
  if(!is.character(jurisdiction)) stop("'jurisdiction' must be type character")
  if(!is.numeric(ecozone))        stop("'ecozone' must be type numeric")
  
  
  
  
  
  #convert 'species' to: genus, species, variety
  species <- str_split(species, "\\.")[[1]]
  
  if(length(species) >= 2) {
    genus <- species[1]
    spp <- species[2] 
    variety  <- species[3]
  } else {
    stop("Wrong species format")
  }
  
  
  #get parameters
  B <- CTAE:::V2Bgetparams(
                          genus = genus,
                          species = spp,
                          variety = variety,
                          jurisdiction = jurisdiction,
                          ecozone = ecozone
                          )
  
  B3 <- B$B3
  B4 <- B$B4
  B5 <- B$B5
  B6 <- B$B6
  
  
  
  #Calculations:
  
  #Merchantable-sized tree stem wood biomass (Eq1, based on a and b parameters from Table 3)
  b_m <- B3$a * volume ^ B3$b #total stem wood biomass of merchantable-sized live trees (biomass includes stumps and tops)
  
  
  #Nonmerchantable-sized tree stem wood biomass
  # nonmerchfactor (Eq2, based on a, b, and k parameters from Table 4)
  nonmerchfactor = B4$k + B4$a * b_m ^ B4$b
  
  #check if nonmerchfactor is below the upper cap value
  nonmerchfactor <- ifelse(nonmerchfactor > B4$cap, B4$cap, nonmerchfactor)
  
  
  b_nm <- nonmerchfactor  * b_m
  b_n <- b_nm - b_m #stem wood biomass of live, nonmerchantable-sized trees (tonnes/ha)
  
  
  #Sapling-sized tree stem wood biomass
  # saplingfactor (Eq3, based on a, b, and k parameters from Table 5))
  saplingfactor = B5$k + B5$a * b_nm ^ B5$b
  
  #check if samplingfactor is below the upper cap value
  saplingfactor <- ifelse(saplingfactor > B5$cap, B5$cap, saplingfactor)
  
  b_snm = saplingfactor * b_nm
  b_s = b_snm - b_nm #stem wood biomass of live, sapling-sized trees
  
  
  #Proportions of total tree biomass in stemwood, stem bark, branch and foliage for live trees of all sizes
  # Equations 4-7
  lvol <- log(volume+5)
  
  p_a <- exp(B6$a1 + B6$a2 * volume + B6$a3 * lvol)
  p_b <- exp(B6$b1 + B6$b2 * volume + B6$b3 * lvol)
  p_c <- exp(B6$c1 + B6$c2 * volume + B6$c3 * lvol)
  p_abc <- 1 + p_a + p_b + p_c
  
  Pstemwood =  1 / p_abc
  Pbark =      p_a / p_abc
  Pbranches =  p_b / p_abc
  Pfoliage =   p_c / p_abc
  
  #total tree biomass
  b_total <- (b_m + b_n + b_s) / Pstemwood
  
  #total bark biomass
  b_bark <- b_total * Pbark
  
  #total branch biomass 
  b_branches <- b_total * Pbranches
  
  #total foliage biomass
  b_foliage <- b_total * Pfoliage
  
  # r <- tibble(b_m, b_n, b_nm, b_s, 
  #             # Pstemwood, Pbark, Pbranches, Pfoliage, 
  #             b_total, b_bark, b_branches, b_foliage)
  
  r <- list(b_m = b_m, 
            b_n = b_n, 
            b_nm = b_nm, 
            b_s = b_s, 
            b_total = b_total, 
            b_bark = b_bark, 
            b_branches = b_branches, 
            b_foliage = b_foliage
    
  )
  return(r)
}




#internal not exported
V2Bgetparams <- function(genus, 
                         species, 
                         variety=NA, 
                         jurisdiction, 
                         ecozone
                         
) {
  
  
  V2B_params_t3 <- parameters_V2B[[1]]
  V2B_params_t4 <- parameters_V2B[[2]]
  V2B_params_t5 <- parameters_V2B[[3]]
  V2B_params_t6 <- parameters_V2B[[4]]
  
  
  #checks
  
  
  
  
  
  #get the parameters 
  B3 <- 
    V2B_params_t3 |> 
    filter(juris_id == !!jurisdiction, 
           genus == !!genus,
           species == !!species,
           ecozone == !!ecozone )
  
  if(!is.na(variety)) {
    B3 <- B3 |> filter(variety == !!variety)
  } else {
    B3 <- B3 |> filter(is.na(variety))
  }
  
  #must be one row 
  if(nrow(B3) != 1) stop("Error in parameter selection.")
  
  
  B4 <- V2B_params_t4 |>  
    
    filter(juris_id == !!jurisdiction, 
           ecozone == !!ecozone,
           genus == !!genus,
           species == !!species
    )
  if(!is.na(variety)) {
    B4 <- B4 |> filter(variety == !!variety)
  } else {
    B4 <- B4 |> filter(is.na(variety))
  }
  
  if(nrow(B4) != 1) stop("Error in parameter selection.")
  
  
  B5 <- V2B_params_t5 |>    #saplingfactor params
    filter(juris_id == !!jurisdiction, 
           genus == !!genus,
           ecozone == !!ecozone )
  if(nrow(B5) != 1) stop("Error in parameter selection.")
  
  B6 <- V2B_params_t6 |>  
    filter(juris_id == !!jurisdiction, 
           ecozone == !!ecozone,
           genus == !!genus,
           species == !!species
    )
  
  if(!is.na(variety)) {
    B6 <- B6 |> filter(variety == !!variety)
  } else {
    B6 <- B6 |> filter(is.na(variety))
  }
  
  if(nrow(B6) != 1) stop("Error in parameter selection.")
  
  B <- list(
    B3 = B3,
    B4 = B4,
    B5 = B5,
    B6 = B6
  )
  
  return(B)
  
}
