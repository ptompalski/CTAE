#' Total volume to gross (net in B.C.) merchantable volume conversion
#'
#' @description Total volume to gross (net in B.C.) merchantable volume conversion based on Appendix 6 in Boudewyn et al. 2007 (see references). 
#' @param volume Gross merchantable volume/ha (net in BC) of all live trees
#' @param species Species code in the NFI standard (e.g. POPU.TRE) 
#' @param jurisdiction A two-letter code depicting jurisdiction (e.g. "AB", "BC", etc)
#' @param ecozone ecozone number (1-15) following Canfi2001 - see table 2 of appendix 7 of Boudewyn et al (2007).
#' 
#' @return Merchantable volume 
#' 
#' @references 
#' Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007). Model-based, volume-to-biomass conversion for forested and vegetated land in Canada. Natural Resources Canada, Canadian Forest Service, Pacific Forestry Centre, Victoria, BC. Information Report BC-X-411. 112 p.
#' 
#' @examples
#' Vtot2Vmerch(total_volume = 300, species = "PINU.CON", jurisdiction = "AB", ecozone = 4)
#' 
#' @export

Vtot2Vmerch <- function(total_volume, 
                species, #only genus is required
                jurisdiction, 
                ecozone) {
  
  #checks
  
  if(!is.numeric(total_volume))   stop("'total_volume' must be type numeric")
  if(!is.character(species))      stop("'species' must be type character")
  if(!is.character(jurisdiction)) stop("'jurisdiction' must be type character")
  if(!is.numeric(ecozone))        stop("'ecozone' must be type numeric")
  
  
  #convert 'species' to: genus, species, variety
  species <- stringr::str_split(species, "\\.")[[1]]
  
  if(length(species) >= 2) {
    genus <- species[1]
    spp <- species[2] 
    variety  <- species[3]
  } else {
    stop("Wrong species format")
  }
  
  
  #get parameters
  B <- CTAE:::Vtot2Vmerchgetparams(
    genus = genus,
    jurisdiction = jurisdiction,
    ecozone = ecozone
  )
  
  
  # merchantable_volume = proportion × total_volume
  # proportion = k + a × (1 – exp(b × total_volume))^c
  
  #Calculations:
  
  proportion <- B$k + B$a * (1 - exp(B$b * total_volume))^B$c
  
  merchantable_volume <- proportion * total_volume
  
  return(merchantable_volume)
}




#internal not exported
Vtot2Vmerchgetparams <- function(genus, 
                         jurisdiction, 
                         ecozone
                         
) {
  
  B <- params_Vtot2Vmerch |>    
    dplyr::filter(juris_id == !!jurisdiction, 
           genus == !!genus,
           ecozone == !!ecozone )
  if(nrow(B) != 1) stop("Error in parameter selection.")

  return(B)
  
}

