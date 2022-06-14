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

AGB_LambertUngDBH <- function(DBH, species=NA){
  
  params <- parameters_LambertUng |> filter(model == "DBH")
  
  if (is.na(species)) {
    warning("No species provided. Using coefficients for all species.")
    species <- "UNKN.SPP"
  } 
  
  #species to capital letters
  species <- toupper(species)
  
  #check if species is included in the coefficients table
  if (!(species %in% unique(params$species))) {
    stop("Wrong species")
  }
  
  #subset parameters for the current species
  B <- params[params$species==species,]
  
  #assign values to coefficients
  for (i in 1:nrow(B)) {
    assign(x = B$parameter[i], B$estimate[i])
  }
  
  Ywood     <- bwood1 * DBH ^ bwood2
  Ybark     <- bbark1 * DBH ^ bbark2
  Ystem     <- Ywood + Ybark
  Yfoliage  <- bfoliage1 * DBH ^ bfoliage2
  Ybranches <- bbranches1 * DBH ^ bbranches2
  Ycrown    <- Yfoliage + Ybranches
  Ytotal    <- Ywood + Ybark + Yfoliage + Ybranches
  
  R <- list(
    Bwood = Ywood,
    Bbark = Ybark,
    Bstem = Ystem,
    Bfoliage = Yfoliage,
    Bbranches = Ybranches,
    Bcrown = Ycrown,
    Btotal = Ytotal
  )
  
  # R <- R |> as_tibble()
  
  return(R) 
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


AGB_LambertUngDBHHT<-function(DBH, height, species=NA){
  
  params <- parameters_LambertUng |> filter(model == "DBHHT")
  
  if (is.na(species)) {
    warning("No species provided. Using coefficients for all species.")
    species <- "UNKNSPP"
  } 
  
  #species to capital letters
  species <- toupper(species)
  
  #check if species is included in the coefficients table
  if (!(species %in% unique(params$species))) {
    stop("Wrong species")
  }
  
  
  B <- params[params$species==species,]
  
  #assign values to coefficients
  for (i in 1:nrow(B)) {
    assign(x = B$parameter[i], B$estimate[i])
  }
  
  Ywood     <- bwood1 * DBH ^ bwood2 * height ^ bwood3
  Ybark     <- bbark1 * DBH ^ bbark2 * height ^ bbark3
  Ystem     <- Ywood + Ybark
  Yfoliage  <- bfoliage1 * DBH ^ bfoliage2 * height ^ bfoliage3
  Ybranches <- bbranches1 * DBH ^ bbranches2 * height ^ bbranches3
  Ycrown    <- Yfoliage + Ybranches
  Ytotal    <- Ywood + Ybark + Yfoliage + Ybranches
  
  R <- list(
    Bwood = Ywood,
    Bbark = Ybark,
    Bstem = Ystem,
    Bfoliage = Yfoliage,
    Bbranches = Ybranches,
    Bcrown = Ycrown,
    Btotal = Ytotal
  )
  
  # R <- R |> as_tibble()
  
  return(R) 
  
}
