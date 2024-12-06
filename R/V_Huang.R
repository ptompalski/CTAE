# HuangV_model_availability <- parameters_HuangV |> group_by(species, NaturalSubregionCode) |> 
#   count()  |> 
#   mutate(NaturalSubregionCode = if_else(is.na(NaturalSubregionCode), "Province", NaturalSubregionCode) )  |> 
#   select(-n)
# 
# usethis::use_data(HuangV_model_availability, internal = TRUE)



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
#' @return A list containing merchantable and total volume values [m3].
#' 
#' @references 
#' Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation for Major Alberta Tree Species. Report 1 - Individual tree volume estimation procedures for Alberta: Methods of Formulation and Statistical Foundations. Alberta Environmental Protection, Land and Forest Service, Forest Management Division, Edmonton, AB.

#' @examples
#' V_Huang(20, 20, "PICE.GLA")
#' V_Huang(20, 20, "PICE.GLA", subregion = "CP")
#' 
#' @export




V_Huang <- function(DBH, height, species, subregion="Province") {
  
  #model parameters stored in 'parameters_HuangV'
  
  #checks-----
  if(!is.numeric(DBH))            stop("'DBH' must be type numeric")
  if(!is.numeric(height))         stop("'height' must be type numeric")
  if(!is.character(species))      stop("'species' must be type character")
  if(!is.character(subregion))    stop("'subregion' must be type character")
  
  #check if species is included in the coefficients table
  if (!(species %in% unique(parameters_HuangV$species))) {
    stop(glue::glue("No model parameters available for {species}"))
  }
  
  #check species, subregion, and species+subregion.
  #note that in Huang et al 1994, Boreal Highlands (code 6) are not split into Lower and Upper.
  #In the current subregion classification, code 6 is "Lower Boreal Highlands (Boreal Highlands)" or "LBH", 
  # while code 21 is "Upper Boreal Highlands (Boreal Highlands)" or "UB".
  # Because of this, if subregion is UB then it is replaced with LBH
  if (subregion == "UB") subregion <- "LBH"
  
  if (!(subregion %in% unique(parameters_HuangV$NaturalSubregionCode))) stop("Wrong subregion code. Subregion codes are listed in AlbertaNaturalRegSubreg dataset")
  
  
  #check species and region combination - e.g. there are no parameters for Provincial models for sofwood or hardwood groups
  if(subregion != "Province") {
    isAvailable <- nrow(parameters_HuangV[parameters_HuangV$species==species & parameters_HuangV$NaturalSubregionCode==subregion,]) > 0
    
    
    if(!isAvailable) {
      
      warning(glue::glue("No model parameters available for {species} in {subregion}. Using Province-level parameters."))
      subregion <- "Province"
      
    }
  }
  
  #get parameters----
  params <- parameters_HuangV |> dplyr::filter(NaturalSubregionCode == subregion, species == !!species)
  
  
  #check if number of parameters is correct (need to be 8)
  if(nrow(params) != 8) stop("Error in parameter selection")
  
  
  #assign parameter values
  for (i in 1:nrow(params)) {
    assign(x = params$parameter[i], params$estimate[i])
  }
  
  
  #calculations----
  #Code adapted from the SAS code in Appendix 3 in Huang et al 1994. Original comments are included.
  
  #Define g = h/height, set the initial value for g;
  g0 = 0.9
  
  #The following iteration process is repeated until the desired precision is obtained
  #A 2.0 cm top diameter inside bark is assumed
  #need to set initial value for g1 as well
  g1 <- 0
  
  
  iii <- 0          #counter to protect against endless loops
  maxiter <- 1000   #maximum number of iterations
  
  while(abs(g0-g1) > 0.00000001 & iii <= maxiter) {
    
    cc <- b1*(g0)**2 + b2*log(g0+0.001) + b3*sqrt(g0) + b4*exp(g0) + b5*(DBH/height)
    
    g1 <- (1 - ((2/(a0*DBH**a1*a2**DBH))**(1/cc))*(1 - sqrt(0.225)))**2
    
    g0 <- (g0 + g1)/2; 
    
    iii <- iii+1
  }
  
  #if no convergence then assign NAs to results
  if(iii > 1000 | !is.finite(g1) | !is.finite(g0)) { 
    
    tvol <- mvol <- NA
    
  } else {
    
    #Compute merchantable height (hi) and merchantable length (mlen); 
    #A stump height of 0.30 m is assumed;
    hi = g0 * height
    # mlen = hi - 0.3
    
    
    # Divide merchantable length into 10 sections of equal length; 
    # Compute the height above the ground from the middle and the top of each section;
    mlen <- 1:20 * (hi - 0.3)/20 + 0.3
    
    # Prediction of diameter inside bark at the middle and top of each section, using the taper equation;
    # Diameter inside bark at stump height is also predicted with stump height = 0.3 meters;
    z <- mlen / height
    
    x <- (1 - sqrt(z)) / (1 - sqrt(.225))
    
    
    dibm0 <-  
      (a0 * DBH^a1) * 
      (a2^DBH) * 
      ((1 - sqrt(0.3/height)) / (1 - sqrt(.225)))^
      (
        b1 * (0.3/height)^2 + 
          b2 * log(0.3 / height+0.001) + 
          b3 * sqrt(0.3 / height) + 
          b4 * exp(0.3 / height) + 
          b5 * DBH / height
      )
    
    
    dibx <- 
      (a0 * DBH^a1) * 
      (a2^DBH) * 
      x^(b1 * z^2 + b2 * log(z+0.001) + b3 * sqrt(z) + b4 * exp(z) + b5 * DBH/height)
    
    dibm <- c(dibm0,dibx)
    
    
    # Compute the merchantable volume (mvol) of the tree to 2.0 cm top dib
    # Calculate mvol in terms of cubic meter using Newton's formula
    
    k <- 0.00007854 * (((hi-0.3)/10)/6)
    
    mvol <-
      k * (dibm[1]^2 +4 * dibm[2]^2 + dibm[3]^2) + 
      k * (dibm[3]^2 +4 * dibm[4]^2 + dibm[5]^2) + 
      k * (dibm[5]^2 +4 * dibm[6]^2 + dibm[7]^2) + 
      k * (dibm[7]^2 +4 * dibm[8]^2 + dibm[9]^2) + 
      k * (dibm[9]^2 +4 * dibm[10]^2 + dibm[11]^2) + 
      k * (dibm[11]^2+4 * dibm[12]^2 + dibm[13]^2) + 
      k * (dibm[13]^2+4 * dibm[14]^2 + dibm[15]^2) +
      k * (dibm[15]^2+4 * dibm[16]^2 + dibm[17]^2) + 
      k * (dibm[17]^2+4 * dibm[18]^2 + dibm[19]^2) + 
      k * (dibm[19]^2+4 * dibm[20]^2 + dibm[21]^2) 
    
    
    #Compute trees/m3 merchantable volume, tip volume, stump volume, and total volume;
    
    # trees <- 1/mvol 
    
    tipvol <- 0.00007854 * dibm[21]^2 * (height-hi)/3
    
    volstp <- 0.00007854*dibm[1]^2 * 0.3
    
    tvol <- mvol + tipvol + volstp
    
    # r <- cbind(mvol,tvol)
    # names(r) <- c("v_merch","v_total")
  }
  
  r <- list(v_merch = mvol,
            v_total = tvol)
  
  return(r)
}




