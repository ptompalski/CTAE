#' Model parameters required for total volume to merchantable volume conversion
#' 
#' A dataset containing parameters for the Boudewyn et al 2007 total volume to merchantable volume conversion models. An updated version
#' of the parameters was downloaded from https://nfi.nfis.org/en/biomass_models.
#' 
#' 
#'' @format A data frame consisting of model parameters. Table 14 in Boudewyn et al 2007.
#' 
#' @references 
#' Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007). Model-based, volume-to-biomass conversion for forested and vegetated land in Canada. Natural Resources Canada, Canadian Forest Service, Pacific Forestry Centre, Victoria, BC. Information Report BC-X-411. 112 p.
"params_Vtot2Vmerch"




#' Model parameters required for Canadian national tree aboveground biomass equations
#'
#' A dataset containing the coefficients for the Lambert et al 2005 and Ung et al 2008 AGB models. 
#' Parameters for both types of models are included (models based on DBH only and models based on DBH and height)
#'
#' @format A data frame with five variables:
#' \describe{
#'   \item{species}{Tree species code following the NFI standard (e.g. POPU.TRE)}
#'   \item{model}{Type of the AGB model - based on DBH-only, or DBH and height}
#'   \item{parameter}{Name of model parameter}
#'   \item{estimate}{Parameter estimate}
#'   \item{stderr}{Standard error of the estimate}
#' }
#' @references
#' Lambert, M. C., Ung, C. H., & Raulier, F. (2005). Canadian national tree aboveground biomass equations. Canadian Journal of Forest Research, 35(8), 1996–2018. https://doi.org/10.1139/x05-112
#' 
#' Ung, C.-H., Bernier, P., & Guo, X.-J. (2008). Canadian national biomass equations: new parameter estimates that include British Columbia data. Canadian Journal of Forest Research, 38(5), 1123–1132. https://doi.org/10.1139/X07-224
#' 
#' @seealso \code{\link{AGB_LambertUngDBH}} and \code{\link{AGB_LambertUngDBHHT}}
"parameters_LambertUng"




#' Model parameters required for model based Volume-to-biomass conversion
#' 
#' A dataset containing parameters for the Boudewyn et al 2007 volume-to-biomass conversion models. An updated version
#' of the parameters was downloaded from https://nfi.nfis.org/en/biomass_models.
#' 
#' 
#'' @format A list of four data frames corresponding to Tables 3-6:
#' \describe{
#'   \item{V2B_params_t3}{Table 3 in Boudewyn et al. 2007 - Stem wood biomass model parameters for merchantable-sized trees by jurisdiction, ecozone and lead species}
#'   \item{V2B_params_t4}{Table 4 in Boudewyn et al. 2007 - Stem wood biomass model parameters for nonmerchantable-sized trees by jurisdiction, ecozone and lead species}
#'   \item{V2B_params_t5}{Table 5 in Boudewyn et al. 2007 - Stem wood biomass model parameters for sapling-sized trees by jurisdiction, ecozone and predominant genus}
#'   \item{V2B_params_t6}{Table 6 in Boudewyn et al. 2007 - Proportion model parameters by jurisdiction, ecozone and lead species}
#'   
#' }
#' 
#' @references 
#' Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007). Model-based, volume-to-biomass conversion for forested and vegetated land in Canada. Natural Resources Canada, Canadian Forest Service, Pacific Forestry Centre, Victoria, BC. Information Report BC-X-411. 112 p.
"parameters_V2B"



#' Model parameters required for tree volume equations for major Alberta tree species
#' 
#' A dataset containing parameters for the Huang 1994 tree volume models in Alberta
#' 
#' 
#' @format A data frame with four variables:
#' \describe{
#'   \item{species}{Tree species code following the NFI standard (e.g. POPU.TRE)}
#'   \item{parameter}{Name of model parameter}
#'   \item{estimate}{Parameter estimate}
#'   \item{NaturalSubregionNum}{Number for natural subregions of Alberta (See \code{\link{AlbertaNaturalRegSubreg}} and Appendix 4 in Huang 1994)}
#'   \item{NaturalSubregionCode}{Code for natural subregions of Alberta. 'Province' indicates provincial-level parameters (See \code{\link{AlbertaNaturalRegSubreg}} and Appendix 4 in Huang et al. 1994)}
#' }
#' 
#' @references 
#' Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation for Major Alberta Tree Species. Report 1 - Individual tree volume estimation procedures for Alberta: Methods of Formulation and Statistical Foundations. Alberta Environmental Protection, Land and Forest Service, Forest Management Division, Edmonton, AB.
"parameters_HuangV"




#' Natural regions and subregions of Alberta
#' 
#' A dataset containing a list of Alberta regions, subregions, and their codes.
#' 
#' 
#' @format A data frame with four variables:
#' \describe{
#'   \item{NaturalRegion}{Natural region name}
#'   \item{NaturalSubregion}{Natural subregion name}
#'   \item{NaturalSubregionNum}{Subregion numeric code}
#'   \item{NaturalSubregionCode}{Subregion text code}
#' }
#' @references
#' https://open.alberta.ca/publications/0778545725
"AlbertaNaturalRegSubreg"



















