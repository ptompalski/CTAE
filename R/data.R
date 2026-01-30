#' Merchantability criteria
#'
#' A dataset containing merchantability criteria, by jurisdiction, and species
#'
#'
#' @format A data frame consisting of merchantability criteria.
"merchcrit"

#' Zakrzewski et al. (2013) taper/volume model parameters for Ontario
#'
#' Species-level coefficients for the mathematically tractable stem taper model
#' evaluated by Zakrzewski et al. (2013) for use in Ontario. The table provides
#' taper parameters (\code{beta}, \code{gamma}), bark-factor parameters
#' (\code{delta}, \code{nu} for conifers; \code{chi} for hardwoods), and a
#' species-dependent slenderness/shape parameter (\code{rho}) used in the
#' \eqn{s(H/DBH)} formulation.
#'
#' @format A tibble with 24 rows:
#' \describe{
#'   \item{Species}{NFI species code (e.g., \code{"ABIE.BAL"}).}
#'   \item{delta}{Conifer bark-factor parameter \eqn{\delta}; 0 for hardwoods.}
#'   \item{nu}{Conifer bark-factor parameter \eqn{\eta} (stored as \code{nu}); 0 for hardwoods.}
#'   \item{rho}{Species parameter controlling the \eqn{s(H/DBH)} function form.}
#'   \item{beta}{Taper parameter \eqn{\beta}.}
#'   \item{gamma}{Taper parameter \eqn{\gamma}.}
#'   \item{chi}{Hardwood bark-factor parameter \eqn{\chi}; 0 for conifers.}
#' }
#'
#' @source
#' Zakrzewski, W. T., Penner, M. (2013). \emph{A Comparison of Tree Stem Taper Models
#' for Use in Ontario}. Ontario Forest Research Institute, Report 176.
"parameters_Zakrzewski2013"


#' Model parameters required for calculating volume using the taper model based on DBH
#'
#' A dataset containing parameters for the Ung et al 2013 models.
#'
#'
#'' @format A data frame consisting of model parameters. Table 6 in Ung et al 2013.
#'
#' @references
#' Ung, C.H., Guo, X.J., Fortin, M., 2013. Canadian national taper models. Forestry Chronicle 89, 211–224. https://doi.org/10.5558/tfc2013-040
"parameters_NationalTaperModelsDBH"

#' Model parameters required for calculating volume using the taper model based on DBH and height
#'
#' A dataset containing parameters for the Ung et al 2013 models.
#'
#'
#'' @format A data frame consisting of model parameters. Table 4 in Ung et al 2013.
#'
#' @references
#' Ung, C.H., Guo, X.J., Fortin, M., 2013. Canadian national taper models. Forestry Chronicle 89, 211–224. https://doi.org/10.5558/tfc2013-040
"parameters_NationalTaperModelsDBHHT"


#' Honer (1983) Volume Equation Parameters for Canadian Tree Species
#'
#' A species-level parameter table for the Honer (1983) individual-tree
#' volume equations, used to estimate total, merchantable, tip, and stump
#' volume from diameter at breast height (DBH) and total height.
#'
#' The parameters are derived from Honer (1983) and are intended for use
#' with functions implementing the Honer volume formulation (\code{vol_honer_dbh_ht()}).
#'
#' @format A tibble with 21 rows and 11 variables:
#' \describe{
#'   \item{Species}{Species code (e.g., \code{"PINU.STR"}).}
#'   \item{Species name}{Common English species name.}
#'   \item{b2}{Diameter inside-bark coefficient used in taper and stump
#'     calculations.}
#'   \item{b3}{Intercept term for stump diameter equation.}
#'   \item{b4}{Linear coefficient for stump diameter equation.}
#'   \item{b5}{Logarithmic height-dependent coefficient for stump diameter
#'     equation.}
#'   \item{c2}{Height-related coefficient in the total volume equation.}
#'   \item{c1}{Intercept term in the total volume equation.}
#'   \item{r1}{Intercept term for the merchantable-to-total volume ratio.}
#'   \item{r2}{Linear coefficient for the merchantable volume adjustment.}
#'   \item{r3}{Quadratic coefficient for the merchantable volume adjustment.}
#' }
#'
#' @references
#' Honer, T.G.; Ker, M.F.; Alemdag, I.S. 1983. Metric timber tables for the commercial tree species of central and eastern Canada.
#' Environ. Can., Can. For. Serv., Maritimes For. Res. Cent., Fredericton, NB. Inf. Rep. M-X-140.
#'
#' @seealso
#' \code{\link{vol_honer_dbh_ht}}
#'
"parameters_Honer"


#' Kozak (1988) variable-exponent taper model parameters
#'
#' Model parameter table for the Kozak (1988) variable-exponent taper equation,
#' used to predict inside-bark diameter along the stem (and derived volumes).
#'
#' @format A data frame containing species/species-group parameter sets required
#'   by the Kozak (1988) taper implementation in this package.
#' @references Kozak, A. (1988). A variable-exponent taper equation. *Canadian Journal
#'   of Forest Research*, 18, 1363–1368.

"parameters_Kozak88"

#' Kozak (1994) taper model parameters for British Columbia
#'
#' A dataset containing parameter estimates for the variable-exponent taper
#' models developed by Kozak (1994) for British Columbia tree species groups.
#' Parameters are provided by species group and Biogeoclimatic Ecosystem
#' Classification (BEC) zone.
#'
#' The Kozak (1994) models were developed using operational forest inventory
#' data and in some cases employ species groups rather than individual tree species.
#' This is the case for the "Spruce" group represents all spruce species combined,
#' consistent with inventory practice in British Columbia at the time.
#'
#' The parameters correspond to the variable-exponent taper equation described
#' in Kozak (1994). A multiplicative log-bias correction factor is provided for
#' back-transformation from log scale where applicable.
#'
#' @format A tibble with 73 rows and 14 columns:
#' \describe{
#'   \item{species}{Species group code used by the model (e.g., pooled genus-level groups).}
#'   \item{bec_zone}{Biogeoclimatic Ecosystem Classification (BEC) zone for which the parameters apply.}
#'   \item{a0}{Model parameter \eqn{a_0}.}
#'   \item{a1}{Model parameter \eqn{a_1}.}
#'   \item{a2}{Model parameter \eqn{a_2}.}
#'   \item{b0}{Model parameter \eqn{b_0}.}
#'   \item{b1}{Model parameter \eqn{b_1}.}
#'   \item{b2}{Model parameter \eqn{b_2}.}
#'   \item{b3}{Model parameter \eqn{b_3}.}
#'   \item{b4}{Model parameter \eqn{b_4}.}
#'   \item{b5}{Model parameter \eqn{b_5}.}
#'   \item{b6}{Model parameter \eqn{b_6}.}
#'   \item{log_bias_factor}{Multiplicative correction factor applied when
#'     back-transforming predictions from the log scale.}
#'   \item{bias_signf}{Indicator of whether the bias correction factor was
#'     statistically significant in the original analysis (1 = significant,
#'     0 = not significant).}
#' }
#'
#'
#' @references Kozak, A. (1994). Development of Taper Equations by BEC Zones and Species. Province of British Columbia, Ministry of Forests (report).
"parameters_Kozak94"


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
#'   \item{V2B_params_t7}{Table 7 in Boudewyn et al. 2007 - Caps on proportion models by jurisdiction, ecozone and lead species}
#' }
#'
#' @references
#' Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007). Model-based, volume-to-biomass conversion for forested and vegetated land in Canada. Natural Resources Canada, Canadian Forest Service, Pacific Forestry Centre, Victoria, BC. Information Report BC-X-411. 112 p.
"parameters_V2B"


#' Taper model parameters for Alberta tree species
#'
#' Species-specific parameter estimates for the individual-tree volume models
#' developed by Huang (1994) for Alberta forests, designed to be used with Kozak (1988)
#' variable exponent taper model.
#'
#' The dataset includes parameter sets applicable at the provincial level as
#' well as, where available, subregion-specific coefficients.
#'
#'
#' @format A data frame:
#' \describe{
#'   \item{Species}{Tree species code following the NFI standard (e.g. POPU.TRE)}
#'   \item{Subregion}{Natural subregion of Alberta. 'Province' indicates provincial-level parameters (See \code{\link{AlbertaNaturalRegSubreg}} and Appendix 4 in Huang et al. 1994)}
#'   \item{a0, a1, a2}{Scaling and conditioning parameters of the K2 taper model.}
#'   \item{b1, b2, b3, b4, b5}{Exponent parameters controlling stem shape in the variable-exponent formulation.}
#'  }
#'
#' @references
#' Huang, S. (1994). Ecologically Based Individual Tree Volume Estimation for Major Alberta Tree Species. Report 1 - Individual tree volume estimation procedures for Alberta: Methods of Formulation and Statistical Foundations. Alberta Environmental Protection, Land and Forest Service, Forest Management Division, Edmonton, AB.
"parameters_Huang94"

#' Taper model parameters for Saskatchewan tree species
#'
#' Species-specific parameter estimates for the Kozak (1988) variable-exponent
#' taper model, as fitted by Gal & Bella (1994) using stem section data from
#' Saskatchewan. These parameters are intended for use in numerical stem taper
#' and volume calculations based on diameter at breast height (DBH) and total
#' tree height.
#'
#' The dataset corresponds to Table 5 in Gal & Bella (1994) and includes parameter
#' estimates for 12 commercially important Saskatchewan tree species.
#'
#' @format A tibble with 12 rows and 12 columns:
#' \describe{
#'   \item{Species}{Species code following the Canadian National Forest Inventory (NFI) convention (e.g., \code{PICE.GLA}).}
#'   \item{a0, a1, a2}{Scaling and conditioning parameters of the K2 taper model.}
#'   \item{b1, b2, b3, b4, b5}{Exponent parameters controlling stem shape in the variable-exponent formulation.}
#'   \item{N}{Number of trees used to fit the model for the given species.}
#'   \item{RSS}{Residual sum of squares from the nonlinear model fit.}
#' }
#'
#' @details
#' These parameters are valid for Saskatchewan conditions and should not be
#' applied outside the region without caution.
#'
#' @source
#' Gal, J., & Bella, I.E. (1994). \emph{New stem taper functions for 12 Saskatchewan
#' timber species}. Natural Resources Canada, Canadian Forest Service, Northwest
#' Region, Information Report NOR-X-338. Table 5.
#'
#' @references
#' Kozak, A. (1988). A variable-exponent taper equation.
#' \emph{Canadian Journal of Forest Research}, 18, 1363–1368.
"parameters_GalBella94"


#' Taper model parameters for Manitoba tree species
#'
#' #' Species-specific parameter estimates for the Kozak (1988) variable-exponent
#' taper model, as fitted by Klos et al. (2007) for Manitoba forests.
#'
#' @references
#' Klos, R. J., Wang, G. G., Dang, Q.-L., & East, E. W. (2007).
#' \emph{Taper equations for five major commercial tree species in Manitoba, Canada}.
#' Western Journal of Applied Forestry, 22(3), 163–170.
#'
#' @format A tibble with 16 rows:
#' \describe{
#'   \item{Species}{Species code following the Canadian National Forest Inventory (NFI) convention (e.g., \code{PICE.GLA}).}
#'   \item{Subregion}{Optional Ecozone}
#'   \item{a0, a1, a2}{Scaling and conditioning parameters of the K2 taper model.}
#'   \item{b1, b2, b3, b4, b5}{Exponent parameters controlling stem shape in the variable-exponent formulation.}
#' }
"parameters_Klos2007"


#' Sharma (2021) tree volume model parameters
#'
#' Species-specific parameters for the dimensionally compatible tree volume
#' equations developed by Sharma (2021) for major commercial tree species
#' in central and eastern Canada and the northeastern United States.
#'
#' The model has the following form:
#' \deqn{
#' V = \alpha + \beta D^{\gamma} H^{3-\gamma}
#' }
#' where \eqn{V} is tree volume (m\eqn{^3}),
#' \eqn{D} is diameter at breast height (DBH, m),
#' and \eqn{H} is total tree height (m).
#'
#' Separate parameter sets are provided for:
#' \itemize{
#'   \item total volume, inside bark
#'   \item total volume, outside bark
#'   \item merchantable volume (inside bark)
#' }
#'
#' Merchantable volume parameters correspond to a fixed merchantability
#' definition used by Sharma (2021): stump height of 0.3 m and a 7 cm
#' inside-bark top diameter.
#'
#' \strong{Cedar species note:}
#' Sharma (2021) reports parameters for a pooled group labelled
#' \emph{"Cedar spp."}, derived from the Stem Data Management System (SDMS).
#' Because this group is not taxonomically explicit, the pooled cedar
#' parameters were reassigned in this dataset to
#' \code{JUNI.VIR} (eastern red cedar), which occurs in central and eastern
#' Canada. The parameters were not refitted; this reassignment represents
#' a direct substitution from the pooled cedar group and should be interpreted
#' accordingly.
#'
#' @format A tibble with the following columns:
#' \describe{
#'   \item{Species}{NFI species code}
#'   \item{volume_type}{Included are paramaters for the merchantable inside bark, total inside bark, and total outside bark volume}
#'   \item{alpha}{Intercept parameter}
#'   \item{beta}{Scale parameter}
#'   \item{gamma}{Exponent for DBH (height exponent is \eqn{3 - \gamma})}
#' }
#'
#' @source
#' Sharma, M. (2021). \emph{Total and Merchantable Volume Equations for 25
#' Commercial Tree Species Grown in Canada and the Northeastern United States}.
#' Forests, 12, 1270.
"parameters_Sharma2021"


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


#' Ecozones of Canada and respective codes
#'
#' A dataset containing a list of Canada's ecozones and their codes.
#'
#'
#' @format A data frame with three variables:
#' \describe{
#'   \item{EcozoneCode}{Ecozone code}
#'   \item{EcozoneName}{Ecozone name}
#'   \item{EcozoneNom}{Ecozone name in french}
#' }
#' @references
#' https://open.canada.ca/data/en/dataset/7ad7ea01-eb23-4824-bccc-66adb7c5bdf8
"CodesEcozones"
