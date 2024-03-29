% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{parameters_V2B}
\alias{parameters_V2B}
\title{Model parameters required for model based Volume-to-biomass conversion}
\format{
An object of class \code{list} of length 7.
}
\usage{
parameters_V2B
}
\description{
A dataset containing parameters for the Boudewyn et al 2007 volume-to-biomass conversion models. An updated version
of the parameters was downloaded from https://nfi.nfis.org/en/biomass_models.
}
\details{
' @format A list of four data frames corresponding to Tables 3-6:
\describe{
  \item{V2B_params_t3}{Table 3 in Boudewyn et al. 2007 - Stem wood biomass model parameters for merchantable-sized trees by jurisdiction, ecozone and lead species}
  \item{V2B_params_t4}{Table 4 in Boudewyn et al. 2007 - Stem wood biomass model parameters for nonmerchantable-sized trees by jurisdiction, ecozone and lead species}
  \item{V2B_params_t5}{Table 5 in Boudewyn et al. 2007 - Stem wood biomass model parameters for sapling-sized trees by jurisdiction, ecozone and predominant genus}
  \item{V2B_params_t6}{Table 6 in Boudewyn et al. 2007 - Proportion model parameters by jurisdiction, ecozone and lead species}
  \item{V2B_params_t7}{Table 7 in Boudewyn et al. 2007 - Caps on proportion models by jurisdiction, ecozone and lead species}
}
}
\references{
Boudewyn, P.A.; Song, X.; Magnussen, S.; Gillis, M.D. (2007). Model-based, volume-to-biomass conversion for forested and vegetated land in Canada. Natural Resources Canada, Canadian Forest Service, Pacific Forestry Centre, Victoria, BC. Information Report BC-X-411. 112 p.
}
\keyword{datasets}
