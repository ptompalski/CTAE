#' Ung2009 Growth and Yield Model
#'
#' This function implements the models described in Ung et al. (2009).  
#' Two models (Nat1 and Nat2) are available:
#'   \itemize{
#'     \item{\strong{Nat1:} A multi-equation system predicting tree height (H), basal area (BA), 
#'           and volume (V) based on stand age, growth degree days (GDD), precipitation (PREC), 
#'           and species-specific parameters.}
#'     \item{\strong{Nat2:} A single equation that focuses on predicting stand volume (V) 
#'           using age, GDD, and PREC.}
#'   }
#'  
#' @details 
#' In the Nat1 model, height and basal area are each modeled using age, GDD, and PREC in a 
#' Chapman-Richards formulation, and volume is derived via an allometric equation. In contrast, 
#' Nat2 is more streamlined, modeling volume directly as a function of these same variables 
#' but without explicit height and basal area components.
#'
#' @param species Character. A valid species name as defined in the parameter table of the model.
#' @param age Numeric vector. The age(s) of the stand in years. Must be greater than 0.
#' @param GDD Numeric. Growth degree days.
#' @param PREC Numeric. Precipitation.
#' @param model Character. Which model(s) to use. One of \code{"Nat1"}, \code{"Nat2"}, or 
#'   \code{"both"}. Defaults to \code{"both"}.
#'
#' @return A tibble containing:
#' \itemize{
#'   \item{\code{age}} 
#'   \item{\code{H}} (if \code{Nat1} is used): Predicted tree height (m).
#'   \item{\code{BA}} (if \code{Nat1} is used): Predicted basal area (m²/ha).
#'   \item{\code{V}} (if \code{Nat1} is used): Predicted volume (m³/ha).
#'   \item{\code{V2}} (if \code{Nat2} is used): Predicted volume (m³/ha) based on the Nat2 model.
#' }
#'
#' @references 
#' Ung, C.-H., Bernier, P.Y., Guo, X.J., Lambert, M.-C. (2009). 
#' "A simple growth and yield model for assessing changes in standing volume across Canada’s forests." 
#' The Forestry Chronicle, 85, 57–64. \doi{10.5558/tfc85057-1}.
#'
#' @examples
#' # Example 1: Use both models (default), returning columns: age, H, BA, V, V2
#' res_both <- Ung2009(
#'   species = "ABIE.BAL",
#'   age = 1:150,
#'   GDD = 1500,
#'   PREC = 800
#' )
#' res_both
#'
#' # Example 2: Use only the Nat1 model, returning columns: age, H, BA, V
#' res_nat1 <- Ung2009(
#'   species = "ABIE.BAL",
#'   age = 1:150,
#'   GDD = 1500,
#'   PREC = 800,
#'   model = "Nat1"
#' )
#' res_nat1
#'
#' # Example 3: Use only the Nat2 model, returning columns: age, V2
#' res_nat2 <- Ung2009(
#'   species = "ABIE.BAL",
#'   age = 1:150,
#'   GDD = 1500,
#'   PREC = 800,
#'   model = "Nat2"
#' )
#' res_nat2
#'
#' @export
Ung2009 <- function(species,
                    age,
                    GDD,
                    PREC,
                    model = c("both", "Nat1", "Nat2")) {
  
  model <- match.arg(model)
  
  # Validate inputs
  if (any(age <= 0)) stop("Age must be greater than 0.")
  if (!is.numeric(GDD) || !is.numeric(PREC)) stop("GDD and PREC must be numeric.")
  if (!is.character(species)) stop("Species must be a character string.")
  
  # parameters for the Nat1 model
  P <- tibble::tribble(
    ~Species,~h10,~h11,~h12,~h20,~h21,~h22,~g10,~g11,~g12,~g20,~g21,~g22,~v30,~v31,~v32,~CDh,~CDg,~CDv,
    "ABIE.BAL",3.8398,0,-0.0011,-48.245,0.0071,0.0202,5.6744,-0.0003,-0.0015,-159.3,0.0209,0.0699,-0.4283,0.8801,0.9799,1.0263,1.2987,1.0013,
    "ABIE.LAS",2.8996,-0.0003,0.0006,0,0,-0.0311,3.2673,0,0,128.5,-0.1014,0,-0.8345,1.0402,0.9469,1.0201,1.2211,1.0016,
    "ACER.RUB",3.8692,-0.0001,-0.0007,-15.8623,0,0.0107,6.0712,-0.0008,-0.0007,-114.2,0.0327,0.0148,-0.4569,0.8327,1.0173,1.0165,1.1783,1.0002,
    "ACER.SAC",4.3286,-0.0002,-0.0007,-56.8861,0.0133,0.0161,5.5923,-0.0009,0,-206.9,0.0598,0.0346,-0.5308,0.8696,1.0061,1.0142,1.1412,1.0002,
    "BETU.ALL",3.9764,-0.0002,-0.0006,-20.5686,0,0.0128,2.761,0.0008,-0.0013,0,-0.0459,0.0772,-0.4278,0.8444,1.0026,1.0123,1.149,1.0001,
    "BETU.PAP",3.1209,0,-0.0004,0,-0.0014,0,1.7647,0.0005,0,0,-0.006,0,-0.3674,0.8293,1.0001,1.0197,1.3192,1.0005,
    "FAGU.GRA",3.4669,0,-0.0005,0,-0.0034,0,0,0.0013,0,199.7,-0.0837,0,-0.5001,0.8659,1.0149,1.0143,1.1061,1.0007,
    "LARI.LAR",2.3952,0.0003,-0.0005,-11.8642,0,0.0065,-2.2655,0.002,0,0,-0.0091,0,-2.0492,1.6383,0.9169,1.0285,1.5867,1.034,
    "LARI.OCC",3.3621,0,0,-25.3806,0,0,3.0823,-0.0004,0.0032,0,0.0275,-0.1762,0,0.5337,1.1357,1.0136,1.0351,1.0024,
    "PICE.ENG",2.9666,0.0002,0,-41.8295,0,0,4.3843,0,0,0,-0.0491,0,-0.9557,1.0098,1.0134,1.0227,1.2133,1.0009,
    "PICE.GLA",3.2311,0.0001,-0.0007,-25.3362,0,0.0142,2.7291,0.001,-0.0016,-53.1073,0,0.0279,-0.2677,0.6682,1.1088,1.0285,1.1943,1.0049,
    "PICE.MAR",2.6671,0.0001,-0.0002,-12.933,0.0041,-0.0085,2.5525,0.0006,-0.0004,-48.7111,0,0,-0.4978,0.8925,0.9986,1.0231,1.3169,1.0013,
    "PICE.RUB",5.3239,-0.0005,-0.001,-121.5,0.0219,0.04,7.0614,-0.0006,-0.0015,-183.5,0.0309,0.0541,0,0.6346,1.0573,1.0151,1.1525,1.0015,
    "PINU.BAN",1.8648,0.0006,-0.0003,10.5878,-0.0142,0.0098,0,0.0024,-0.0018,-20.337,-0.0395,0.0768,-0.1302,0.6996,1.0526,1.022,1.2658,1.0013,
    "PINU.CON",2.4225,0.0003,0.0003,-23.5784,0,0,3.8904,0,0.0008,-114.1,0.0115,0.0406,-0.5614,0.8799,1.0386,1.0198,1.1372,1.0024,
    "PINU.RES",5.2084,-0.001,0.0002,-73.9606,0.0225,0,9.0269,-0.0019,0,-219.1,0.0818,-0.0396,-0.8252,1.0681,0.9523,1.0235,1.2422,1.0007,
    "PINU.STR",3.6756,0,-0.0008,-53.8046,0.0187,0,4.0591,0,-0.0007,-209.9,0.0735,0,-0.5129,0.9098,0.9867,1.0421,1.3091,1.0005,
    "POPU.BAL",3.8384,-0.0003,0,0,-0.0063,0,0,0.0019,0,182,-0.1069,0,-0.3614,0.7289,1.072,1.0124,1.167,1.0037,
    "POPU.GRA",3.3093,0,-0.0003,-9.5398,0,0,4.0044,0,0,-42.5505,0,0,-0.6908,0.9292,1.0129,1.0117,1.2014,1.0002,
    "POPU.TRE",3.0661,0.0002,-0.0005,0,-0.0114,0.0153,4.431,0,-0.0006,-77.2433,0.0102,0.012,-0.6823,0.9262,1.0192,1.018,1.1195,1.0011,
    "PSEU.MEN",2.8286,0,0.0006,-44.9718,0.0143,-0.0181,2.8812,0,0.0013,-88.9565,0.034,-0.0458,0.1808,0.1188,1.4372,1.0267,1.1281,1.0202,
    "QUER.RUB",4.0259,0,-0.0009,-75.0032,0.0221,0,3.8256,0,0,-45.2706,0,0,-0.4158,0.8569,0.9904,1.0096,1.085,1.0001,
    "THUJ.OCC",4.1218,-0.0006,0,-52.7774,0.0185,0,5.4412,0,-0.0019,-98.0635,0.0323,0,0,0.5696,1.0937,1.0146,1.2254,1.0054,
    "TSUG.CAN",2.8478,0,0,61.7319,-0.0196,-0.0132,14.5555,-0.004,0,-757,0.2647,0,-0.4276,0.8547,0.9996,1.0172,1.2047,1.0001,
    "TSUG.HET",2.4353,0.0003,0.0003,-17.9089,0,-0.0067,5.7049,-0.0004,0,-154.5,0.0292,0.0188,-0.7764,0.8928,1.0524,1.0153,1.0572,1.0012
  )
  
  # parameters for the Nat2 model
  P2 <- tibble::tribble(
    ~Species,~v10,~v11,~v12,~v20,~v21,~v22,~Cd,
    "ABIE.BAL",6.9831,0,-0.0011,-165.647,0.0254,0.0477,1.1042,
    "ABIE.LAS",5.1561,0,0,269.199,-0.1994,0,1.3015,
    "ACER.RUB",6.8511,0,-0.0012,-93.0483,0,0.051,1.0619,
    "ACER.SAC",6.3001,0,-0.0007,-76.15,0,0.0441,1.0331,
    "BETU.ALL",7.4254,0,-0.0017,-144.749,0,0.0937,1.0783,
    "BETU.PAP",5.4297,0.0007,-0.0017,-79.2512,0,0.0387,1.0986,
    "FAGU.GRA",5.3354,0,0,-18.0541,0,0,1.0491,
    "LARI.LAR",0.3964,0.0023,-0.0013,0,-0.0096,0,1.2925,
    "LARI.OCC",8.4027,-0.0011,0.001,-279.372,0.0944,0,1.0473,
    "PICE.ENG",4.0283,0.0015,0,0,-0.1009,0.0815,1.1507,
    "PICE.GLA",4.3751,0.0013,-0.0016,0,-0.0319,0.0413,1.0923,
    "PICE.MAR",4.6774,0.0005,-0.0005,-60.224,0.0118,-0.0236,1.1481,
    "PICE.RUB",8.2368,-0.0009,0,-163.794,0.0452,0,1.0446,
    "PINU.BAN",5.3141,0.0007,-0.0012,-139.856,0.0214,0.0417,1.1136,
    "PINU.CON",4.1498,0.0007,0.0012,0,-0.0257,0,1.0818,
    "PINU.RES",7.8984,0,-0.0019,0,-0.0457,0.075,1.1238,
    "PINU.STR",11.8506,-0.0022,0,-596.185,0.2028,0,1.1342,
    "POPU.BAL",6.2604,0,0,0,-0.0216,0,1.0094,
    "POPU.GRA",9.5598,0,-0.0028,-219.535,0,0.1366,1.1075,
    "POPU.TRE",6.7296,0,-0.0005,-86.6405,0.0135,0,1.0333,
    "PSEU.MEN",4.2494,0,0.0022,-44.9884,0.0337,-0.0913,1.086,
    "QUER.RUB",5.824,0,0,-58.3159,0,0,1.1044,
    "THUJ.OCC",2.8399,0.0012,0,0,-0.0236,0,1.0548,
    "TSUG.CAN",4.6304,0,0.001,0,0,-0.0263,1.0566,
    "TSUG.HET",6.8699,0,0.0003,-143.964,0.0294,0,1.0154
  )
  
  # Look up species parameters
  param <- P[P$Species == species,]
  param2 <- P2[P2$Species == species,]
  if (nrow(param) == 0 || nrow(param2) == 0) {
    stop("Invalid species. Please provide a valid species name.")
  }
  
  # Initialize columns for the output tibble
  # We'll create them only if needed
  out <- tibble::tibble(age = age)
  
  #----------------
  #   Nat1 model
  #----------------
  if (model %in% c("Nat1","both")) {
    lnH <- (param$h10 + (param$h11 * GDD) + (param$h12 * PREC)) + 
      ((param$h20 + (param$h21 * GDD) + (param$h22 * PREC)) / age)
    H <- exp(lnH) * param$CDh
    
    lnBA <- (param$g10 + (param$g11 * GDD) + (param$g12 * PREC)) + 
      ((param$g20 + (param$g21 * GDD) + (param$g22 * PREC)) / age)
    BA <- exp(lnBA) * param$CDg
    
    lnV <- param$v30 + param$v31 * lnH + param$v32 * lnBA
    V  <- exp(lnV) * param$CDv
    
    out$H  <- H
    out$BA <- BA
    out$V  <- V
  }
  
  #----------------
  #   Nat2 model
  #----------------
  if (model %in% c("Nat2","both")) {
    lnV2 <- (param2$v10 + (param2$v11 * GDD) + (param2$v12 * PREC)) +
      ((param2$v20 + (param2$v21 * GDD) + (param2$v22 * PREC)) / age)
    V2 <- exp(lnV2) * param2$Cd
    
    out$V2 <- V2
  }
  
  return(out)
}

