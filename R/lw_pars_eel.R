#' @title Length-weight regression coefficients for American eel
#'
#' @description A dataset containing regression coefficients of 
#' log10length-log10weight relationships for each regional reporting group of 
#' American eel.
#'
#' @format A data frame with 9 observations of 7 variables:
#' \describe{
#'     \item{\code{Region}}{Regional reporting group}
#'     \item{\code{Sex}}{Fish sex: \code{"Female"}, \code{"Male"}, or \code{"Pooled"}}
#'     \item{\code{alpha}}{Intercept}
#'     \item{\code{alpha.se}}{Standard error for the intercept}
#'     \item{\code{beta}}{Slope}
#'     \item{\code{beta.se}}{Standard error for the slope}
#' }
#'
#' @references Atlantic States Marine Fisheries Commission. 2024. American eel
#' benchmark stock assessment and peer-review report. ASMFC, Arlington, VA.
#' URL: https://asmfc.org/wp-content/uploads/2024/11/AmEelBenchmarkStockAssessment_PeerReviewReport_Aug2023.pdf
#'
#' @source Atlantic States Marine Fisheries Commission
"lw_pars_eel"
