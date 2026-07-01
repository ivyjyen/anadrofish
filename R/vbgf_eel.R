#' @title Sex-specific von Bertalanffy growth parameters for American eel
#'
#' @description A dataset containing estimates of von Bertalanffy
#' growth parameters for American eel by region from the 2024 ASMFC American
#' eel benchmark stock assessment (ASMFC 2024).
#'
#' @format A data frame with __ observations of 5 variables:
#' \describe{
#'     \item{\code{Region}}{Regional reporting group}
#'     \item{\code{Sex}}{Sex of fish}
#'     \item{\code{linf}}{Asymptotic length of fish}
#'     \item{\code{linf.sd}}{Asymptotic length of fish standard error}
#'     \item{\code{k}}{Brody growth coefficient mean}
#'     \item{\code{k.sd}}{Brody growth coefficient standard error}
#'     \item{\code{t0}}{Size at age zero}
#'     \item{\code{t0.sd}}{Size at age zero standard error}
#' }
#'
#' @references Atlantic States Marine Fisheries Commission. 2024. American Eel
#' benchmark stock assessment and peer-review report. ASMFC, Arlington, VA.
#' URL: https://asmfc.org/wp-content/uploads/2024/11/AmEelBenchmarkStockAssessment_PeerReviewReport_Aug2023.pdf
#'
#' @source Atlantic States Marine Fisheries Commission
"vbgf_eel"