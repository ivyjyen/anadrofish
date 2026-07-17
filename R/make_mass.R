#' @title Mass by age in American eel.
#'
#' @description Function used to simulate Mass by age for American eel.
#'
#' @param species Species for which population dynamics will be simulated.
#' Choice is American eel (\code{"EEL"}).
#'
#' @param custom_habitat A dataframe containing columns corresponding to the
#' those in the output from \code{\link{custom_habitat_template}}. The default,
#' \code{NULL} uses the default habitat data set for a given combination of
#' \code{species} and \code{river}.
#'
#' @section Details:
#' The default method for American eel uses a stochastic approach to draw 
#' allometric growth parameters from \code{lw_pars_eel}.
#'
#' @return A vector containing age-specific mass (g) \code{length = max_age}.
#'
#' @references Atlantic States Marine Fisheries Commission. 2024. American eel
#' benchmark stock assessment and peer-review report. ASMFC, Arlington, VA.
#' URL: Table 2.
#'
#'
#' @export
#'
make_mass <- function(river, species = "EEL", length,
                        custom_habitat = NULL) {

  # Get region
  region <- get_region(
    river = river, species = species,
    custom_habitat = custom_habitat
  )
  
  # Get length-weight regression parameters
  alpha <- unlist(anadrofish::lw_pars_eel[
    anadrofish::lw_pars_eel$Region == region,
    c("alpha", "alpha.se")
  ])
  
  beta <- unlist(anadrofish::lw_pars_eel[
    anadrofish::lw_pars_eel$Region == region,
    c("beta", "beta.se")
  ])
  
  alpha <- rnorm(1, alpha[1], alpha[2])
  beta <- rnorm(1, beta[1], beta[2])
  
  # Predict mass (g) from tl (mm)
  mass <- alpha * length^beta
  
  return(mass)
}
