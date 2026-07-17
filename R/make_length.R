#' @title Length by age in American eel.
#'
#' @description Function used to simulate length by age for American eel.
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
#' von Bertalanffy growth parameters from \code{vbgf_eel}.
#'
#' @return A vector containing age-specific length (mm) \code{length = max_age}.
#'
#' @references Atlantic States Marine Fisheries Commission. 2024. American eel
#' benchmark stock assessment and peer-review report. ASMFC, Arlington, VA.
#' URL: Table 4.
#'
#'
#' @export
#'
make_length <- function(river, species = "EEL",
                        custom_habitat = NULL) {
  # Error handling ----
  # Require species to be specified from vector of choices
  if (!missing(species)) species <- match.arg(species, "EEL")
  
  # River error handling
  if (missing(river)) {
    stop("

    Argument 'river' must be specified.

    To see a list of available rivers, run get_rivers() or specify river name
    in custom_habitat if used.")
  }
  
  if (!river %in% get_rivers(species) & is.null(custom_habitat)) {
    stop("

    Argument 'river' must be one of those included in get_rivers() or in
    custom_habitat if used.

    To see a list of available rivers, run get_rivers()")
  }
  
  # Get region
  region <- get_region(
    river = river, species = species,
    custom_habitat = custom_habitat
  )
  
  # Get maximum age
  max_age <- make_maxage(
    river = river, sex = "female", species = species,
    custom_habitat = custom_habitat
  )
  
  # Get growth params for region
  growth_parms <- anadrofish::vbgf_eel[anadrofish::vbgf_eel$Region == region, ]
  
  Linf <- rtrunc_norm(1, a = 0, b = Inf, mean = growth_parms$linf, sd = growth_parms$linf.se)
  K <- rtrunc_norm(1, a = 0, b = 1, mean = growth_parms$k, sd = growth_parms$k.se)
  t0 <- rnorm(1, mean = growth_parms$t0, sd = growth_parms$t0.se)
  
  # Get sequence of ages
  ages <- seq(1, max_age, 1)
  
  # Predict total length at age
  tl <- Linf * (1 - exp(-K * (ages - t0)))
  
  return(tl)
}