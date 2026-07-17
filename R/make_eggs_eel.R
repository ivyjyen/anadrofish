#' @title Simulate number of eggs per fish for American eel
#'
#' @description Function used to simulate number of eggs
#' produced per female (total fecundity) for American eel.
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
#' The default method for American eel uses a stochastic, MCMC sampling 
#' approach to draw correlated sets of von Bertalanffy growth
#' parameters in \code{vbgf_eel}, length-weight regression parameters in 
#' \code{link{lw_pars_eel}}, and length-fecundity relationships from ASFMC 
#' (2011), Barbin & McCleave (1997), Tremblay (2009), and Wenner & Musick (1974)
#' to simulate the number of eggs per female within regional reporting groups
#' (\code{region}).
#'
#' @return A vector containing age-specific potential annual
#' fecundity with \code{length = max_age}.
#'
#' @examples make_eggs_rh(river = "Susquehanna", species = "BBH")
#'
#' @references Atlantic States Marine Fisheries Commission. 2024. American eel
#' benchmark stock assessment and peer-review report. ASMFC, Arlington, VA.
#' URL: 
#'
#'
#' @export
#'
make_eggs_eel <- function(river, species = "EEL", length,
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
    
    # Sample parameters and use to calculate fecundity at age
    # Barbin & McCleave (1997), Tremblay (2009), Wenner & Musick (1974)
    alpha_fec <- anadrofish::fec_eel[fec_eel$region == region, "alpha"]
    beta_fec <- anadrofish::fec_eel[fec_eel$region == region, "beta"]
    
    eggs <- alpha_fec * length^beta_fec

    return(eggs)
}