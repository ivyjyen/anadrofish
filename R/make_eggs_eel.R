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
make_eggs_eel <- function(river, species = "EEL",
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
    mass <- alpha * tl^beta
    
    # Sample parameters and use to calculate fecundity at age
    # Barbin & McCleave (1997), Tremblay (2009), Wenner & Musick (1974)
    alpha_fec <- anadrofish::fec_eel[, "alpha"]
    beta_fec <- anadrofish::fec_eel[, "beta"]
    
    eggs <- vector(mode = "list", length = length(alpha_fec))
    
    for (i in 1:length(alpha_fec)) {
        eggs[[i]] <- alpha_fec[i]*tl^beta_fec[i]
    }
    
    eggs <- apply(do.call(rbind, eggs), 2, mean) * sample(1:1, 1, replace = FALSE)

    return(eggs)
}