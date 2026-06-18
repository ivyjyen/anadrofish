#' @title Make biomass in
#'
#' @description Function used to calculate biomass transported into freshwater.
#'
#' @param spawners The number of spawner recruits.
#' 
#' @param spawners_down The number of spawners that successfully spawned in
#' freshwater and returned to the ocean.
#'
#' @return Biomass in grams of unsuccessful spawners. Numeric vector 
#' of \code{length(eggs}.
#'
#' @example inst/examples/make_biomass_in_ex.R
#'
#' @export
#'
make_biomass_in <- function(spawners, spawners_down, mass) {
    
    # Subtract successful spawners that returned from spawning population
    spawners_in <- spawners - spawners_down
    
    # multiply by age-specific weight
    biomass_in <- spawners_in * mass
    
    # Return the result to R
    return(biomass_in)
}
