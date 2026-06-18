#' @title Make biomass out
#'
#' @description Function used to calculate biomass transported out of freshwater.
#'
#' @param age0_down The number of successful juvenile outmigrants.
#'
#' @return Biomass in grams of successful juvenile outmigrants. Numeric vector 
#' is a single value (vector of length 1).
#'
#' @example inst/examples/make_biomass_out_ex.R
#'
#' @export
#'
make_biomass_out <- function(age0_down) {
    # Multiply juveniles by 3.5 g
    biomass_out <- age0_down * 3.5
    
    # Return the result to R
    return(biomass_out)
}
