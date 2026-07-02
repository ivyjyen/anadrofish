#' @title Calculate upstream survival given dam passage scenario.
#'
#' @description Function used to create population-level
#' survival through dams during seasonal upstream migration of
#' juveniles.
#'
#' @param river Character string specifying river name.
#'
#' @param species Species for which population dynamics will be simulated.
#' Choices include American eel (\code{"EEL"}).
#'
#' @param upstream Numeric indicating proportional upstream passage
#' through a single dam.
#'
#' @param custom_habitat A dataframe containing columns corresponding to the
#' those in the output from \code{\link{custom_habitat_template}}. The default,
#' \code{NULL} uses the default habitat data set for a given combination of
#' \code{species} and \code{river}.
#'
#' @return Numeric vector of length 1 representing weighted catchment-scale
#' downstream migration mortality for juvenile or adult fish.
#'
#' @details This function assigns cumulative downstream passage values
#' to all features in \code{\link{habitat}} corresponding to \code{river}.
#' It then calculates the proportion of habitat in each habitat segment of a
#' river, and weights downstream mortality at the catchment-scale
#' by proportion of habitat. This implicitly assumes that fish are
#' distributed throughout the river during spawning in proportion to
#' available habitat.
#'
#' @example inst/examples/make_upstream_ex.R
#'
#' @export
#'
make_upstream <- function(river,
                            species = "EEL",
                            upstream,
                            custom_habitat = NULL) {
  # Error handling
  # Species error handling
  if (missing(species)) {
    stop("

    Argument 'species' must be 'EEL'.")
  }
  
  if (!species == "EEL") {
    stop("

    Argument 'species' must be 'EEL'.")
  }
  
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
  
  # Built-in habitat data routines
  if (is.null(custom_habitat)) {

    # Select habitat units based on huc_code
    units <- anadrofish::habitat_eel[anadrofish::habitat_eel$River_huc == river, ]
    
    # Calculate passage to habitat segment
    units$p_upstream <- upstream^units$DamOrder
    
    # Available habitat
    units$functional_upstream <- units$Hab_sqkm * units$p_upstream
    
    # Calculate proportion of habitat in each segment of available
    units$p_habitat <- units$functional_upstream / sum(units$functional_upstream)
    
    # The ratio is survival rate
    s_upstream <- sum(units$p_habitat * ((upstream^units$DamOrder)))
  } 
    
  # Custom habitat routine
  else {
    # Assign custom habitat to units
    units <- custom_habitat
    
    # Calculate passage to habitat segment
    units$p_upstream <- upstream^units$DamOrder
    
    # Available habitat
    units$functional_upstream <- units$Hab_sqkm * units$p_upstream
    
    # Calculate proportion of habitat in each segment of available
    units$p_habitat <- units$functional_upstream / sum(units$functional_upstream)
    
    # The ratio is survival rate
    s_upstream <- sum(units$p_habitat * ((upstream^units$DamOrder)))
  }
  
  return(s_upstream)
}
