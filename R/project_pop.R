#' @title Project population
#'
#' @description Function used to project population forward
#' one time step using vector multiplication.
#'
#' @param x A numeric vector containing age-specific
#' abundance at end of year t.
#'
#' @param age0 A numeric vector of length one containing number of new recruits
#' at end of year t.
#'
#' @param nM Numeric vector of instantaneous natural mortality.
#'
#' @param fM Numeric vector of instantaneous fishing mortality.
#'
#' @param max_age Numeric indicating maximum age of spawning fish.
#'
#' @param species Species for which population dynamics will be simulated.
#' Choices include American shad (\code{"AMS"}), alewife (\code{"ALE"}),
#' blueback herring (\code{"BBH"}) and American eel (\code{"EEL"}).
#'
#' @return A numeric vector of age-structured abundance in time t + 1.
#'
#' @example inst/examples/projectpop_ex.R
#'
#' @export
#'
project_pop <- function(x, age0, nM, fM, max_age,
                        species = c("AMS", "ALE", "BBH", "EEL")) {
  # Error handling
  # Species error handling
  if (missing(species)) {
    stop("

    Argument 'species' must be one of 'ALE', 'AMS', 'BBH', or 'EEL'.")
  }

  if (!species %in% c("ALE", "AMS", "BBH", "EEL")) {
    stop("

    Argument 'species' must be one of 'ALE', 'AMS', 'BBH', or 'EEL'.")
  }

  # Calculate total mortality
  Z <- nM + fM

  # Survival rate
  if (species == "AMS") {
    s <- rep(1 - (1 - exp(-Z)), max_age)
    s[1] <- (1 - (1 - exp(-nM)))^3
  }

  if (species %in% c("ALE", "BBH", "EEL")) {
    s <- 1 - (1 - exp(-Z))
  }

  if (species =="EEL") {
    # Shift ages: prepend age0 (recruits) as new column 1, 
    # drop the oldest age class (columns beyond max_age - 1)
    tplus <- cbind(age0, x[, 1:(max_age - 1)])
    
    # Apply age-specific survival to each column
    tplus <- sweep(tplus, MARGIN = 2, s, `*`)
    
    # Truncate to max_age columns (in case of any overflow)
    return(tplus[, 1:max_age])
    
  } else {
    # Project population one time-step and drop
    # any fish > max_age
    tplus <- c(age0, x[1:(max_age - 1)]) * s
    
    # Return the result to R
    return(tplus[1:max_age])
    }
}
