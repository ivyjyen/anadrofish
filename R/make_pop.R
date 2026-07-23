#' @title Make population
#'
#' @description Function used to seed initial age-structured
#' population for simulation based on life-history
#' characteristics. Uses an initial population seed, mortality
#' estimates, and maximum age to create a starting population.
#'
#' @param species Species for which population dynamics will be simulated.
#' Choices include American shad (\code{"AMS"}), alewife (\code{"ALE"}), 
#' blueback herring (\code{"BBH"}), and American eel (\code{"EEL"}).
#'
#' @param max_age The maximum age of fish in the population(s).
#' A numeric vector of length 1.
#'
#' @param nM Instantaneous natural mortality rate.
#' A numeric vector of length for (\code{"AMS"} or a vector of
#' length \code{max_age} for \code{"ALE"}, \code{"BBH"}, and (\code{"EEL"}).
#'
#' @param fM Instantaneous fishing mortality rate.
#' A numeric vector of length 1.
#'
#' @param n_init Initial population abundance (includes all age classes).
#'
#' @return A vector containing age-specific abundances
#' with \code{length = max_age}.
#'
#' @example inst/examples/makepop_ex.R
#'
#' @export
#'
make_pop <- function(river, species, max_age, nM, fM, n_init) {
  # Calculate total mortality
  Z <- nM + fM

  # Survival rate
  # Annual mortality rate (A) = 1-exp(-Z)
  # s = 1 - A
  if (species == "AMS") {
    s <- rep(1 - (1 - exp(-Z)), max_age)
    s[1] <- (1 - (1 - exp(-nM)))^4
    
    # Multiply by an arbitrarily large
    # number to get a population
    pop <- n_init * cumprod(s)
  }

  if (species %in% c("ALE", "BBH")) {
    s <- 1 - (1 - exp(-Z))
    
    # Multiply by an arbitrarily large
    # number to get a population
    pop <- n_init * cumprod(s)
  }
  
  if (species == "EEL") {
    s <- 1 - (1 - exp(-Z))
    
    # Multiply by an arbitrarily large
    # number to get a population
    pop <- sum(n_init * cumprod(s))
    
    distribute_normal <- function(pop, n, mean = NULL, sd = NULL) {
      if (is.null(mean)) mean <- (n + 1) / 2   # center of bins
      if (is.null(sd)) sd <- n / 6              # reasonable spread
      
      x <- 1:n
      weights <- dnorm(x, mean = mean, sd = sd)
      weights <- weights / sum(weights)         # normalize to sum to 1
      
      pop * weights
    }
    
    # Distribute 250000 across max years
    pop <- distribute_normal(pop, max_age)
    
    # # n reaches
    # reaches <- nrow(habitat_eel[habitat_eel$River_huc == river,])
    # 
    # # Create a matrix
    # pop <- matrix(rep(pop / reaches, times = reaches), nrow = reaches, byrow = TRUE)
  }

  # Return the result to R
  return(pop)
}
