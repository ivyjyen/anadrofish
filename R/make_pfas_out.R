#' @title Make PFAS out
#'
#' @description Function used to simulate total PFAS transported out of freshwater.
#'
#' @param age0_down Number of outmigrating juveniles
#'
#' @return Total PFAS, here PFOS, in nanograms transported by outmigrating juveniles from freshwater to ocean.
#' Numeric vector is a single value (vector of length 1).
#' Simulated concentrations per individual from Xia et al. 2024. Resident adult alewife in Lake Michigan from 1994.
#'
#' @example inst/examples/make_pfas_out_ex.R
#'
#' @export
#'
make_pfas_out <- function(age0_down) {
    
  # Simulate PFAS concentration for out-migrating individuals and compute total

  # simulated_conc <- rnorm( # Unity empirical data
  #   n = round(age0_down),
  #   mean = 16.88,
  #   sd = 0.28
  # )

  simulated_conc <- rlnorm( # log distribution (Xia et al. 2024)
    n = round(age0_down),
    meanlog = log(15),
    sdlog = log(2)
  )

    # simulated_conc <- rtrunc_norm( # normal distribution (Xia et al. 2024)
    #     n    = round(age0_down),
    #     a    = 0.05,   # lower bound
    #     b    = 50,     # upper bound
    #     mean = 14.259, # Lake Michigan resident alewife ww ng/g
    #     sd   = 5
    # )
    
    sum(simulated_conc * 3.5)
}
