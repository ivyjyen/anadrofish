#' @title Make PFAS in
#'
#' @description Function used to simulate total PFAS transported into freshwater.
#'
#' @param spawners The number of spawner recruits.
#' 
#' @param spawners_down The number of spawners that successfully spawned in
#' freshwater and returned to the ocean.
#' 
#' @param mass Mass at age vector
#'
#' @return Total PFAS, here PFOS, in nanograms offloaded by spawner recruit carcasses into freshwater.
#' Essentially those that die from post-spawning, dam death, and from 2 months of natural mortality.
#' Numeric value of \code{length(spawners)}. Simulated concentrations per individual from Melnyk et al. 2024. Migrating BBH from Penobscot River in 2021.
#' 
#' @example inst/examples/make_pfas_in_ex.R
#'
#' @export
#'
make_pfas_in <- function(spawners, spawners_down, mass) {
    
    # Calculate unsuccessful spawners
    spawners_in <- spawners - spawners_down
    
    # For each age class, simulate PFAS concentration and compute mass-specific total
    age_specific_pfas <- sapply(seq_along(spawners_in), function(i) {
        
        simulated_conc <- rtrunc_norm(
            n    = spawners_in[i],
            a    = 0.05,
            b    = 100,
            mean = 3.94,
            sd   = 2.92
        )
        
        sum(simulated_conc * mass[i])
    })
    
    # Sum across all mass classes
    return(age_specific_pfas)
}
