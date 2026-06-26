#' @title Make one row of simulation output
#'
#' @description Internal function used to capture all output fields from
#' \code{.sim_pop} for a single year. Called inside the simulation loop
#' in \code{\link{sim_pop}}.
#'
#' @param .sim_pop The simulation environment.
#'
#' @param sex_specific Logical indicating whether to use sex-specific output.
#'
#' @return A named list representing one row of output.
#'
#' @keywords Internal
#'
make_output_row <- function(.sim_pop, sex_specific = FALSE) {
  row <- list(
    river          = .sim_pop$river,
    region         = .sim_pop$region,
    govt           = .sim_pop$govt,
    lat            = .sim_pop$latitude,
    habitat        = .sim_pop$acres / 247.105,
    year           = .sim_pop$t,
    upstream       = if (length(.sim_pop$upstream) > 1)
      "dam specific: check your scenarios" else as.character(.sim_pop$upstream),
    downstream     = if (length(.sim_pop$downstream) > 1)
      "dam specific: check your scenarios" else as.character(.sim_pop$downstream),
    downstream_j   = if (length(.sim_pop$downstream_j) > 1)
      "dam specific: check your scenarios" else as.character(.sim_pop$downstream_j),
    fM             = mean(.sim_pop$fM[.sim_pop$fM != 0], na.rm = TRUE),
    n_init         = .sim_pop$n_init,
    sr             = .sim_pop$sr,
    s_juvenile     = .sim_pop$s_juvenile,
    iteroparity    = .sim_pop$iteroparity,
    spawners       = list(.sim_pop$spawners), # spawners entering fw to spawn, spawners2 = spawners who have survived after spawning
    spawners_down  = list(.sim_pop$spawners_down), # spawners who have successfully spawned and outmigrated past dams
    pop_before_nM  = sum(.sim_pop$pop_down)+.sim_pop$age0_down,
    #pop_down       = list(.sim_pop$pop_down),
    pop            = list(.sim_pop$pop), # after project pop (after inst mortality)
    larvae         = sum(.sim_pop$recruits_f_age),
    juveniles      = .sim_pop$age0,
    juveniles_down = .sim_pop$age0_down,
    biomass_in     = sum(.sim_pop$biomass_in), # units = g
    biomass_in2    = sum(.sim_pop$biomass_in2), # units = g
    biomass_out    = .sim_pop$biomass_out,
    pfas_in        = sum(.sim_pop$pfas_in), # units = ng
    pfas_in2       = sum(.sim_pop$pfas_in2), # units = ng
    pfas_out       = .sim_pop$pfas_out
  )

  if (sex_specific) {
    row$max_age_m      <- .sim_pop$max_age_m
    row$max_age_f      <- .sim_pop$max_age_f
    row$nM_m           <- mean(.sim_pop$nM_m, na.rm = TRUE)
    row$nM_f           <- mean(.sim_pop$nM_f, na.rm = TRUE)
    row$s_spawn_m      <- mean(.sim_pop$s_spawn_m, na.rm = TRUE)
    row$s_spawn_f      <- mean(.sim_pop$s_spawn_f, na.rm = TRUE)
    row$s_postspawn_m  <- mean(.sim_pop$s_postspawn_m, na.rm = TRUE)
    row$s_postspawn_f  <- mean(.sim_pop$s_postspawn_f, na.rm = TRUE)
  } else {
    row$max_age      <- .sim_pop$max_age
    row$nM           <- mean(.sim_pop$nM, na.rm = TRUE)
    row$s_spawn      <- mean(.sim_pop$s_spawn, na.rm = TRUE)
    row$s_postspawn  <- mean(.sim_pop$s_postspawn, na.rm = TRUE)
  }

  row
}
