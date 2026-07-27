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
make_output_row <- function(.sim_pop, species = c("AMS", "ALE", "BBH", "EEL"), sex_specific = FALSE) {
  
  if (species %in% c("AMS", "ALE", "BBH")) {
    row <- list(
      river        = .sim_pop$river,
      region       = .sim_pop$region,
      govt         = .sim_pop$govt,
      lat          = .sim_pop$latitude,
      habitat      = .sim_pop$acres,
      year         = .sim_pop$t,
      upstream     = if (length(.sim_pop$upstream) > 1)
        "dam specific: check your scenarios" else as.character(.sim_pop$upstream),
      downstream   = if (length(.sim_pop$downstream) > 1)
        "dam specific: check your scenarios" else as.character(.sim_pop$downstream),
      downstream_j = if (length(.sim_pop$downstream_j) > 1)
        "dam specific: check your scenarios" else as.character(.sim_pop$downstream_j),
      fM           = mean(.sim_pop$fM[.sim_pop$fM != 0], na.rm = TRUE),
      n_init       = .sim_pop$n_init,
      sr           = .sim_pop$sr,
      s_juvenile   = .sim_pop$s_juvenile,
      iteroparity  = .sim_pop$iteroparity,
      spawners     = list(.sim_pop$spawners), # spawners entering fw to spawn, spawners2 = spawners who have survived after spawning
      spawners_down = list(.sim_pop$spawners_down), # spawners who have successfully spawned and outmigrated past dams
      #pop_before_nM = sum(.sim_pop$pop_down)+.sim_pop$age0_down,
      #pop_down         = list(.sim_pop$pop_down),
      pop          = list(.sim_pop$pop), # after project pop (after inst mortality)
      juveniles_out = .sim_pop$age0_down,
      juveniles_before_outmigrating = .sim_pop$age0,
      larvae       = sum(.sim_pop$recruits_f_age)
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
  }
  
  if (species == "EEL") {
    row <- list(
      river        = .sim_pop$river,
      region       = .sim_pop$region,
      govt         = .sim_pop$govt,
      lat          = .sim_pop$latitude,
      habitat      = .sim_pop$acres,
      year         = .sim_pop$t,
      upstream     = if (length(.sim_pop$upstream) > 1)
        "dam specific: check your scenarios" else as.character(.sim_pop$upstream),
      downstream   = if (length(.sim_pop$downstream) > 1)
        "dam specific: check your scenarios" else as.character(.sim_pop$downstream),
      n_init       = .sim_pop$n_init,
      sr           = .sim_pop$sr,
      s_juvenile   = .sim_pop$s_juvenile,
      spawners     = as.list(unname(colSums(.sim_pop$spawners))), # spawners entering fw to spawn, spawners2 = spawners who have survived after spawning
      #pop_before_nM = sum(.sim_pop$pop_down)+.sim_pop$age0_down,
      #pop_down         = list(.sim_pop$pop_down),
      pop          = as.list(unname(colSums(.sim_pop$pop))), # after project pop (after inst mortality)
      juveniles_up = sum(.sim_pop$age0_up_reach),
      beta         = .sim_pop$b
      # juveniles_before_up = .sim_pop$age0,
      # larvae       = sum(.sim_pop$recruits_f_age)
    )
    
    if (sex_specific) {
      row$max_age_m      <- .sim_pop$max_age_m
      row$max_age_f      <- .sim_pop$max_age_f
      row$nM_m           <- mean(.sim_pop$nM_m, na.rm = TRUE)
      row$nM_f           <- mean(.sim_pop$nM_f, na.rm = TRUE)
    } else {
      row$max_age      <- .sim_pop$max_age
      row$nM           <- mean(.sim_pop$nM, na.rm = TRUE)
    }
  
  }
  
  row
}
