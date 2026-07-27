#' @title Simulate coupled American eel population dynamics across three rivers
#'
#' @description Runs the EEL branch of `sim_pop()` logic simultaneously across
#' three rivers, where each year's raw `age0` (juveniles produced from the
#' Beverton-Holt recruitment step, BEFORE upstream passage survival is
#' applied) is replaced with the mean of `age0` across all three rivers for
#' that year (i.e., `sum(age0_river1, age0_river2, age0_river3) / 3`). Each
#' river then applies its OWN upstream passage survival (`s_upstream_j`) to
#' that shared/averaged age0 value before it is used to project each river's
#' population into the *next* year.
#'
#' This is NOT a wrapper around `sim_pop()` -- it reimplements the EEL
#' year-loop internals so that the three rivers can be stepped through the
#' same year `t` together and their age0 values pooled before projecting
#' population forward. It relies on the same internal helper functions used
#' by `sim_pop()` (`get_region`, `get_govt`, `make_maxage`, `make_mortality`,
#' `make_spawnrecruit`, `make_eggs`, `sim_juvenile_s`, `make_habitat`,
#' `make_downstream`, `make_upstream`, `make_pop`, `make_spawners`,
#' `make_recruits`, `beverton_holt`, `project_pop`, `make_output_row`,
#' `add_unequal_vectors`, `assemble_output`), so it must be defined/sourced
#' within (or alongside) the same package/namespace as `sim_pop()`. If those
#' helpers are not exported from your package, use `pkg:::helper_name()`
#' inside this function, or run this script from within the package's
#' development environment (e.g., via `devtools::load_all()`).
#'
#' @param nyears Number of years for simulation.
#' @param rivers Character vector of EXACTLY 3 river names (must be valid
#'   for species = "EEL", i.e., present in `get_rivers("EEL")` or in
#'   `custom_habitat` if used).
#' @param max_age,nM,fM,n_init,spawnRecruit,eggs,sr,b,s_juvenile,upstream,
#'   downstream,downstream_j,output_years,age_structured_output,sex_specific,
#'   custom_habitat Same meaning as in `sim_pop()`. NOTE: these are currently
#'   applied identically to all three rivers if you override the defaults
#'   (e.g., supplying your own `nM`). River-specific defaults (max age,
#'   mortality, maturity, eggs, habitat, downstream/upstream survival) are
#'   still looked up separately per river when left as `NULL`, exactly as in
#'   `sim_pop()`.
#'
#' @return A single data.frame stacking the per-river outputs (via `rbind`),
#'   in the same column format as `sim_pop()` produces for `species = "EEL"`.
#'
#' @export
sim_pop_3river_eel <- function(
    nyears = 50,
    rivers,
    max_age = NULL,
    nM = NULL,
    fM = 0,
    n_init = runif(1, 10e5, 80e7),
    spawnRecruit = NULL,
    eggs = NULL,
    sr = 0.50,
    b = 0.21904,
    s_juvenile = NULL,
    upstream = 1,
    downstream = 1,
    downstream_j = 1,
    output_years = c("last", "all"),
    age_structured_output = FALSE,
    sex_specific = TRUE,
    custom_habitat = NULL) {
  
  species <- "EEL"
  output_years <- match.arg(output_years)
  
  if (missing(rivers) || length(rivers) != 3) {
    stop("

    `rivers` must be a character vector of exactly 3 river names.")
  }
  
  bad_rivers <- !rivers %in% get_rivers(species) & is.null(custom_habitat)
  if (any(bad_rivers)) {
    stop("

    All values in `rivers` must be included in get_rivers('EEL') or in
    custom_habitat if used. Problem river(s): ",
         paste(rivers[bad_rivers], collapse = ", "))
  }
  
  n_riv <- 3
  envs  <- vector("list", n_riv)   # one hidden environment per river (mirrors .sim_pop)
  rows  <- vector("list", n_riv)   # rows[[i]] holds nyears of output rows for river i
  
  # ---------------------------------------------------------------------
  # Set up each river's starting state (mirrors the front matter of sim_pop)
  # ---------------------------------------------------------------------
  for (i in seq_len(n_riv)) {
    e <- new.env()
    e$river         <- rivers[i]
    e$species       <- species
    e$nyears        <- nyears
    e$fM            <- fM
    e$sr            <- sr
    e$b             <- b
    e$upstream      <- upstream
    e$downstream    <- downstream
    e$downstream_j  <- downstream_j
    e$custom_habitat <- custom_habitat
    e$sex_specific  <- sex_specific
    e$n_init        <- n_init
    
    e$region <- get_region(river = e$river, species = species, custom_habitat = custom_habitat)
    e$govt   <- get_govt(e$river, species, custom_habitat = custom_habitat)
    
    if (sex_specific) {
      
      e$max_age_m <- if (is.null(max_age)) {
        make_maxage(river = e$river, sex = "male", species = species, custom_habitat = custom_habitat)
      } else max_age
      
      e$max_age_f <- if (is.null(max_age)) {
        make_maxage(river = e$river, sex = "female", species = species, custom_habitat = custom_habitat)
      } else max_age
      
      e$length_m <-
        make_length(river = e$river, species = species, custom_habitat = custom_habitat)
      
      e$length_f <-
        make_length(river = e$river, species = species, custom_habitat = custom_habitat)
      
      e$mass_m <-
        make_mass(river = e$river, species = species, length = e$length_m, custom_habitat = custom_habitat)
      
      e$mass_f <-
        make_mass(river = e$river, species = species, length = e$length_f, custom_habitat = custom_habitat)
      
      e$spawnRecruit_m <- if (is.null(spawnRecruit)) {
        make_spawnrecruit_eel(e$river, sex = "male", species = species, length = e$length_m, custom_habitat = custom_habitat)
      } else spawnRecruit
      
      e$spawnRecruit_f <- if (is.null(spawnRecruit)) {
        make_spawnrecruit_eel(e$river, sex = "female", species = species, length = e$length_f, custom_habitat = custom_habitat)
      } else spawnRecruit
    
      e$nM_m <- if (is.null(nM)) {
        make_mortality(river = e$river, sex = "male", species = species,
                       max_age = e$max_age_m, custom_habitat = custom_habitat)
      } else nM
      
      e$nM_f <- if (is.null(nM)) {
        make_mortality(river = e$river, sex = "female", species = species,
                       max_age = e$max_age_f, custom_habitat = custom_habitat)
      } else nM
      
      e$eggs <- if (is.null(eggs)) {
        make_eggs_eel(e$river, species = species, length = e$length_f,
                      custom_habitat = custom_habitat)
      } else eggs
      
    } else {
      
      e$max_age <- if (is.null(max_age)) {
        make_maxage(river = e$river, species = species, custom_habitat = custom_habitat)
      } else max_age
      
      e$length <-
        make_length(river = e$river, species = species, custom_habitat = custom_habitat)

      e$mass <-
        make_mass(river = e$river, species = species, length = e$length, custom_habitat = custom_habitat)

      e$spawnRecruit <- if (is.null(spawnRecruit)) {
        make_spawnrecruit_eel(river = e$river, species = species, length = e$length, custom_habitat = custom_habitat)
      } else spawnRecruit

      e$nM <- if (is.null(nM)) {
        make_mortality(river = e$river, sex = NULL, species = species,
                       max_age = e$max_age, custom_habitat = custom_habitat)
      } else nM
      
      e$eggs <- if (is.null(eggs)) {
        make_eggs_eel(e$river, species = species, length = e$length,
                      custom_habitat = custom_habitat)
      } else eggs
      
    }
    
    e$s_juvenile <- if (is.null(s_juvenile)) {
      sim_juvenile_s(species = species)
      } else s_juvenile
    
    e$acres          <- make_habitat(river = e$river, species = species, upstream = upstream, custom_habitat = custom_habitat)
    e$s_downstream   <- make_downstream(river = e$river, species = species, downstream = downstream,   upstream = upstream, custom_habitat = custom_habitat)
    e$prob_distrib   <- make_pop_distribution(river = e$river, species = species, upstream = upstream, downstream = downstream)
    
    environment(make_pop) <- e
    if (sex_specific) {
      
      e$pop_m <- make_pop(river = river, species = species, max_age = e$max_age_m, nM = e$nM_m, fM = fM, n_init = n_init * (1 - sr))
      e$pop_f <- make_pop(river = river, species = species, max_age = e$max_age_f, nM = e$nM_f, fM = fM, n_init = n_init * sr)
      
      e$pop_m <- outer(e$prob_distrib, e$pop_m, "*")
      e$pop_f <- outer(e$prob_distrib, e$pop_f, "*")
    } else {
      
      e$pop <- make_pop(river = river, species = species, max_age = e$max_age, nM = e$nM, fM = fM, n_init = n_init)
      e$pop <- outer(e$prob_distrib, e$pop, "*")
    }
    
    # EEL-specific placeholders (as in sim_pop's EEL branch)
    e$latitude    <- 0
    e$iteroparity <- 0
    e$s_spawn     <- 0
    e$s_postspawn <- 0
    
    envs[[i]] <- e
    rows[[i]] <- vector("list", nyears)
  }
  
  # ---------------------------------------------------------------------
  # Year loop: each river does its within-year steps up through raw age0,
  # then RAW age0 (pre-upstream-passage) is pooled (averaged) across rivers.
  # Each river then applies its own upstream passage survival to that
  # shared/averaged age0 before projecting its population forward.
  # ---------------------------------------------------------------------
  for (t in seq_len(nyears)) {
    
    age0_riv <- numeric(n_riv)  # each river's own RAW (pre-upstream) age0 this year
    hab_prop <- numeric(n_riv)  # proportion of habitat per river
    
    for (i in seq_len(n_riv)) {
      e <- envs[[i]]
      e$t <- t
      
      if (sex_specific) {
        e$spawners_m <- make_spawners(e$pop_m, probs = e$spawnRecruit_m, species = species)
        e$spawners_f <- make_spawners(e$pop_f, probs = e$spawnRecruit_f, species = species)
        e$spawners   <- add_unequal_vectors(e$spawners_m, e$spawners_f) # this needs work
        
        e$pop_m <- e$pop_m - e$spawners_m
        e$pop_f <- e$pop_f - e$spawners_f
        e$pop   <- add_unequal_vectors(e$pop_m, e$pop_f)
      } else {
        e$spawners <- make_spawners(e$pop, probs = e$spawnRecruit, species = species)
        e$pop <- e$pop - e$spawners
      }
      
      e$spawners_down <- colSums(e$spawners * e$s_downstream) # those that do not successfully outmigrate are assumed to die, are not added back into pop
      
      e$fec <- make_recruits(eggs = e$eggs, sr = sr)
      
      e$age0 <- e$spawners_down * e$fec
      
      e$age0 <- sum(e$age0 * e$s_juvenile)
      
      age0_riv[i] <- e$age0
      
      hab_prop[i] <- e$acres
      
      # each river's own upstream passage survival, applied below to the
      # SHARED/averaged age0 rather than to each river's own raw age0
      e$s_upstream_j <- make_upstream(
        river = e$river, species = species, upstream = upstream, custom_habitat = custom_habitat
      )
    }
    
    # ---- distribute across rivers by proportion of habitat (pre-upstream-passage) ----
    age0_shared <- sum(age0_riv)
    
    hab_prop <- hab_prop / sum(hab_prop)
    
    age0_riv2 <- hab_prop * age0_shared

    for (i in seq_len(n_riv)) {
      e <- envs[[i]]
      
      # apply the probability distribution by in-migrating juveniles proportionalized by upstream survival (so some are not successful)
      e$age0_up <- age0_riv2[i] * (e$prob_distrib  * e$s_upstream_j)
      
      e$spawners2 <- rowSums(e$spawners)
       
      #bev_holt 2A...
      e$acres_by_reach <- anadrofish::habitat_eel[anadrofish::habitat_eel$River_huc == e$river, ]$Hab_sqkm * 247.105
      
      e$spawners_per_acre <- e$spawners2#/acres_by_reach
      e$spawners_per_acre[!is.finite(e$spawners_per_acre)] <- 200
      
      e$fit <- nls(e$age0_up ~ (alpha * e$spawners_per_acre) / (1 + 0.005 * e$spawners_per_acre),
                 data = data.frame(spawners_per_acre = e$spawners_per_acre, age0_up = e$age0_up),
                 start = list(alpha = 0.34))
      
      summary(e$fit)
      e$alpha_est <- coef(e$fit)["alpha"]
      e$alpha_est
      
      # check fit quality
      # Plot observed vs. predicted
      # thing <- as.data.frame(cbind(e$age0_up, e$spawners_per_acre))
      # 
      # thing$predicted <- predict(e$fit)
      # 
      # plot(thing$spawners_per_acre, thing$age0_up, 
      #      xlab = "Spawners", ylab = "Age-0 Recruits",
      #      main = "Beverton-Holt Fit")
      # points(thing$spawners_per_acre, thing$predicted, col = "red", pch = 16)
      # 
      # # Or overlay a smooth curve across the full spawner range
      # S_seq <- seq(0, max(thing$spawners_per_acre), length.out = 100)
      # lines(S_seq, (alpha_est * S_seq) / (1 + 0.005 * S_seq), col = "blue", lwd = 2)
      
      # Build final function
      beverton_holt2 <- function(S, alpha = 5, beta = 0.005) {
        beta = 0.005/e$acres_by_reach
        (alpha * S) / (1 + beta * S)
      }
      
      e$predicted_recruits <- beverton_holt2(S=e$spawners2)
      e$predicted_recruits[!is.finite(e$predicted_recruits)] <- 0
      
      e$age0_up_reach <- pmin(e$age0_up, e$predicted_recruits)

      
      # # separate density-dependent mortality stage: incoming age-0 in-migrants
      # # (age0_up) compete with the standing population (pop, not spawners --
      # # spawners have already died at sea) for space in the river's total
      # # habitat. Total fish in the river (pop + age0_up) are subject to
      # # Beverton-Holt limitation (a = 1 => no mortality from this process at
      # # S near 0), then survivors are apportioned back to age0_up in
      # # proportion to its share of the pre-limitation total.
      # e$total_fw <- sum(e$pop) + sum(e$age0_up)
      # e$total_fw_survivors <- beverton_holt(
      #   a = 0.34, S = e$total_fw, b = b, acres = e$acres, age_structured = FALSE
      # )
      # e$age0_up <- e$total_fw_survivors * (e$age0_up / e$total_fw)
      # 
      # # # Calculate density-dependent recruitment from Beverton-Holt curve
      # # e$age0_up <- sum(beverton_holt(
      # #   a = e$fec,
      # #   S = e$spawners,
      # #   b = e$b,
      # #   acres = e$acres,
      # #   age_structured = TRUE
      # # ))
      
      if (sex_specific) {
        e$pop_m <- project_pop(x = e$pop_m, age0 = e$age0_up * (1 - sr),
                               nM = e$nM_m, fM = fM, max_age = e$max_age_m, species = species)
        e$pop_f <- project_pop(x = e$pop_f, age0 = e$age0_up * sr,
                               nM = e$nM_f, fM = fM, max_age = e$max_age_f, species = species)
        e$pop   <- add_unequal_vectors(e$pop_m, e$pop_f)
      } else {
        e$pop <- project_pop(x = e$pop, age0 = e$age0_up_reach, nM = e$nM, fM = fM,
                             max_age = e$max_age, species = species)
      }
      
      rows[[i]][[t]] <- make_output_row(e, species, sex_specific)
    }
  }
  
  # ---------------------------------------------------------------------
  # Assemble per-river output, then stack into one data.frame
  # ---------------------------------------------------------------------
  out_list <- lapply(seq_len(n_riv), function(i) {
    assemble_output(rows[[i]], age_structured_output, output_years)
  })
  
  do.call(rbind, out_list)
}