# =============================================================================
# American Eel Spatial Distribution Model (multi-branch / tributary version)
#
# Combines four processes into a single per-reach probability:
#   1. Dam passage      - cumulative probability of passing all dams
#                          downstream of a reach, following the true network
#                          path (own branch -> parent branch -> ... -> ocean)
#   2. Distance kernel   - unimodal preference peaking at head of tide,
#                          measured as TOTAL network distance from the ocean
#   3. Redistribution    - eels that fail to pass a dam "pile up" just below
#                          it, within the SAME branch, decaying downstream
#   4. Branch topology    - reaches/dams live on branches; branches connect
#                          to a parent branch at a known confluence point
#
# Final weight per reach = base_weight (habitat x passage x distance)
#                         + redistributed_weight (failed passage from the
#                           dam immediately upstream on the SAME branch)
# Probabilities are normalized to sum to 1, then multiplied by total
# population to get n_eels per reach.
# =============================================================================
redistribute <- function (hab, dams, branches) {
library(dplyr)
library(purrr)
  
  peak_km <- max(hab[hab$Tidal == 1, ]$riv_dist)
# -----------------------------------------------------------------------
# Kernel functions
# -----------------------------------------------------------------------

# Distance preference: rises from the mouth, peaks at head of tide,
# decays upstream. river_km here must be TOTAL distance from the ocean.
distance_kernel <- function(riv_dist, peak_km, spread = 1) {
  sigma <- spread
  mu <- log(peak_km) + sigma^2   # solves so that mode = peak_km
  dlnorm(riv_dist, meanlog = mu, sdlog = sigma)
}

# rd <- seq(0.01, 390, length.out = 1000)  # avoid 0 (log-normal undefined at 0)
# adf <- distance_kernel(rd, peak_km = 40, spread = 1)
# plot(rd, adf, type = "l", col = "blue", lwd = 3,
#   xlab = "River kilometer", ylab = "Probability", bty = "l")
# abline(v = peak_km, col = "black", lty = 3, lwd = 2)
# legend("topright",
#        legend = c("Distance kernel", "Head of Tide"),
#        col = c("blue", "black"),
#        lty = c(1, 3), lwd = c(3, 2), bty = "n")
  
# Redistribution of failed-passage eels: peaks at the dam (dist = 0),
# decays moving downstream/away from it, within one branch.
redistribution_kernel <- function(dist_below_dam, rate = 0.1) {
  exp(-rate * dist_below_dam)
}

# dbd <- seq(0, 100, length.out = 1000)
# redist <- redistribution_kernel(dbd, rate = 0.1)
# 
# plot(-dbd, redist, type = "l", col = "blue", lwd = 3,
#      xlab = "Distance below dam (km)", ylab = "Redistribution weight", bty = "l")
# abline(v = 0, col = "black", lty = 3, lwd = 2)
# legend("topleft",
#        legend = c("Redistribution kernel", "Dam location"),
#        col = c("blue", "black"),
#        lty = c(1, 3), lwd = c(3, 2), bty = "n")

# -----------------------------------------------------------------------
# Branch topology helpers
# -----------------------------------------------------------------------

# Distance from the ocean to the MOUTH (confluence with parent) of each
# branch, computed recursively up the parent chain. Outlet branch(es)
# (parent_branch_id = NA) get offset 0.
# compute_branch_offsets <- function(branches) {
#   offsets <- setNames(rep(NA_real_, nrow(branches)), branches$branch_id)
#   
#   resolve <- function(bid, visited = character(0)) {
#     if (!is.na(offsets[bid])) return(offsets[[bid]])
#     if (bid %in% visited) stop("Circular branch reference detected at: ", bid)
#     row <- branches[branches$branch_id == bid, ]
#     if (nrow(row) == 0) stop("branch_id not found in branches table: ", bid)
#     if (is.na(row$parent_branch_id)) {
#       offsets[[bid]] <<- 0
#     } else {
#       parent_offset <- resolve(row$parent_branch_id, c(visited, bid))
#       offsets[[bid]] <<- parent_offset + row$confluence_km
#     }
#     offsets[[bid]]
#   }
#   
#   for (bid in branches$branch_id) resolve(bid)
#   offsets
# }

## Make tolerant of bad parent_branch_id
compute_branch_offsets <- function(branches) {
  offsets <- setNames(rep(NA_real_, nrow(branches)), branches$branch_id)
  
  resolve <- function(bid, visited = character(0)) {
    if (!is.na(offsets[bid])) return(offsets[[bid]])
    if (bid %in% visited) stop("Circular branch reference detected at: ", bid)
    row <- branches[branches$branch_id == bid, ]
    if (nrow(row) == 0) {
      warning("branch_id not found in branches table: ", bid, " -- treating as offset 0.")
      offsets[[bid]] <<- 0
      return(offsets[[bid]])
    }
    if (is.na(row$parent_branch_id)) {
      offsets[[bid]] <<- 0
    } else if (!(row$parent_branch_id %in% branches$branch_id)) {
      warning("parent_branch_id '", row$parent_branch_id, "' for branch '", bid,
              "' not found in branches table -- treating '", bid, "' as an outlet (offset 0).")
      offsets[[bid]] <<- 0
    } else {
      parent_offset <- resolve(row$parent_branch_id, c(visited, bid))
      offsets[[bid]] <<- parent_offset + row$confluence_km
    }
    offsets[[bid]]
  }
  
  for (bid in branches$branch_id) resolve(bid)
  offsets
}

# All dams strictly downstream of a given point (branch_id, km-within-branch),
# following the true network path: down the branch, then onto the parent
# branch at the confluence point, recursively, down to the ocean.
dams_below_point <- function(branch_id, km, dams, branches) {
  result <- character(0)
  current_branch <- branch_id
  current_km <- km
  visited <- character(0)
  
  repeat {
    if (current_branch %in% visited) stop("Circular branch reference detected")
    visited <- c(visited, current_branch)
    
    d <- dams$dam_id[dams$branch_id == current_branch & dams$river_km < current_km]
    result <- c(result, d)
    
    prow <- branches[branches$branch_id == current_branch, ]
    if (nrow(prow) == 0 || is.na(prow$parent_branch_id)) break
    current_km <- prow$confluence_km
    current_branch <- prow$parent_branch_id
  }
  result
}

# -----------------------------------------------------------------------
# Core function
# -----------------------------------------------------------------------
# hab required columns:
#   COMID, Hab_sqkm, branch_id, river_km (distance from THIS branch's
#   own mouth/confluence, increasing upstream)
#
# dams required columns:
#   dam_id, branch_id, river_km (same convention as hab), passage_eff_up
#
# branches required columns:
#   branch_id, parent_branch_id (NA for the outlet branch reaching the ocean),
#   confluence_km (distance along the PARENT branch where this branch joins;
#   NA/unused when parent_branch_id is NA)

distribute_eels <- function(hab, dams, branches,
                            peak_km,
                            sigma_down  = 5,
                            #sigma_up    = 40,
                            redist_rate = 0.1) {
  
  stopifnot(all(c("COMID", "Hab_sqkm", "branch_id", "river_km") %in% names(hab)))
  stopifnot(all(c("dam_id", "branch_id", "river_km", "passage_eff_up") %in% names(dams)))
  stopifnot(all(c("branch_id", "parent_branch_id", "confluence_km") %in% names(branches)))
  
  # Added warning
  known_branches <- branches$branch_id
  
  bad_hab <- !(hab$branch_id %in% known_branches)
  if (any(bad_hab)) {
    warning(sum(bad_hab), " hab row(s) reference unknown branch_id(s): ",
            paste(unique(hab$branch_id[bad_hab]), collapse = ", "),
            " -- these rows will be dropped.")
    orph <- hab[which(bad_hab==T),]
    hab <- hab[!bad_hab, ]
  }
  
  bad_dams <- !(dams$branch_id %in% known_branches)
  if (any(bad_dams)) {
    warning(sum(bad_dams), " dam row(s) reference unknown branch_id(s): ",
            paste(unique(dams$branch_id[bad_dams]), collapse = ", "),
            " -- these dams will be dropped.")
    dams <- dams[!bad_dams, ]
  }
  
  ## end warning
  
  branch_offsets <- compute_branch_offsets(branches)
  dams_static <- dams  # snapshot used for all topology queries below
  
  # 1. dam-level: what's downstream of each dam -> cumulative passage
  #    arriving at it -> fraction that fails there
  dams <- dams %>%
    rowwise() %>%
    mutate(dams_below_ids = list(dams_below_point(branch_id, river_km, dams_static, branches))) %>%
    ungroup() %>%
    mutate(
      river_km_total = branch_offsets[branch_id] + river_km,
      cum_passage_below = map_dbl(dams_below_ids, function(ids) {
        if (length(ids) == 0) return(1)
        prod(dams_static$passage_eff_up[match(ids, dams_static$dam_id)], na.rm = TRUE)
      }),
      failed_flux_frac = cum_passage_below * (1 - passage_eff_up)
    )
  
  # 2. reach-level: dams downstream (auto-derived) -> passage probability
  #    -> distance kernel weight -> base weight
  hab <- hab %>%
    rowwise() %>%
    mutate(dams_downstream_ids = list(dams_below_point(branch_id, river_km, dams_static, branches))) %>%
    ungroup() %>%
    mutate(
      river_km_total = branch_offsets[branch_id] + river_km,
      cum_passage = map_dbl(dams_downstream_ids, function(ids) {
        if (length(ids) == 0) return(1)
        prod(dams_static$passage_eff_up[match(ids, dams_static$dam_id)], na.rm = TRUE)
      }),
      dist_wt     = distance_kernel(river_km_total, peak_km),
      base_weight = Hab_sqkm * cum_passage * dist_wt
    )
  
  # 3. assign each reach to the nearest dam upstream WITHIN THE SAME BRANCH
  hab <- hab %>%
    rowwise() %>%
    mutate(
      upstream_dam = {
        cand <- dams_static[dams_static$branch_id == branch_id & dams_static$river_km >= river_km, ]
        if (nrow(cand) == 0) NA_character_ else cand$dam_id[which.min(cand$river_km)]
      }
    ) %>%
    ungroup()
  
  # 4. redistribute each dam's failed pool across reaches in its own
  #    branch segment, weighted by habitat x downstream-decay from the dam
  hab <- hab %>%
    group_by(upstream_dam) %>%
    mutate(
      dam_km_local     = dams_static$river_km[match(upstream_dam, dams_static$dam_id)],
      dist_below_dam   = dam_km_local - river_km,
      redist_wt_raw    = Hab_sqkm * redistribution_kernel(dist_below_dam, redist_rate),
      redist_share     = if (sum(redist_wt_raw, na.rm = TRUE) > 0)
        redist_wt_raw / sum(redist_wt_raw, na.rm = TRUE)
      else 0,
      failed_flux_frac = dams$failed_flux_frac[match(upstream_dam, dams$dam_id)],
      redistributed_weight = ifelse(is.na(upstream_dam), 0, redist_share * failed_flux_frac)
    ) %>%
    ungroup() %>%
    select(-dam_km_local)
  
  # 5. combine, normalize, allocate population
  hab %>%
    mutate(
      total_weight = base_weight + coalesce(redistributed_weight, 0),
      probability  = total_weight / sum(total_weight, na.rm = TRUE)
    )
}

# # =============================================================================
# # TOY EXAMPLE - mainstem + one tributary - replace with your real data
# # =============================================================================
# 
# branches_ex <- tibble(
#   branch_id        = c("M", "T1"),
#   parent_branch_id = c(NA, "M"),     # T1 joins the mainstem M
#   confluence_km     = c(NA, 20)       # ...at km 20 along M
# )
# 
# dams_ex <- tibble(
#   dam_id         = c("D1", "D2"),
#   branch_id      = c("M", "T1"),
#   river_km       = c(15, 10),         # D1 at km15 on M; D2 at km10 on T1
#   passage_eff_up = c(0.6, 0.4)
# )
# 
# hab_ex <- tibble(
#   COMID    = paste0("R", 1:6),
#   branch_id   = c("M", "M", "M", "M", "T1", "T1"),
#   river_km    = c(2, 8, 12, 25, 3, 15),   # local to each reach's own branch
#   Hab_sqkm = c(50, 80, 40, 60, 45, 55)
# )
# # R1-R3: mainstem, below D1
# # R4:    mainstem, above D1
# # R5:    tributary T1, below D2 (still must pass D1 on the mainstem to get here)
# # R6:    tributary T1, above D2 (must pass D1 then D2)

# split off hab rows whose branch_id isn't in the branches table
orph <- hab[!(hab$branch_id %in% branches$branch_id), ]
hab_ok <- hab[hab$branch_id %in% branches$branch_id, ]

result <- distribute_eels(
  hab         = hab_ok,
  dams        = dams,
  branches    = branches,
  peak_km     = peak_km,
  sigma_down  = 4,
  redist_rate = 0.15
)

# add orphans back (only if there are any)
if (nrow(orph) > 0) {
  result <- bind_rows(result, orph)
}

return(result %>%
        select(COMID, branch_id, river_km, river_km_total, cum_passage,
               dist_wt, redistributed_weight, probability))

# branch_offsets <- compute_branch_offsets(branches)
# dams$river_km_total <- branch_offsets[dams$branch_id] + dams$river_km
# 
# plot(result$river_km_total, result$probability,
#      type = "p", pch = 16, col = "blue",
#      main = "Androscoggin",
#      xlab = "River kilometer (total distance from ocean)",
#      ylab = "Probability",
#      bty = "l")
# 
# abline(v = dams$river_km_total, col = "red", lty = 2, lwd = 1.5)
# 
# legend("topright",
#        legend = c("Reach probability", "Dams"),
#        col = c("blue", "red"),
#        pch = c(16, NA), lty = c(NA, 2), lwd = c(NA, 1.5),
#        bty = "n")

}