# =============================================================================
# Build `branches`, and add `branch_id` / `river_km` to hab and dams,
# directly from NHDPlus VAA (Value Added Attribute) fields.
#
# NOTE: this script needs network access to the USGS VAA endpoint that
# nhdplusTools::get_vaa() calls, which isn't reachable from this sandbox,
# so it hasn't been run end-to-end here. Logic below is derived directly
# from the NHDPlus VAA field definitions - run and sanity-check it against
# your real data before trusting the output.
# =============================================================================
make_branches <- function (hab) {
  
library(nhdplusTools)
library(dplyr)

peak_km <- max(hab[hab$Tidal == 1, ]$riv_dist)
# -----------------------------------------------------------------------
# 1. Pull VAA fields for your watershed
# -----------------------------------------------------------------------
# IMPORTANT: pull VAA for the FULL upstream network (every flowline in your
# watershed), not just the comids that happen to have a reach or dam on
# them. If you filter too tightly, a tributary's dnlevelpat can point to a
# parent branch whose outlet flowline got filtered out, leaving NAs later.
# Get the full comid set for your watershed first (e.g. via get_UT() from
# your basin's most downstream comid, or a spatial AOI query), THEN filter.

my_watershed_comids <- hab$COMID

vaa <- get_vaa(c("comid", "levelpathi", "hydroseq", "dnlevelpat",
                 "pathlength", "lengthkm", "terminalfl")) %>%
  filter(comid %in% my_watershed_comids)

# -----------------------------------------------------------------------
# 2. Identify each branch's (level path's) own outlet flowline
# -----------------------------------------------------------------------
# LevelPathI is DEFINED as the hydroseq of that path's own most-downstream
# flowline, so hydroseq == levelpathi picks out the outlet row directly.
outlets <- vaa %>%
  filter(hydroseq == levelpathi) %>%
  transmute(
    branch_id          = levelpathi,
    outlet_pathlength  = pathlength,
    dnlevelpat,
    terminalfl
  )

# sanity check: should be exactly one outlet row per unique levelpathi
stopifnot(!any(duplicated(outlets$branch_id)))

# -----------------------------------------------------------------------
# 3. Build the branches table
# -----------------------------------------------------------------------
branches <- outlets %>%
  mutate(
    parent_branch_id = ifelse(terminalfl == 1, NA_real_, dnlevelpat),
    parent_outlet_pathlength = outlet_pathlength[match(parent_branch_id, branch_id)],
    confluence_km = ifelse(
      is.na(parent_branch_id), NA_real_,
      outlet_pathlength - parent_outlet_pathlength
    )
  ) %>%
  select(branch_id, parent_branch_id, confluence_km)

# sanity check: every non-NA parent_branch_id should actually exist as a
# branch_id in this table (if not, you filtered your comid set too tightly
# in step 1 - go back and include more of the network)
orphans <- setdiff(na.omit(branches$parent_branch_id), branches$branch_id)
if (length(orphans) > 0) {
  warning("Parent branch(es) missing from `branches` - widen your comid pull: ",
          paste(orphans, collapse = ", "))
}

# add in the orphans
vaa_orphans <- get_vaa(atts = c("comid", "levelpathi", "hydroseq")) %>%
  filter(hydroseq %in% orphans)

my_watershed_comids <- c(my_watershed_comids, vaa_orphans$comid)

vaa <- get_vaa(c("comid", "levelpathi", "hydroseq", "dnlevelpat",
                 "pathlength", "lengthkm", "terminalfl")) %>%
  filter(comid %in% my_watershed_comids)

outlets <- vaa %>%
  filter(hydroseq == levelpathi) %>%
  transmute(
    branch_id          = levelpathi,
    outlet_pathlength  = pathlength,
    dnlevelpat,
    terminalfl
  )

branches <- outlets %>%
  mutate(
    parent_branch_id = ifelse(terminalfl == 1, NA_real_, dnlevelpat),
    parent_outlet_pathlength = outlet_pathlength[match(parent_branch_id, branch_id)],
    confluence_km = ifelse(
      is.na(parent_branch_id), NA_real_,
      outlet_pathlength - parent_outlet_pathlength
    )
  ) %>%
  select(branch_id, parent_branch_id, confluence_km)


# -----------------------------------------------------------------------
# 4. Compute within-branch river_km for every flowline
# -----------------------------------------------------------------------
# river_km = distance from THIS branch's own mouth (its outlet flowline),
# increasing upstream - matches the convention used in distribute_eels().
# Using the flowline MIDPOINT here; swap in frommeas/tomeas if you need to
# place a dam or reach more precisely along a specific flowline.
vaa <- vaa %>%
  mutate(
    branch_outlet_pathlength = outlets$outlet_pathlength[match(levelpathi, outlets$branch_id)],
    river_km = pathlength - branch_outlet_pathlength + lengthkm / 2
  )

flowline_lookup <- vaa %>%
  transmute(comid, branch_id = levelpathi, river_km)

# -----------------------------------------------------------------------
# 5. Join onto your existing hab / dams tables
# -----------------------------------------------------------------------
# Assumes hab and dams each have a `comid` column identifying which
# NHDPlus flowline that reach/dam sits on. If a "reach" in your data spans
# multiple flowlines (e.g. you dissolved several COMIDs into one habitat
# reach), pick one representative comid per reach (commonly the most
# downstream one) before this join.

hab <- hab %>% left_join(flowline_lookup, by = c("COMID"="comid"))

dams <- subset(hab, hab$LocDams==1)
dams <- dams[,c("COMID","Dam1_Name")]
dams <- dams %>% left_join(flowline_lookup, by = c("COMID"="comid"))
dams$passage_eff_up <- 0.5
names(dams)[names(dams) == "Dam1_Name"] <- "dam_id"
# sanity check after joining - no reach/dam should be missing branch info
# stopifnot(!any(is.na(hab$branch_id)))
# stopifnot(!any(is.na(dams$branch_id)))

return(list(hab,dams,branches))

}