# eel pop distribution, only considering dam order, weighted by avail hab

make_pop_distribution3 <- function (river = river,
                                   species = species,
                                   upstream = upstream,
                                   downstream = downstream) {
  
  hab <- anadrofish::habitat_eel[anadrofish::habitat_eel$River_huc == river, ]
  
  dam_name_cols <- grep("^Dam[0-9]+_Name$", names(hab), value = TRUE)

  # downstream dam string
  hab$dam_str <- apply(hab[, dam_name_cols], 1, function(x) {
    x <- x[!is.na(x) & x != "0" & x != "" & x != " "]
    paste(x, collapse = ",")
  })
  
  hab$dam_str <- strsplit(hab$dam_str,split = ",")
  
  # dam table: HERE IS WHERE WE CAN CHANGE DAM EFFICIENCIES
  dam_table <- data.frame(name = unique(unlist(hab[,dam_name_cols])),
                          efficiency = 0.5)
  
  hab <- hab %>%
    mutate(
      cum_passage = map_dbl(dam_str, function(dam_ids) {
        if (length(dam_ids) == 0) return(1)  # no dams = unimpeded
        eff <- dam_table$efficiency[match(dam_ids, dam_table$name)]
        prod(eff, na.rm = TRUE)
      })
    )
  
  hab <- hab %>%
    mutate(
      weight = Hab_sqkm * cum_passage,
      probability = weight / sum(weight)
      )
  
  prob_dist <- c(hab$probability)
  
  return(prob_dist)
  
}
  