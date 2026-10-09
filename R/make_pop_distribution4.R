# eel pop distribution, only considering dam order, weighted by avail hab

make_pop_distribution4 <- function (river = river,
                                   species = species,
                                   upstream = upstream,
                                   downstream = downstream) {
  
  hab <- anadrofish::habitat_eel[anadrofish::habitat_eel$River_huc == river, ]
  
  dam_name_cols <- grep("^Dam[0-9]+_Name$", names(hab), value = TRUE)
  
  hab$dam_str    <- sapply(seq_list, paste, collapse = "_")
  
  riv_groups <- split(seq_len(nrow(hab)), hab$dam_str)  

  # downstream dam string
  hab$dam_str <- apply(hab[, dam_name_cols], 1, function(x) {
    x <- x[!is.na(x) & x != "0" & x != "" & x != " "]
    paste(x, collapse = ",")
  })
  
  hab$dam_str <- strsplit(hab$dam_str,split = ",")
  hab$dam_str <- lapply(hab$dam_str, rev)
  dam_km_cols <- sub("_Name$", "_km", dam_name_cols)

hab$n_dam <- rowSums(hab[, dam_km_cols] != 0)

name_mat <- as.matrix(hab[dam_name_cols])
km_mat   <- as.matrix(hab[dam_km_cols])

seq_list <- vector("list", nrow(hab))
km_list  <- vector("list", nrow(hab))

for (i in seq_len(nrow(hab))) {
    nm <- name_mat[i, ]
    km <- suppressWarnings(as.numeric(km_mat[i, ]))
    keep <- !is.na(nm) & nm != "" & nm != "0" & !is.na(km) & km != 0
    seq_list[[i]] <- unname(nm[keep])
    km_list[[i]]  <- unname(km[keep])
}

# hab$dam_seq    <- seq_list
# hab$dam_km_seq <- km_list
hab$dam_str    <- sapply(seq_list, paste, collapse = "_")
# hab$dam_string <- as.factor(hab$dam_str)
# hab$dam_string <- as.numeric(hab$dam_string)
# dam_table <- unique(hab[,c("dam_str","dam_string")])

riv_groups <- split(seq_len(nrow(hab)), hab$dam_str)  # named list of row-index vectors

# make dam lists for each group
all_dam_dists <- list()
all_dam_names <- list()

for (i in 1:length(riv_groups)){
    
    # unique dam paths
    hab_riv_group <- hab[riv_groups[[i]], c("n_dam", "riv_dist", dam_km_cols, dam_name_cols)]
    
    # keep only dam km columns actually used in this group, plus matching name columns
    keep_km <- dam_km_cols[colSums(hab_riv_group[, dam_km_cols] != 0) > 0]
    keep_name <- sub("_km$", "_Name", keep_km)
    hab_riv_group <- hab_riv_group[, c("n_dam", "riv_dist", keep_km, keep_name)]
    
    print(i)
    if (is.data.frame(hab_riv_group) && length(keep_km) > 0) {
        n_dam <- max(hab_riv_group$n_dam)
        dam_dists <- vector()
        dam_names <- vector()
        
        for (j in 1:n_dam) {
            dam_km_col <- paste0("Dam", j, "_km")
            dam_name_col <- paste0("Dam", j, "_Name")
            
            hab_riv_group[[paste0("dam_", j)]] <- hab_riv_group$riv_dist - hab_riv_group[[dam_km_col]]
            dam_loc <- mean(hab_riv_group[[paste0("dam_", j)]])
            dam_dists <- c(dam_dists, dam_loc)
            
            dam_name <- unique(hab_riv_group[[dam_name_col]][hab_riv_group[[dam_name_col]] != ""])
            dam_names <- c(dam_names, dam_name)
        }
        
    } else {
        dam_dists <- 0
        dam_names <- NA
    }
    
    all_dam_dists[[i]] <- dam_dists
    all_dam_names[[i]] <- dam_names
}


# make the overall dam database for the river
dam_df <- data.frame(
    group = rep(seq_along(all_dam_names), lengths(all_dam_names)),
    dam_name = unlist(all_dam_names),
    dam_length = unlist(all_dam_dists),
    stringsAsFactors = FALSE
)

# there are some differences, so summarize
dam_summary <- dam_df %>%
    group_by(dam_name) %>%
    summarise(mean_length = mean(dam_length),
              n_groups = n(),
              sd = sd(dam_length),
              diff = max(dam_length)-min(dam_length))


dam_summary <- as.data.frame(dam_summary[order(dam_summary$mean_length),])
  # dam table: HERE IS WHERE WE CAN CHANGE DAM EFFICIENCIES
  dams <- data.frame(name = unique(unlist(hab[,dam_name_cols])),
                     efficiency = 0.5)
  
  hab <- hab %>%
    mutate(
      cum_passage = map_dbl(dam_str, function(dam_ids) {
        if (length(dam_ids) == 0) return(1)  # no dams = unimpeded
        eff <- dam_table$efficiency[match(dam_ids, dam_table$name)]
        prod(eff, na.rm = TRUE)
      })
    )
  
  # log normal distance prob
  lognormal_kernel <- function(x, peak_km, spread = 1) {
    sigma <- spread
    mu <- log(peak_km) + sigma^2   # solves so that mode = peak_km
    dlnorm(x, meanlog = mu, sdlog = sigma)
  }

  peak_km <- max(hab[hab$Tidal==1,]$riv_dist)
  x <- seq(1:max(hab$riv_dist))
  
  plot(lognormal_kernel(x,peak_km))  
  
  # Combine dam and distance into one weight
  hab <- hab %>%
    mutate(
      dist_wt   = lognormal_kernel(riv_dist, peak_km = peak_km,
                                   spread = 1),
      weight    = Hab_sqkm * cum_passage * dist_wt,
      probability = weight / sum(weight)
    )
  
  prob_dist <- c(hab$probability)
  
  return(prob_dist)
  
}
  