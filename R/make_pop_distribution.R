# eel pop distribution

make_pop_distribution <- function (river = river,
                                   species = species,
                                   upstream = upstream,
                                   downstream = downstream) {

hab <- anadrofish::habitat_eel[anadrofish::habitat_eel$River_huc == river, ]
# hab <- data.frame(
#   REACHCODE = c("A","B","C","D","E","F","G","H"),
#   #riv_km_start = c(0,50,0,100,200),
#   #riv_km_end = c(50,100,100,200,250),
#   riv_dist = c(10,20,30,50,100,100,200,250),
#   Hab_sqkm = c(0.1,0.1,0.1,2,0.5,7,2,4),
#   #DamOrder = c(0,1,0,1,2),
#   Latitude = rep(44,8),
#   State = rep("ME",8),
#   River_huc = rep("Trial River", 8),
#   POP = rep("NNE", 8)
# )

dams <- anadrofish::dams_eel[anadrofish::dams_eel$River_huc == river, ]
# dams <- data.frame(
#   River_huc = "Trial River",
#   hot = 125,
#   Dam_1 = 200
# )

# Set head of tide and furthest point inland
hot <- dams$hot # peak is at head of tide
max <- max(hab$riv_dist)
dams_list <- na.omit(unlist(dams[1, 3:ncol(dams)]))

# Solve y = A * b^x given two points (x1,y1) and (x2,y2)
exp_fit <- function(x1, y1, x2, y2) {
  b <- (y2 / y1)^(1 / (x2 - x1))
  A <- y1 / b^x1
  list(A = A, b = b)
}

# Build underlying distribution ----
make_exp <- function(x1, y1, x2, y2) {
  params <- exp_fit(x1, y1, x2, y2)
  function(x) params$A * params$b^x
}

tidal_f <- make_exp(0, 0.01, hot, 0.1)

fw_f <- make_exp(hot, 0.1, max, 0.01)

# Dam penalty ----
combined_f <- function(x, dams = dams_list, hot_val = hot, drop = upstream) {
  dams_sorted <- sort(dams)
  
  sapply(x, function(xi) {
    
    # underlying distribution
    base_val <- if (xi <= hot_val) tidal_f(xi) else fw_f(xi)
    
    # walk through each dam upstream of xi, accumulating the vertical offset
    offset <- 0
    dams_passed <- dams_sorted[dams_sorted <= xi]  # use `<` if drop shouldn't apply exactly at the dam
    
    for (d in dams_passed) {
      base_at_dam <- if (d <= hot_val) tidal_f(d) else fw_f(d)
      current_at_dam <- base_at_dam - offset   # value just upstream of this dam, after prior drops
      offset <- offset + current_at_dam * drop # add this dam's drop to the running offset
    }
    
    pmax(base_val - offset, 0)  # clamp at 0 in case offset overshoots
  })
}

# # Plot ----
# x_vals <- seq(0, max, by = 0.01)
# y_vals <- combined_f(x_vals)
# 
# plot(x_vals, y_vals, type = "l", col = "blue", lwd = 3,
#      xlab = "River Distance",
#      ylab = "Probability Distribution",
#      main = river)
#   abline(v = dams_list, col = "red", lty = 2)
#   abline(v = hot, col = "darkgreen", lty = 3, lwd = 2)
#   legend("topright", 
#          legend = c("Combined f(x)", "Dams", "Head of Tide"),
#          col = c("blue", "red", "darkgreen"), 
#          lty = c(1, 2, 3), lwd = c(2, 1, 2))

# Calculate Distribution by river km----
hab$prob_dist <- combined_f(hab$riv_dist) / sum(combined_f(hab$riv_dist))

prob_dist <- c(hab$prob_dist)

return(prob_dist)
}





# Calculate proportion
# hab$area <- sapply(1:nrow(hab), function(i) {
#   integrate(combined_f, lower = hab$riv_km_start[i], upper = hab$riv_km_end[i])$value
# })
# 
# hab$p_area <- hab$area / sum(hab$area, na.rm = T)


# total_area <- 0
# hab$area <- 0
# 
# for (i in 1:nrow(hab)) {
# 
#   lower = hab[i,"riv_km_start"]
#   upper = hab[i,"riv_km_end"]
# 
#   area <- integrate(combined_f, lower = lower, upper = upper)$value
# 
#   hab[i,"area"] <- area
# }
# 
# total_area <- sum(hab$area)
# 
# hab$p_area <- hab$area/total_area


# tidal_area <- integrate(tidal_f, lower = 0, upper = 50)
# 
# fw_area <- integrate(fw_f, lower = 50, upper = 250)









#}
















