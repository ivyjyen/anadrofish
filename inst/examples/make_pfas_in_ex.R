# Example usage
\dontrun{
    
    # Generated values 
    spawners <- c(0, 0, 0, 1634, 14506, 11674, 6392, 3521, 1948)
    spawners_down <- c(0, 0, 0, 1464, 13064, 10542, 5782, 3189, 1765)
    mass <- c(64, 144, 206, 247, 271, 284, 292, 296, 298)   
    
    # Calculate expected number of age-0 fish  
    pfas_in <- make_pfas_in(spawners = spawners, spawners_down = spawners_down, mass = mass)
    
}
