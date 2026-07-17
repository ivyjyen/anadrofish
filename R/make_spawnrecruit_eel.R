#' @title Make spawnrecruit vectors for American eel
#'
#' @description Simulate proportion of population that is mature
#' spawners from sex-specific maturity schedules for American eel.
#'
#' @details The primary use of this function is to simulate proportion of
#' mature spawners at each age in a population of American eel based on
#' region-specific probabilities of maturation at each age. Estuarine
#' maturity schedule was estimated from the general stock assessment model 
#' by ASMFC (2012) and inland maturity schedule was derived from data collected
#' from the Shenandoah River (Sheila Eyler, U.S. Fish & Wildlife Service, 
#' unpublished data.)
#' 
#' @param species A character indicating American eel species.
#'
#' @param river River for which maximum age is needed.
#'
#' @param sex A character indicating \code{"male"}, \code{"female"},
#' or \code{"Pooled"} sex for fish.
#'
#' @param custom_habitat A dataframe containing columns corresponding to the
#' those in the output from \code{\link{custom_habitat_template}}. The default,
#' \code{NULL} uses the default habitat data set for a given combination of
#' \code{species} and \code{river}.
#'
#' @return A numeric vector containing a single realization for proportion
#' of mature fish at each age, from age 1 to maximum age.
#'
#' @references Atlantic States Marine Fisheries Commission. 2023. American Eel
#' benchmark stock assessment and peer-review report. ASMFC, Arlington, VA.
#' URL: https://asmfc.org/wp-content/uploads/2024/11/AmEelBenchmarkStockAssessment_PeerReviewReport_Aug2023.pdf
#'
#' @examples make_spawnrecruit_eel(river = "Hudson", species = "EEL", sex = "female")
#'
#' @export
#'
make_spawnrecruit_eel <- function(river,
                                 sex = c("male", "female"),
                                 species = "EEL",
                                 length, # remove
                                 custom_habitat = NULL) {
    # Error handling ----
    # Require sex to be specified from vector of choices
    if (!missing(sex)) sex <- match.arg(sex, c("male", "female"))

    # Require species to be specified from vector of choices
    if (!missing(species)) species <- match.arg(species, "EEL")
    
    # River error handling
    if (missing(river)) {
        stop("

    Argument 'river' must be specified.

    To see a list of available rivers, run get_rivers() or specify river name
    in custom_habitat if used.")
    }
    
    if (!river %in% get_rivers(species) & is.null(custom_habitat)) {
        stop("

    Argument 'river' must be one of those included in get_rivers() or in
    custom_habitat if used.

    To see a list of available rivers, run get_rivers()")
    }
    
    # Get region
    region <- get_region(
      river = river, species = species,
      custom_habitat = custom_habitat
    )
    
    if (missing(sex)) {
      max_age <- make_maxage(
          river = river, species = species, custom_habitat = custom_habitat)
      probs <- as.numeric(
        colMeans(anadrofish::maturity_eel[
          anadrofish::maturity_eel$region == region, 4:(3 + max_age)
        ])
      )
    }
    
    if (!missing(sex)) {

      if (sex == "female") {
        max_age <- make_maxage(
          river = river, species = species, sex = "female", custom_habitat = custom_habitat)
        
        probs <- as.numeric(
          colMeans(anadrofish::maturity_eel[
            anadrofish::maturity_eel$region == region &
              anadrofish::maturity_eel$sex == "F", 4:(3 + max_age)
          ])
        )
      }
        
      if (sex == "male") {
        max_age <- make_maxage(
          river = river, species = species, sex = "male", custom_habitat = custom_habitat)
        
        probs <- as.numeric(
          colMeans(anadrofish::maturity_eel[
            anadrofish::maturity_eel$region == region &
              anadrofish::maturity_eel$sex == "M", 4:(3 + max_age)
          ])
        )
      }
    }
    
    
    return(probs)
}
