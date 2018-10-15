#' Get endangered and threatened US species from FWS
#'
#' Downloads all species listed on https://ecos.fws.gov's site for a state
#'
#' @param state Two letter state code
#' @return A vector of species
#' @export
lr_get_listed_species <- function(state="TN") {
  return(rphylotastic::url_get_scientific_names(paste0('https://ecos.fws.gov/ecp0/reports/species-listed-by-state-report?state=', state, '&status=listed')))
}

#' Get GBIF data for a given species
#'
#' Download data for a given species from GBIF
#'
#' @param species The species name to search for info for
#' @param limit How many records to return
#' @param country Two letter country code. NULL if you want all countries.
#' @param state Full state name. NULL if you want all states.
#' @return An object of class gbif (from the rgbif package)
#' @export
lr_get_gbif_data_for_species <- function(species, limit=1000, country="US", state="Tennessee") {
  result <- c()
  if(is.null(country)) {
    result <- rgbif::occ_search(scientificName = species, limit = limit)
  } else {
    if(is.null(state)) {
      result <- rgbif::occ_search(scientificName = species, limit = limit, country=country)
    } else {
      result <- rgbif::occ_search(scientificName = species, limit = limit, country=country, stateProvince=state )
    }
  }
  return(result)
}
