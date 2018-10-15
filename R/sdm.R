#' Create species model from GBIF object
#'
#' Uses ideas from https://jcoliver.github.io/learn-r/011-species-distribution-models.html, by Jeff Oliver
#'
#' @param gbif gbif class object, from lr_get_gbif_data_for_species() or rgbif::occ_search()
#' @return prediction from dismo, a Bioclim object
#' @export
lr_prediction <- function(gbif) {
  observations <- data.frame(longitude=gbif$data$decimalLongitude, latitude=gbif$data$decimalLatitude, stringsAsFactors=FALSE)
  observations <- observations[!is.na(observations$longitude),]
  bioclim_data <- dismo::getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = tempdir())
  predictions <- dismo::predict(x=bioclim_data, p=observations)
  return(predictions)
}
