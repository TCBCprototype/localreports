#' Create species model from GBIF object
#'
#' Uses ideas from https://jcoliver.github.io/learn-r/011-species-distribution-models.html, by Jeff Oliver
#'
#' @param gbif gbif class object, from lr_get_gbif_data_for_species() or rgbif::occ_search()
#' @return list with objects prediction (from dismo::predict()), geographic_extent (raster extent return), observations (data.frame of longitudes and latitudes), ranges (vector of ranges of species data), and gbif (original data).
#' @export
lr_prediction <- function(gbif) {
  observations <- data.frame(longitude=gbif$data$decimalLongitude, latitude=gbif$data$decimalLatitude, stringsAsFactors=FALSE)
  observations <- observations[!is.na(observations$longitude),]
  max.lat <- ceiling(max(observations$latitude))
  min.lat <- floor(min(observations$latitude))
  max.lon <- ceiling(max(observations$longitude))
  min.lon <- floor(min(observations$longitude))
  geographic_extent <- raster::extent(x = c(min.lon, max.lon, min.lat, max.lat))
  bioclim_data <- raster::getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = tempdir())
  bioclim_data_pruned <- raster::crop(x = bioclim_data, y = geographic_extent)
  bioclim_model <- dismo::bioclim(x = bioclim_data_pruned, p = observations)
  predictions <- dismo::predict(object = bioclim_model, x = bioclim_data_pruned, ext = geographic_extent)
  return(list(predictions=predictions, geographic_extent=geographic_extent, observations=observations, ranges=c(min.lon=min.lon, max.lon=max.lon, min.lat=min.lat, max.lat=max.lat)))
}

#' Plot prediction and species points
#'
#' Follows Jeff Oliver's tutorial at https://jcoliver.github.io/learn-r/011-species-distribution-models.html
#'
#' @param lr_prediction_output Output from lr_prediction_output() function
#' @param state State to plot (for all states, pass in ".")
#' @return Nothing
#' @export
#' @examples
#' prediction_object <- lr_prediction(lr_get_gbif_data_for_species("Quercus alba", country="US", state="Tennessee"))
#' lr_prediction_plot(prediction_object)
lr_prediction_plot <- function(lr_prediction_output, state="tennessee") {
  maps::map('state', region=tolower(state),  fill=FALSE)
  # plot(wrld_simpl,
  #    xlim = c(lr_prediction_output$ranges$min.lon, lr_prediction_output$ranges$max.lon),
  #    ylim = c(lr_prediction_output$ranges$min.lat, lr_prediction_output$ranges$max.lat),
  #    axes = TRUE,
  #    col = "grey95")
  plot(lr_prediction_output$predictions, add = TRUE)
  points(lr_prediction_output$observations$longitude, lr_prediction_output$observations$latitude, col = "black", pch = 20, cex = 0.75)
  maps::map('state', region=tolower(state),  fill=FALSE, add=TRUE)
}

#' Function to cache all species data
#' @param pkg If TRUE, save within package
#' @return A list of lists. Each element in the list has a species_name and the prediction and other information from lr_prediction()
#' @export
lr_cache_all <- function(pkg=TRUE) {
  all_species <- lr_get_listed_species()
  cached_data <- list()
  for (i in seq_along(all_species)) {
    prediction_object <- NULL
    try(prediction_object <- lr_prediction(lr_get_gbif_data_for_species(all_species[i])))
    if(!is.null(prediction_object)) {
      cached_data[[length(cached_data)+1]] <- list(species_name = all_species[i], prediction=prediction_object)
    }
  }
  if(pkg) {
    devtools::use_data(cached_data, overwrite=TRUE)
  }
  return(cached_data)
}
