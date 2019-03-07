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
#' @param xlim Min and max of longitude
#' @param ylim Min and max of latitude
#' @return Nothing
#' @export
#' @examples
#' prediction_object <- lr_prediction(lr_get_gbif_data_for_species("Quercus alba", country="US", state="Tennessee"))
#' lr_prediction_plot(prediction_object, state="tennessee")
lr_prediction_plot <- function(lr_prediction_output, state="all", xlim=c(-90.360481, -81.53846), ylim=c(34.947001, 36.67528)) {
  maps::map('county', region=tolower(state),  fill=FALSE, xlim=xlim, ylim=ylim)
  # plot(wrld_simpl,
  #    xlim = c(lr_prediction_output$ranges$min.lon, lr_prediction_output$ranges$max.lon),
  #    ylim = c(lr_prediction_output$ranges$min.lat, lr_prediction_output$ranges$max.lat),
  #    axes = TRUE,
  #    col = "grey95")
  sp::plot(lr_prediction_output$predictions, add = TRUE)
  graphics::points(lr_prediction_output$observations$longitude, lr_prediction_output$observations$latitude, col = "black", pch = 20, cex = 0.75)
  maps::map('county', region=tolower(state),  fill=FALSE, add=TRUE, xlim=xlim, ylim=ylim)
  maps::map('state',  fill=FALSE, add=TRUE)
}

#' Function to cache all species data
#' @param pkg If TRUE, save within package
#' @param state Two letter state code
#' @return A list of lists. Each element in the list has a species_name and the prediction and other information from lr_prediction()
#' @export
lr_cache_all <- function(pkg=TRUE, state=NULL) {
  all_species <- lr_get_listed_species(state=state)
  atrisk <- list()
  for (i in seq_along(all_species)) {
    prediction_object <- NULL
    print(paste0("Now trying ", all_species[i], ", which is ", i, " of ", length(all_species)))
    try(prediction_object <- lr_prediction(lr_get_gbif_data_for_species(all_species[i], country=NULL, state=NULL))) #get all locations to do better prediction
    tn_only <- NULL
    try(tn_only <- lr_prediction(lr_get_gbif_data_for_species(all_species[i]))) #to make sure there are records in TN
    if(!is.null(prediction_object) & !is.null(tn_only)) {
      atrisk[[length(atrisk)+1]] <- list(species_name = all_species[i], prediction=prediction_object)
    }
  }
  if(pkg) {
    devtools::use_data(atrisk, overwrite=TRUE)
  }
  return(atrisk)
}

#' Data on TN endangered species
#'
#' This is a list created with the lr_cache_all() function.
#' Each element of that list has two elements: species_name with the name of a federally-listed endangered or threatened species in Tennessee, and a prediction element that has the result of the lr_prediction function.
#' Note that the location data and prediction is for the species' entire range, not just the range in Tennessee.
#' Species with insufficient data for analysis are not included
#'
#' @name atrisk
#' @docType data
#' @author Brian O'Meara \email{omeara.brian@gmail.com}
#' @keywords data
"atrisk"
