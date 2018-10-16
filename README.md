# TCBC student demo

This R package creates a species distribution model for any federally listed (threatened or endangered) species in Tennessee with enough published localities to do so. It also links to a phylogeny (evolutionary family tree of species) to let you examine its relationships.

You can install it in R by doing:

```
install.packages("devtools")
devtools::install_github("TCBCprototype/localreports")
```

The main functions are:

* `lr_get_gbif_data_for_species()`: Get GBIF locality information for a species
* `lr_prediction()`: Use the output of the above function to create a distribution map
* `lr_prediction_plot()`: To plot the output of the above function

Read the help for each. This also has functions for loading and parsing lists of federally listed species. It has cached results for all Tennessee species in `data(atrisk)`.

There is also a shiny app for a web interface. After installing the package, you can run this with `runDemo()`.

The species distribution modeling follows [Jeff Oliver's tutorial](https://jcoliver.github.io/learn-r/011-species-distribution-models.html). The dashboard links out to [OneZoom](http://onezoom.org). Tools from [Global Names](http://gnrd.globalnames.org/) and [Phylotastic](http://www.phylotastic.org) are also used.
