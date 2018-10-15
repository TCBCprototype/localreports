test_that("Getting TN species works", {
  expect_gte(sum(grepl("Myotis", lr_get_listed_species())),0)
})

test_that("Getting Quercus alba works", {
  expect_equal(class(lr_get_gbif_data_for_species("Quercus alba", country=NULL, state=NULL)), "gbif")
  expect_equal(class(lr_get_gbif_data_for_species("Quercus alba", country="US", state=NULL)), "gbif")
  expect_equal(class(lr_get_gbif_data_for_species("Quercus alba", country="US", state="Tennessee")), "gbif")
})

test_that("Prediction works", {
  result <- lr_prediction(r_get_gbif_data_for_species("Quercus alba", country="US", state="Tennessee"))
  expect_equal(class(result), "Bioclim")
})
