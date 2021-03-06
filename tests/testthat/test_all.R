test_that("Getting TN species works", {
  expect_gte(sum(grepl("Myotis", lr_get_listed_species())),0)
})

test_that("Getting Quercus alba works", {
  expect_equal(class(lr_get_gbif_data_for_species("Quercus alba", country=NULL, state=NULL)), "gbif")
  expect_equal(class(lr_get_gbif_data_for_species("Quercus alba", country="US", state=NULL)), "gbif")
  expect_equal(class(lr_get_gbif_data_for_species("Quercus alba", country="US", state="Tennessee")), "gbif")
})

test_that("Prediction works", {
  result <- lr_prediction(lr_get_gbif_data_for_species("Quercus alba", country="US", state="Tennessee"))
  expect_gte(length(result$predictions),2000)
})

test_that("Getting Country's Species works", {
  result<- lr_get_gbif_data_for_species("Quercus alba", country="US")
  expect_gte(length(unique(result$data$stateProvince)),2)
})

test_that("Get listed species",{
  result <- lr_get_listed_species(state=NULL)
  expect_gte(length(result),1000)
  result <- lr_get_listed_species(state="TN")
  expect_gte(length(result),50)
})