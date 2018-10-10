context("Check searcher efficiency functions")



test_that("pkm fits a searcher efficiency model", {
  data(mock)
  
  
  ##############
  # Simple test.
  ##############
  SEModel <- pkm(formula_p = p ~ Season,
                 data = mock$SE,
                 kFixed = 1)
  
  # Test that a pkm model was returned.
  expect_true("pkm" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSimple.rds", sep = "/"))
  
  #############
  # More complex test.
  #############
  data(mock)
  SEModel <- pkm(
    formula_p = p ~ Season,
    formula_k = k ~ Visibility,
    data = mock$SE,
    obsCol = c("Search1", "Search2", "Search3")
  )
  
  # Test that a pkm model was returned.
  expect_true("pkm" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmComplex.rds", sep = "/"))
  
  ########################
  # Test errors and warning messages.
  #########################
  
  # Observation column provided not in data.
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mock$SE,
    obsCol = c("Non-existent.")
  ))
  
  # No observer columns in data.
  mockErrorSE <-
    mock$SE[, grep("Search",
                   colnames(mock$SE),
                   invert = TRUE,
                   value = TRUE)]
  expect_error(pkm(formula_p = p ~ Season,
                   data = mockErrorSE))
  
  # No obsCol provided and no appropriate column names found.
  expect_error(pkm(formula_p = p ~ Season,
                   data = mockErrorSE))
  
  # User-supplied formula includes predictor(s) not found in data.
  expect_error(pkm(
    formula_p = p ~ Season + NotHere,
    data = mock$SE,
    kFixed = 1
  ))
  
  
  # User-supplied kFixed must be numeric (or NULL)
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = "10"
  ))
  
  # Vector-valued kFixed. Only the first element will be used.
  expect_message(pkm(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = c(0.5, 0.6)
  ))
  
  # User-supplied kFixed is outside the supported range [0, 1].
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = 5
  ))
  
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = -0.5
  ))
  
  # Formula and fixed value provided for k, fixed value used.
  expect_message(pkm(
    formula_p = p ~ Season,
    formula_k = k ~ 1,
    data = mock$SE,
    kFixed = 1
  ))
  
  # Only one search occasion per carcass. k not estimated.
  expect_message(pkm(
    formula_p = p ~ Season,
    formula_k = k ~ 1,
    data = mock$SE,
    obsCol = "Search1"
  ))
  
  # p is estimated from data in first search occasion only.
  expect_message(pkm(
    formula_p = p ~ Season,
    formula_k = NULL,
    data = mock$SE
  ))
  
  # No non-missing data present in user-provided data.
  # TODO: Can't reproduce this error.
  
  # Carcasses observed more than once. Check data.
  mockErrorSE <- mock$SE
  mockErrorSE[1, "Search2"] <- 1
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mockErrorSE,
    kFixed = 1
  ))
  
  # Searches continue after carcass discovery? Check data.
  # TODO: Can't reproduce this error.
  
  # User-supplied kFixed = 0. However, carcasses were found after being missed in previous searches, which indicates k > 0
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = 0
  ))
  # Hyphen ( - ) and dot ( . ) not allowed in predictor names
  mockErrorSE <- mock$SE
  colnames(mockErrorSE) <- paste0(colnames(mock$SE), ".")
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mockErrorSE,
    kFixed = 1
  ))
  
  mockErrorSE <- mock$SE
  colnames(mockErrorSE) <- paste0(colnames(mock$SE), "-")
  expect_error(pkm(
    formula_p = p ~ Season,
    data = mockErrorSE,
    kFixed = 1
  ))
  
  # Initial search has all 0s or 1s in a cell.
  # TODO: Unable to reproduce error.
  
  
  # Failed optimization. Consider simplifying predictors.
  # TODO: Unable to reproduce error.
  
  # Unable to estimate variance.
  # TODO: Unable to reproduce error.
  
  
})


# TODO: Test print.pkm

# TODO: Test pkLogLik

test_that("pkmSet fits a searcher efficiency model", {
  data(mock)
  
  
  ##############
  # Simple test.
  ##############
  SEModel <- pkmSet(formula_p = p ~ Season,
                    data = mock$SE,
                    kFixed = 1)
  
  # Test that a pkm model was returned.
  expect_true("pkmSet" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSetSimple.rds", sep = "/"))
  
  #############
  # More complex test.
  #############
  SEModel <- pkmSet(
    formula_p = p ~ Season + HabitatType,
    formula_k = k ~ Visibility + Size,
    data = mock$SE,
    obsCol = c("Search1", "Search2", "Search3")
  )
  
  # Test that a pkm model was returned.
  expect_true("pkmSet" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSetComplex.rds", sep = "/"))
  
  ########################
  # Test errors and warning messages.
  #########################
  
  # Vector-valued kFixed. Only the first element will be used.
  expect_warning(pkmSet(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = c(0.5, 0.6)
  ))
  
  # Formula and fixed value provided for k, fixed value used.
  expect_message(pkmSet(
    formula_p = p ~ Season,
    formula_k = k ~ 1,
    data = mock$SE,
    kFixed = 1
  ))
  
  
  # Only one search occasion for each carcass; k not estimated.
  # TODO: Can't reproduce error.
})

test_that("pkmSetSize fits a searcher efficiency model", {
  data(mock)
  
  ##############
  # Simple test.
  ##############
  SEModel <- pkmSetSize(
    formula_p = p ~ Season,
    formula_k = k ~ Season,
    data = mock$SE,
    sizeclassCol = "Size"
  )
  # Test that a pkm model was returned.
  expect_equal(class(SEModel), c("pkmSetSize", "list"))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSetSizeSimple.rds", sep = "/"))
  
  #############
  # More complex test.
  #############
  SEModel <- pkmSetSize(
    formula_p = p ~ Season + HabitatType,
    formula_k = k ~ Season + Visibility,
    data = mock$SE,
    obsCol = c("Search1", "Search2", "Search3"),
    sizeclassCol = "Size"
  )
  
  # Test that a pkm model was returned.
  expect_true("pkmSetSize" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSetSizeComplex.rds", sep = "/"))
  
  # sizeclassCol not in data set.
  expect_error(
    pkmSetSize(
      formula_p = p ~ Season,
      formula_k = k ~ Season,
      data = mock$SE,
      sizeclassCol = "NotInData"
    )
  )
  # One unnamed kFixed value provided by user. All classes assumed to have this value.
  expect_message(pkmSetSize(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = 1,
    sizeclassCol = "Size"
  ))
  
  # kFixed names must be names of size classes.
  expect_error(pkmSetSize(
    formula_p = p ~ Season,
    data = mock$SE,
    kFixed = c("Season" = 1, "NotInData" = 1),
    sizeclassCol = "Size"
  ))
  
})

test_that("pkmSetAICcTab creates the AICc tables for a set of searcher efficiency models.",
          {
            data(mock)
            SEModel <- pkmSet(
              formula_p = p ~ Season,
              formula_k = k ~ Season,
              data = mock$SE
            )
            
            pkmTable <- pkmSetAICcTab(SEModel)
            
            # Test that value matches the old output.
            expect_equal_to_reference(pkmTable,
                                      paste(SETestDataPath, "pkmSetAICcTab.rds", sep = "/"))
          })

test_that("rpk simulates parameters from a fitted pk model", {
  data(mock)
  
  SEModel <-
    pkm(
      formula_p = p ~ 1,
      formula_k = k ~ Season,
      data = mock$SE
    )
  simulatedValues <- rpk(n = 10, model = SEModel, seed = 1)
  
  expect_equal_to_reference(simulatedValues,
                            paste(SETestDataPath, "rpk.rds", sep = "/"))
  
  # TODO: Test error values.
  
})

# TODO: Finish for functions in searcher_efficiency_functions
