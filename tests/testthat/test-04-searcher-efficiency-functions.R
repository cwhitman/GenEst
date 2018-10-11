context("Check searcher efficiency functions")



test_that("Simple test of pkm.", {
  data(mock)
  
  
  SEModel <- pkm(formula_p = p ~ Season,
                 data = mock$SE,
                 kFixed = 1)
  
  # Test that a pkm model was returned.
  expect_true("pkm" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSimple.rds", sep = "/"))
  
})

test_that("Complex test of pkm.", {
  data(mock)
  
  SEModel <- pkm(
    formula_p = p ~ Visibility * Season,
    formula_k = k ~ Visibility,
    data = mock$SE,
    obsCol = c("Search1", "Search2", "Search3")
  )
  
  # Test that a pkm model was returned.
  expect_true("pkm" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmComplex.rds", sep = "/"))
})

test_that("Errors, messages, and warnings of pkm.", {
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

test_that("Simple test of pkmSet.", {
  data(mock)
  
  SEModel <- pkmSet(formula_p = p ~ Season,
                    data = mock$SE,
                    kFixed = 1)
  
  # Test that a pkm model was returned.
  expect_true("pkmSet" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSetSimple.rds", sep = "/"))
  
})

test_that("Complex test of pkmSet.", {
  SEModel <- pkmSet(
    formula_p = p ~ Season * HabitatType,
    formula_k = k ~ Visibility,
    data = mock$SE,
    obsCol = c("Search1", "Search2", "Search3")
  )
  
  # Test that a pkm model was returned.
  expect_true("pkmSet" %in% class(SEModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(SEModel,
                            paste(SETestDataPath, "pkmSetComplex.rds", sep = "/"))
})

test_that("Errors, messages, and warnings of pkmSet", {
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

test_that("Simple test of pkmSetSize.", {
  data(mock)
  
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
  
})

test_that("Complex test of pkmSetSize.", {
  data(mock)
  
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
  
})

test_that("Errors, messages, and warnings of pkmSetSize.", {
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

test_that("Test of pkmSetAICcTab.",
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

test_that("Test of rpk.", {
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


test_that("Test of pkmFail." , {
  data(mock)
  
  SEModel <-
    pkm(
      formula_p = p ~ Season,
      formula_k = k ~ Season,
      data = mock$SE
    )
  
  # Successful fit.
  expect_false(pkmFail(SEModel))
  
  # Wrong model passed in.
  wrongModel <-     pkmSet(
    formula_p = p ~ Season,
    formula_k = k ~ Season,
    data = mock$SE
  )
  
  expect_true(pkmFail(wrongModel))
  
  # NA value present.
  SEModelNA <- SEModel
  SEModelNA$CL <- NA
  
  expect_true(pkmFail(SEModelNA))
  
  # Sum of diagonal of variance matrix is less than 0.
  SEModelNegVarbeta <- SEModel
  diag(SEModelNegVarbeta$varbeta) <- -1
  
  expect_true(pkmFail(SEModelNegVarbeta))
  
})

test_that("Test of pkmSetFail." , {
  data(mock)
  
  SEModel <-
    pkmSet(
      formula_p = p ~ Season,
      formula_k = k ~ Season * Visibility,
      data = mock$SE
    )
  
  # Successful fit.
  expect_false(any(pkmSetFail(SEModel)))
  
  # Wrong model passed in.
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)
  wrongModel <- lm(weight ~ group)
  
  expect_true(any(pkmSetFail(wrongModel)))
  
  # NA value present.
  SEModelNA <- SEModel
  SEModelNA[[1]]$CL <- NA
  
  expect_true(any(pkmFail(SEModelNA)))
  
  # Sum of diagonal of variance matrix is less than 0.
  SEModelNegVarbeta <- SEModel
  diag(SEModelNegVarbeta[[1]]$varbeta) <- -1
  
  expect_true(any(pkmSetFail(SEModelNegVarbeta)))
})

test_that("Test of pkmSetSizeFail.", {
  data(mock)
  
  SEModel <- pkmSetSize(
    formula_p = p ~ Season,
    formula_k = k ~ Season,
    data = mock$SE,
    sizeclassCol = "Size"
  )
  
  # Successful fit.
  expect_false(any(unlist(pkmSetSizeFail(SEModel))))
  
  # Wrong model passed in.
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)
  wrongModel <- lm(weight ~ group)
  
  expect_true(any(unlist(pkmSetSizeFail(wrongModel))))
  
  # NA value present.
  SEModelNA <- SEModel
  SEModelNA$M[[1]]$CL <- NA
  
  expect_true(any(unlist(pkmSetSizeFail(SEModelNA))))
  
  # Sum of diagonal of variance matrix is less than 0.
  SEModelNegVarbeta <- SEModel
  diag(SEModelNegVarbeta$M[[1]]$varbeta) <- -1
  
  expect_true(any(unlist(pkmSetSizeFail(
    SEModelNegVarbeta
  ))))
})

test_that("Test of pkmSetAllFail", {
  data(mock)
  
  SEModel <- pkmSet(
    formula_p = p ~ Season,
    formula_k = k ~ Season,
    data = mock$SE
  )
  
  # Successful fit.
  expect_false(pkmSetAllFail(SEModel))
  
  # Wrong model passed
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)
  wrongModel <- lm(weight ~ group)
  
  expect_true(pkmSetAllFail(wrongModel))
  
  # NA value present.
  SEModelNA <- lapply(SEModel, function(model) {
    model$CL <- NA
  })
  
  expect_true(pkmSetAllFail(SEModelNA))
  
  # Sum of diagonal of variance matrix is less than 0.
  SEModelNegVarbeta <-
    lapply(SEModel, function(model) {
      diag(model$varbeta) <- -1
    })
  
  expect_true(pkmSetAllFail(SEModelNegVarbeta))
})

test_that("Test of pkmSetFailRemove.", {
  data(mock)
  
  SEModel <- pkmSet(
    formula_p = p ~ Season * Visibility * HabitatType,
    formula_k = k ~ 1,
    data = mock$SE
  )
  
  SEModelSuccesses <- pkmSetFailRemove(SEModel)
  
  # Check size of model.
  expect_equal(length(SEModelSuccesses), 10)
  
  # Check that value is the same as old output.
  expect_equal_to_reference(SEModelSuccesses,
                            paste(SETestDataPath, "pkmSetFailRemove.rds", sep =
                                    "/"))
  
  
})

test_that("Test of pkmSetSizeFailRemove.", {
  data(mock)
  
  SEModel <- pkmSetSize(
    formula_p = p ~ Season * Visibility * HabitatType,
    formula_k = k ~ 1,
    data = mock$SE,
    sizeclassCol = "Size"
  )
  
  SEModelSuccesses <- pkmSetSizeFailRemove(SEModel)
  
  # Check size of models.
  expect_equal(length(SEModelSuccesses$S), 10)
  expect_equal(length(SEModelSuccesses$L), 10)
  expect_equal(length(SEModelSuccesses$M), 10)
  expect_equal(length(SEModelSuccesses$XL), 10)
  
  # Check that value is the same as old output.
  expect_equal_to_reference(SEModelSuccesses,
                            paste(SETestDataPath, "pkmSetSizeFailRemove.rds", sep =
                                    "/"))
  
})

test_that("Test of SEsi.", {
  #TODO: See if these inputs make sense.
  
  decayedSE <- SEsi(c(1, 2, 3), c(0.5, 0.5))
  expect_equal(decayedSE, c(0.5, 0.125))
})

test_that("Test of SEsi0", {
  #TODO: See if these inputs make sense.
  
  decayedSE <- SEsi0(c(1, 2, 3), c(0.5, 0.5))
  expect_equal(decayedSE, c(0.5, 0.125))
})
