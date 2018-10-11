context("Check carcass persistence functions.")


test_that("Simple test of cpm.", {
  data(mock)

  CPModel <- cpm(formula_l = l ~ 1, 
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays")
  
  # Test that a cpm model was returned.
  expect_true("cpm" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSimple.rds",sep="/"))
  
})

test_that("Complex test of cpm.",{
  
  CPModel <- cpm(formula_l = l ~ Season*Visibility, 
                 formula_s = s ~ Season,
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays",
                 dist="loglogistic")
  
  # Test that a cpm model was returned.
  expect_true("cpm" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmComplex.rds",sep="/"))
  
  # TODO: Test errors for cpm
  
})

# TODO: test cpLogLik

# TODO: test rcp

test_that("Simple test of cpmSet.", {
  data(mock)
  
  CPModel <- cpmSet(formula_l = l ~ 1, 
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays")
  
  # Test that a cpm model was returned.
  expect_true("cpmSet" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSetSimple.rds",sep="/"))
})

test_that("Complex test of cpmSet.",{
  
  CPModel <- cpmSet(formula_l = l ~ Season*Visibility, 
                 formula_s = s ~ Season,
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays",
                 dist="loglogistic")
  
  # Test that a cpm model was returned.
  expect_true("cpmSet" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSetComplex.rds",sep="/"))
  
  
  # TODO: Test errors for cpmSet.
})


test_that("Simple test of cpmSetSize.", {
  data(mock)
  
  CPModel <- cpmSetSize(formula_l = l ~ 1, 
                    data = mock$CP, 
                    left = "LastPresentDecimalDays", 
                    right = "FirstAbsentDecimalDays",
                    sizeclassCol="Size")
  
  # Test that a pkm model was returned.
  expect_true("cpmSetSize" %in% class(CPModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSetSizeSimple.rds",sep="/"))
  
})

test_that("Complex test of cpmSetSize.",{

  CPModel <- cpmSetSize(formula_l = l ~ Season*Visibility, 
                    formula_s = s ~ Season,
                    data = mock$CP, 
                    left = "LastPresentDecimalDays", 
                    right = "FirstAbsentDecimalDays",
                    dist="loglogistic",
                    sizeclassCol = "Size")
  
  # Test that a cpm model was returned.
  expect_true("cpmSetSize" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSetSizeComplex.rds",sep="/"))
  
  
  # TODO: Test errors.
  
})

test_that("Test of cpmSetAICcTab",{
  data(mock)
  CPModel <- cpmSet(formula_l = l ~ Season, 
                formula_s = s ~ Season,
           data = mock$CP, 
           left = "LastPresentDecimalDays", 
           right = "FirstAbsentDecimalDays"
          )
  CPTable <- cpmSetAICcTab(CPModel)
  
  # Test that table matches old output.
  expect_equal_to_reference(CPTable,paste(CPTestDataPath,"cpmSetAICcTab.rds",sep="/"))
   
   
  # TODO: Test two message outputs for cpmSetAICcTab.
  
})

# TODO: Test ppersist.

test_that("Test of cpmFail.",{
  data(mock)
  
  CPModel <- cpm(formula_l = l ~ Season, 
                    formula_s = s ~ Season,
                    data = mock$CP, 
                    left = "LastPresentDecimalDays", 
                    right = "FirstAbsentDecimalDays"
  )
  
  # Successful fit.
  expect_false(cpmFail(CPModel))
  
  # Wrong model passed in.
  wrongModel <-   cpmSet(formula_l = l ~ Season, 
                      formula_s = s ~ Season,
                      data = mock$CP, 
                      left = "LastPresent", 
                      right = "FirstAbsent")
  
  expect_true(cpmFail(wrongModel))
  
  # NA value present.
  CPModelNA <-   CPModel
  CPModelNA$AIC <- NA
  
  expect_true(cpmFail(CPModelNA))
  
  # Sum of diagonal of variance matrix is less than 0.
  CPModelNegVarbeta <-   CPModel
  diag(CPModelNegVarbeta$varbeta) <- -1
  
  expect_true(cpmFail(CPModelNegVarbeta))
})

test_that("Test of cpmSetFail.",{
  data(mock)
  
  CPModel <- cpmSet(formula_l = l ~ Season, 
                 formula_s = s ~ Season,
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays"
  )
  
  # Successful fit.
  expect_false(any(cpmSetFail(CPModel)))
  
  # Wrong model passed in.
  # Wrong model passed in.
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)
  wrongModel <- lm(weight ~ group)
  
  expect_true(any(cpmSetFail(wrongModel)))
  
  # NA value present.
  CPModelNA <-   CPModel
  CPModelNA[[1]]$AIC <- NA
  
  expect_true(any(cpmFail(CPModelNA)))
  
  # Sum of diagonal of variance matrix is less than 0.
  CPModelNegVarbeta <-   CPModel
  diag(CPModelNegVarbeta[[1]]$varbeta) <- -1
  
  expect_true(any(cpmFail(CPModelNegVarbeta)))
})

test_that("Test of cpmSetSizeFail.",{
  data(mock)
  
  CPModel <- cpmSetSize(formula_l = l ~ Season, 
                    formula_s = s ~ Season,
                    data = mock$CP, 
                    left = "LastPresentDecimalDays", 
                    right = "FirstAbsentDecimalDays",
                    sizeclassCol="Size"
  )
  
  # Successful fit.
  expect_false(any(unlist(cpmSetSizeFail(CPModel))))
  
  # Wrong model passed in.
  # Wrong model passed in.
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  weight <- c(ctl, trt)
  wrongModel <- lm(weight ~ group)
  
  expect_true(any(unlist(cpmSetSizeFail(wrongModel))))
  
  # NA value present.
  CPModelNA <-   CPModel
  CPModelNA[[1]]$M$AIC <- NA
  
  expect_true(any(cpmFail(CPModelNA)))
  
  # Sum of diagonal of variance matrix is less than 0.
  CPModelNegVarbeta <-   CPModel
  diag(CPModelNegVarbeta$M[[1]]$varbeta) <- -1
  
  expect_true(any(unlist(cpmSetSizeFail(CPModelNegVarbeta))))
})

test_that("Test of cpmSetFailRemove.",{
  CPModel <- cpmSet(formula_l = l ~ Season*HabitatType, 
                        formula_s = s ~ Season,
                        data = mock$CP, 
                        left = "LastPresentDecimalDays", 
                        right = "FirstAbsentDecimalDays")
  
  CPModelSuccesses <- cpmSetFailRemove(CPModel)
  
  # Test length to determine if cpm actually removed the failing models.
  expect_equal(length(CPModelSuccesses),14)
  
  # Test that output equals the old output.
  expect_equal_to_reference(CPModelSuccesses,paste(CPTestDataPath,"cpmSetFailRemove.rds",sep="/"))
  
  
})

test_that("Test of cpmSetSizeFailRemove.",{
  CPModel <- cpmSetSize(formula_l = l ~ Season*HabitatType, 
                    formula_s = s ~ Season,
                    data = mock$CP, 
                    left = "LastPresentDecimalDays", 
                    right = "FirstAbsentDecimalDays",
                    sizeclassCol = "Size")
  
  CPModelSuccesses <- cpmSetSizeFailRemove(CPModel)
  
  # Test length to determine if cpm actually removed the failing models.
  expect_equal(length(CPModelSuccesses$S),14)
  expect_equal(length(CPModelSuccesses$M),14)
  expect_equal(length(CPModelSuccesses$L),14)
  expect_equal(length(CPModelSuccesses$XL),14)
  
  # Test that output equals the old output.
  expect_equal_to_reference(CPModelSuccesses,paste(CPTestDataPath,"cpmSetSizeFailRemove.rds",sep="/"))
  
  
})



