context("Check carcass persistence functions.")


test_that("cpm fits a single carcass persistence model", {
  data(mock)

  ##############
  # Simple test.
  ##############
  CPModel <- cpm(formula_l = l ~ 1, 
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays")
  
  # Test that a cpm model was returned.
  expect_true("cpm" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSimple.rds",sep="/"))
  
  ##############
  # Complex test.
  ##############
  # TODO: see is the input to this actually makes sense.
  CPModel <- cpm(formula_l = l ~ Season, 
                 formula_s = s ~ 1,
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays",
                 dist="loglogistic")
  
  # Test that a cpm model was returned.
  expect_true("cpm" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmComplex.rds",sep="/"))
  
  # TODO: Test errors.
  
})

# TODO: test cpLogLik

# TODO: test rcp

test_that("cpmSet fits multiple carcass persistence models", {
  data(mock)
  
  ##############
  # Simple test.
  ##############
  CPModel <- cpmSet(formula_l = l ~ 1, 
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays")
  
  # Test that a cpm model was returned.
  expect_true("cpmSet" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSetSimple.rds",sep="/"))
  
  ##############
  # Complex test.
  ##############
  # TODO: see is the input to this actually makes sense.
  CPModel <- cpmSet(formula_l = l ~ Season, 
                 formula_s = s ~ 1,
                 data = mock$CP, 
                 left = "LastPresentDecimalDays", 
                 right = "FirstAbsentDecimalDays",
                 dist="loglogistic")
  
  # Test that a cpm model was returned.
  expect_true("cpmSet" %in% class(CPModel))
  
  # Test that cpm model matches the old cpm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSetComplex.rds",sep="/"))
  
  
  # TODO: Test errors.
  
  
})


test_that("cpmSetSize fits multiple carcass persistence models", {
  data(mock)
  
  ##############
  # Simple test.
  ##############
  CPModel <- cpmSetSize(formula_l = l ~ 1, 
                    data = mock$CP, 
                    left = "LastPresentDecimalDays", 
                    right = "FirstAbsentDecimalDays",
                    sizeclassCol="Size")
  
  # Test that a pkm model was returned.
  expect_true("cpmSetSize" %in% class(CPModel))
  
  # Test that pkm model matches the old pkm output.
  expect_equal_to_reference(CPModel,paste(CPTestDataPath,"cpmSetSizeSimple.rds",sep="/"))
  
  ##############
  # Complex test.
  ##############
  # TODO: see is the input to this actually makes sense.
  CPModel <- cpmSetSize(formula_l = l ~ Season, 
                    formula_s = s ~ 1,
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

# TODO: test rest of functions.
