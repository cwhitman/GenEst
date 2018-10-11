#' @title R code run before tests.
#' 
#' @description This file is run before any of the test files.
#'              It sets up the file locations of the test data.
#'              All variables within this file are available to all tests.
#'              

#TODO: Throw error here if output folder doesn't exist.

# Test input files folder.
inputTestDataPath <- "./tests/testData/input/"

# Test output files folder.
SETestDataPath <- "../testData/test-04-searcher-efficiency-functions/"
CPTestDataPath <- "../testData/test-05-carcass-persistence-functions/"
DPTestDataPath <- "../testData/test-06-detection-probability-functions/"