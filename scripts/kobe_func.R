# 
# Copyright https://github.com/norm42/License-Repository/blob/master/normzeck_mit_license%202017.md
# (Mit license)
#
# Code Flow
#  1.  kobe_get_sec computes seconds in game.
#  2.  check_pkgs finds packages needed but not loaded, ask to load those packages.
#  3. load_libraries loads required libraries.  I do not use require as I assume you have
#     loaded the libraries with check_pkgs
#
#  this function computes the seconds in the game
#  periods 1-4 are 12 minutes each
#  overtime periods are 5 minutes each
#  parameters:  
#  k_per:  period in game (1-7 in the data set)
#  k_min:  minutes left in period
#  k_sec:  seconds left in current minute
kobe_get_sec <- function(k_per, k_min, k_sec) {
  sec_in_game <- 0
  if(k_per <= 4) {
    sec_in_game <- (k_per - 1) * 12 * 60 + ((11 - k_min) * 60) + (60 - k_sec)
  } else {
    sec_in_game <- (4 * 12 * 60) + ((k_per - 5) * 5 * 60) + ((4 - k_min) * 60) + (60 - k_sec)
  }
  return(sec_in_game)
}

#--------------------------
# This function will check for required packages and ask to load the ones that are not loade
#
check_pkgs <- function() {
  pkg.inst <- installed.packages()
  pkgs <- c("quantmod", "ggplot2", "plyr", "histogram", "graphics", "pscl",
            "caret", "ROCR", "caTools", "randomForest", "pROC", 
            "ResourceSelection", "xgboost", "data.table", "Matrix", "e1071")
  have.pkg <- pkgs %in% rownames(pkg.inst)
  
  if(any(!have.pkg)) {
    need <- pkgs[!have.pkg]
    cat("Some packages need to be installed\n")
    cat("Packages to install: ", need, "\n")
    r <- readline("Install necessary packages [y/n]? ")
    if(tolower(r) == "y") {
      message("installing packages ",
              paste(need, collapse = ", "))
      install.packages(need)
    }
  }
}

#----------------------------
# Function to load libraries used in the analsys and modeling
#
load_libraries <- function() {
  library(quantmod)
  library(ggplot2)
  library(plyr)
  library(histogram)
  library(graphics)
  library(pscl)
  library(caret)
  library(ROCR)
  library(caTools)
  library(randomForest)
  library(pROC)
  library(ResourceSelection)
  library(xgboost)
  library(data.table)
  library(Matrix)
  library(e1071)
}
