library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(markdown)

# DATA ----------------------------------------------------
load("data/NISPUF14.RData")

# remove columns with all NAs and PROVWT_D, RDDWT_D, STRATUM
MDATA <- NISPUF14[ , colSums(is.na(NISPUF14)) < nrow(NISPUF14)] 
MDATA[2:6] <- NULL 

# make a separate data frame for labels
MDATA_VARS <- data.frame(key = names(MDATA), lbl = sapply(MDATA, attr, "label"))

rm(NISPUF14)

# UTILS -------------------------------------------
source("util.R")