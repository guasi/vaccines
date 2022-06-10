library(shiny)
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

# THEMES -------------------------------------------
td_theme <- bslib::bs_theme(version = 5,
                            bootswatch = "yeti",
                            font_scale = .9,
                            "input-font-size" = ".75rem",
                            "table-cell-padding-y" = ".2rem")

# TEXT----------------------------------------------
TXT_PICKONE <- "Pick at least one indicator."

TXT_NOTICE <- list(
  p("This application allows you to explore", 
    strong("raw unweigthed"), 
    "vaccination data from the",
    a("2014 Child National Immunization Survey", href="https://www.cdc.gov/nchs/nis/data_files.htm")), 
  p("It provides a", 
    strong("rough overview"), 
    "to help analysts select indicators for further analysis elsewhere."),
  p("To select an idicator, click on its row on the table on the left.")
)