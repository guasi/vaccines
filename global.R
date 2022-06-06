library(shiny)
library(tidyverse)
library(DT)

# DATA ----------------------------------------------------
load("data/NISPUF14.RData")

# remove columns with all NAs and PROVWT_D, RDDWT_D, STRATUM
NISPUF14 <- NISPUF14[ , colSums(is.na(NISPUF14)) < nrow(NISPUF14)] 
NISPUF14[2:6] <- list(NULL) 

# make a separate data frame for labels
NISPUF14_VARS <- data.frame(key = names(NISPUF14), lbl = sapply(NISPUF14,attr,"label"))

# THEMES -------------------------------------------
td_theme <- bslib::bs_theme(version = 5,
                            bootswatch = "yeti",
                            font_scale = .9,
                            "input-font-size" = ".75rem",
                            "table-cell-padding-y" = ".2rem")

# ALERTS -------------------------------------------
click_to_select_alert <- "<div class='alert alert-dismissible alert-warning'>
    <button type='button' class='btn-close' data-bs-dismiss = 'alert'></button>
    <strong>Click</strong> on the >> or the << to add or delete idicators you want to visualize.
  </div>"

# TEXT----------------------------------------------
TEXT_PICKONE <- "Pick at least one indicator"
