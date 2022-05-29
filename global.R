library(shiny)
library(tidyverse)
library(DT)

# DATA ----------------------------------------------------
load("data/NISPUF14_w_FACTORS.RData")
NISPUF14_VARS <- readRDS("data/NISPUF14_VARS.RDS")

NISPUF14 <- NISPUF14[ , colSums(is.na(NISPUF14)) < nrow(NISPUF14)] #remove columns with all NAs
NISPUF14_VARS <- NISPUF14_VARS[names(NISPUF14),] #remove columns with all NAs

# THEMES -------------------------------------------
td_theme <- bslib::bs_theme(version = 5,
                            bootswatch = "sandstone",
                            font_scale = .8,
                            "table-cell-padding-y" = ".2rem")

# ALERTS -------------------------------------------
click_to_select_alert <- "<div class='alert alert-dismissible alert-warning'>
    <button type='button' class='btn-close' data-bs-dismiss = 'alert'></button>
    <strong>Click</strong> on the >> or the << to add or delete idicators you want to visualize.
  </div>"
