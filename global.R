library(shiny)
library(tidyverse)
library(DT)

# DATA ----------------------------------------------------
load("data/NISPUF14_w_FACTORS.RData")
NISPUF14_VARS <- readRDS("data/NISPUF14_VARS.RDS")

# JS SNIPETS ----------------------------------------------
enter_to_search_js <- '$(document).on("keyup", function(e) {
  if(e.keyCode == 13) {
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'

icon_arrow <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><!--! Font Awesome Pro 6.1.1 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license (Commercial License) Copyright 2022 Fonticons, Inc. --><path d="M438.6 278.6l-160 160C272.4 444.9 264.2 448 256 448s-16.38-3.125-22.62-9.375c-12.5-12.5-12.5-32.75 0-45.25L338.8 288H32C14.33 288 .0016 273.7 .0016 256S14.33 224 32 224h306.8l-105.4-105.4c-12.5-12.5-12.5-32.75 0-45.25s32.75-12.5 45.25 0l160 160C451.1 245.9 451.1 266.1 438.6 278.6z"/></svg>'
# THEMES -------------------------------------------
td_theme <- bslib::bs_theme(version = 5,
                            bootswatch = "sandstone",
                            font_scale = .8,
                            "table-cell-padding-y" = ".2rem")

# ALERTS -------------------------------------------# Alerts --------------------------------------------
click_to_select_alert <- "<div class='alert alert-dismissible alert-warning'>
    <button type='button' class='btn-close' data-bs-dismiss = 'alert'></button>
    <strong>Click</strong> on the >> or the << to add or delete idicators you want to visualize.
  </div>"
