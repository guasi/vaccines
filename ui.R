navbarPage("Vaccines 2014", id="tabs",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  theme = td_theme,
  #tags$script(enter_to_search_js),
  collapsible = T,
  
  tabPanel("Select", value="tab1",
    fluidRow(
      column(6,
             h3(class = "text-primary", "Indicators to choose from"),
             hr(),
             DTOutput("tbl_notpicked")),
      column(6,
             h3(class = "text-primary", "Chosen indicators to visualize"),
             hr(),
             actionButton("bt_clear", "reset", class = "btn-sm mb-2"),
             actionButton("bt_apply","glimpse", class = "btn-sm mb-2"),
             DTOutput("tbl_picked"))
    )
  ),
  tabPanel("Glimpse", value = "tab2",
    fluidRow(
      column(6,
             h3("summary()"),
             verbatimTextOutput("tbl_summary")),
      column(6,
             h3("get_summary()"),
             verbatimTextOutput("tbl_my_summary"))
    )
  ),
  tabPanel("Group", value = "tab3",
           h3("Grouped associations"),
           verbatimTextOutput("tbl_group")),
  tabPanel("About"),
  bslib::nav_spacer(),
  bslib::nav_item(tags$a(icon("github", class="fa-lg"), href = "https://github.com/guasi/vaccines")),
  tags$footer(includeHTML("www/footer.html"))
)
