navbarPage("Vaccines 2014", id="tabs",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  theme = td_theme,
  collapsible = T,
  
  tabPanel("Explore", value="explore",
    fluidRow(
      column(6,
             h3(class = "text-primary", "Indicators"),
             p("Indicators without data (all NAs) have been dropped"),
             hr(),
             DTOutput("tbl_vars")),
      column(6,
             h3(class = "text-primary", "Summary of selected Indicators"),
             p("Raw unweigted summary of selected indicator"),
             hr(),
             actionButton("bt_clear","reset", class = "btn btn-sm mb-2", icon = icon("trash")),
             actionButton("bt_visualize", "visualize selected", class = "btn btn-sm mb-2", icon = icon("chart-bar")),
             verbatimTextOutput("tbl_my_summary"))
    )
  ),
  tabPanel("Visualize selected", value = "visualize",
    fluidRow(
      sidebarPanel(
        selectInput("s_group", "Group by", multiple = F, selectize = F, choices = NULL),
        h4("Filter"),
        tags$div(id = 'dynamic_selects'),
        actionButton("bt_filter", "filter", class = "btn-sm")
      ),
      mainPanel(
        h4("Plot"),
        plotOutput("plot_main"),
        tableOutput("table_main")
      )
    )
  ),
  tabPanel("About"),
  bslib::nav_spacer(),
  bslib::nav_item(tags$a(icon("github", class="fa-lg"), href = "https://github.com/guasi/vaccines")),
  tags$footer(includeHTML("www/footer.html"))
)
