navbarPage("Vaccines 2014", id="navbar",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  theme = td_theme,
  collapsible = T,
  
  tabPanel("Explore Raw Data",
    fluidRow(
      column(5,
             h3(class = "text-primary", "Indicators"),
             actionButton("bt_clear","reset", class = "btn btn-sm", style="float:right", icon = icon("trash")),
             p("Indicators without data not included"),
             hr(),
             DTOutput("table_vars")),
      column(7,
             h3(class = "text-primary", "Quick view of selected Indicators"),
             actionButton("bt_examine","examine selected", class = "btn btn-sm", style="float:right", icon = icon("eye")),
             p("Raw unweigted summary of selected indicators"),
             hr(),
             tabsetPanel(
               tabPanel("Summaries", icon = icon("info"), 
                        verbatimTextOutput("text_summary")),
               tabPanel("Grouped Table", icon = icon("table"), 
                        DTOutput("table_grouped")),
               tabPanel("Grouped Plot*", icon = icon("chart-bar"), 
                        plotOutput("plot_grouped"),
                        p(class = "text-danger", "* Plotting only ",strong("up to first four"),"factor variables"))
             ))
    )
  ),
  tabPanel("Examine Selected", value = "tab_examine",
    sidebarLayout(
      sidebarPanel(
        h5(class = "text-primary", "Plot elements"),
        selectInput("s_xaxis", "x-axis", choices = NULL, multiple = F, selectize = F),
        selectInput("s_measures","measure", choices = NULL, multiple = F, selectize = F),
        h5(class = "text-primary", "Filters"),
        tags$div(id = "div_filters"),
        actionButton("bt_apply", "apply filters", class = "btn btn-sm")
      ),
      mainPanel(
        plotOutput("plot_examine")
      )
    )
  ),
  tabPanel("About"),
  bslib::nav_spacer(),
  bslib::nav_item(tags$a(icon("github", class="fa-lg"), href = "https://github.com/guasi/vaccines")),
  tags$footer(includeHTML("www/footer.html"))
)
