navbarPage("Vaccines 2014", id="navbar",
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  theme = td_theme,
  collapsible = T,
  
  
  tabPanel("Select Indicators",
    modalDialog(title = "Explore Raw Data", 
                footer = modalButton("close", icon = icon("times")), 
                easyClose = T, 
                div(TXT_NOTICE)),
    fluidRow(
      column(5,
             h3(class = "text-primary", "Indicators"),
             actionButton("bt_clear","reset", class = "btn btn-sm", style="float:right", icon = icon("trash")),
             p("Indicators without data not included"),
             hr(),
             DTOutput("table_vars")),
      column(7,
             h3(class = "text-primary", "Quick view of selected indicators"),
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
    fluidRow(class = "px-3",
      column(2, class = "well",
        p(strong(class = "text-primary", icon("chart-bar"), "Plot elements")),
        selectInput("plot_x", "x-axis", choices = NULL, selected = NULL, multiple = F, selectize = T),
        selectInput("plot_y","measure", choices = NULL, selected = NULL, multiple = F, selectize = T),
        radioButtons("plot_pos","Bar plot style", inline = T,
                     choices = c("stack","fill","dodge"), selected = "stack"),
        radioButtons("plot_type","Plot type (numerical only)", inline = T,
                     choices = c("density","jitter"), selected = "density")
      ),
      column(7,
        plotOutput("plot_examine"),
        hr(),
        tableOutput("table_examine")
      ),
      column(3, class = "well",
        p(strong(class = "text-primary", icon("filter"), "Filters")),
        div(id = "div_filters"),
        actionButton("bt_fapply", "apply filters", class = "btn btn-primary btn-sm"),
        actionButton("bt_fclear", "clear filters", class = "btn btn-warning btn-sm"),
        br(),br(),
        p(tags$small(class = "text-muted", "* filters apply to all selected"))
      )
    )
  ),
  tabPanel("About", style="max-width:700px;margin: 0 auto", includeMarkdown("include/about.md")),
  bslib::nav_spacer(),
  bslib::nav_item(tags$a(icon("github", class="fa-lg"), href = "https://github.com/guasi/vaccines")),
  tags$footer(includeHTML("www/footer.html"))
)
