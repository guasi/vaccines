# Plot elements
plot_controls <- 
  menuItem("Plot elements", icon = icon("chart-bar"), startExpanded = T,
           selectInput("plot_x", "x-axis", 
                       choices = NULL, selected = NULL, multiple = F, selectize = F),
           selectInput("plot_y","measure", 
                       choices = NULL, selected = NULL, multiple = F, selectize = F),
           radioButtons("plot_pos","Bar plot style", inline = T,
                        choices = c("stack","fill","dodge"), selected = "stack"),
           radioButtons("plot_type","Plot type (numerical only)", inline = T,
                        choices = c("density","jitter"), selected = "density")
  )

# filters
filter_controls <- 
  menuItem("Filters", icon = icon("filter"),
           div(id = "div_filters"),
           actionButton("bt_fapply", "filter", 
                        class = "btn btn-primary btn-sm", style = "color:#fff; display:inline-block"),
           actionButton("bt_fclear", "clear", 
                        class = "btn btn-warning btn-sm", style = "color:#fff; display:inline-block")
  )

# main page
main_content <- 
  fluidRow(
    tabBox(width = 8,
           tabPanel("Info",
                    includeMarkdown("include/info.md")),
           tabPanel("CTabs",
                    plotOutput("plot_examine"),
                    hr(),
                    tableOutput("table_examine")),
           tabPanel("Summary", tableOutput("table_summary")),
           tabPanel("XTable", DTOutput("table_grouped")),
           tabPanel("4Plot", plotOutput("plot_grouped"))
    ),
    box(width = 4,
        actionButton("bt_clear","reset", icon = icon("trash"),
                     class = "btn btn-sm"),
        DTOutput("table_vars")
    )
  )

dashboardPage(
  dashboardHeader(
    title = "Data Explorer"
  ),
  dashboardSidebar(
    sidebarMenu(
      plot_controls,
      filter_controls)
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    main_content
  )
)
