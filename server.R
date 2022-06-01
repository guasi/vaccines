shinyServer(function(input, output, session) {

  r <- reactiveValues(
    picked_vars = NULL,
    picked_df = NULL
  )
  
  # VARS TABLE ------------------------------------------------
  output$table_vars <- renderDT({
    input$bt_clear
    
    datatable(NISPUF14_VARS,
              style = "auto",
              rownames = F,
              selection = list(mode = "multiple"),
              options = list(searching = T, paging = F, scrollY = "500"))
  })
  
  observeEvent(input$table_vars_rows_selected, {
    r$picked_vars <- NISPUF14_VARS$key[input$table_vars_rows_selected]
    r$picked_df <- select(NISPUF14, r$picked_vars)
  })
  
  # QUICK SUMMARY ------------------------------------------------
  output$text_summary <- renderPrint({
    if (!is.null(r$picked_vars)) {
    
      get_summary <- function(vect) {
        if (!is.factor(vect)) {  
          quants <- unique(quantile(vect, na.rm = T))
          vect <- if (length(quants) > 1) cut(vect, quants) else factor(vect)
        }
        dat <- fct_count(vect)
        dat$`%` <- round(100*dat$n/sum(dat$n), 3)
        return(as.data.frame(dat))
      }
      
      lapply(r$picked_df, get_summary)
    }
  })
  
  observeEvent(input$bt_clear, {
    r$picked_vars <-  NULL
  })
  
  # GROUPED TABLE ------------------------------------------------
  output$table_grouped <- renderDT({
    if (!is.null(r$picked_vars)) {
      dt <- r$picked_df %>% 
        group_by_if(is.factor) %>% 
        summarise(
          freq = n(), 
          across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
      
      datatable(dt,
                style = "auto",
                rownames = F,
                selection = "none",
                options = list(searching = F, paging = F, scrollY = "500", scrollX = T))
    }
  })
  
  # GROUPED PLOT ------------------------------------------------
  output$plot_grouped <- renderPlot({
    if(!is.null(r$picked_vars)) {
      
      df <- select(r$picked_df, where(is.factor))
      #df <- select(NISPUF14[17:21], where(is.factor))
      nm <- colnames(df)
      n <- ncol(df)
      colnames(df) <- paste0("c",1:n)
      
      if (n >= 1) {p <- ggplot(df, aes(c1)) + geom_bar() + labs(x = nm[1])} 
      if (n >= 2) {p <- p + geom_bar(aes(fill = c2)) + labs(fill = nm[2])}
      if (n >= 3) {p <- p + facet_grid(~c3, drop = T)}
      if (n >= 4) {p <- p + facet_grid(vars(c3), vars(c4), drop = T)}
      p + guides(x = guide_axis(angle = 25))
    }
    
  })
  
  # EXAMINE ---------------------------------------------------
})
