shinyServer(function(input, output, session) {
  
  r <- reactiveValues(
    inserted_vars = NULL,
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
  
  observeEvent(input$table_vars_row_last_clicked, {
    r$picked_vars <- NISPUF14_VARS$key[input$table_vars_rows_selected]
    r$picked_df <- select(NISPUF14, r$picked_vars)
  })
  
  observeEvent(input$bt_clear, {
    r$picked_vars <-  NULL
  })
  
  # QUICK SUMMARY ------------------------------------------------
  output$text_summary <- renderPrint({
    if (length(r$picked_vars) != 0) {
      
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
  
  # GROUPED TABLE ------------------------------------------------
  output$table_grouped <- renderDT({
    if (length(r$picked_vars) != 0) {
      
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
    if(length(r$picked_vars) != 0) {
      
      df <- select(r$picked_df, where(is.factor))
      nm <- colnames(df)
      n <- ncol(df)
      
      if(n > 0) {
        colnames(df) <- paste0("c",1:n)
        if (n >= 1) {p <- ggplot(df, aes(c1)) + geom_bar() + labs(x = nm[1])} 
        if (n >= 2) {p <- p + geom_bar(aes(fill = c2)) + labs(fill = nm[2])}
        if (n >= 3) {p <- p + facet_grid(c3 ~ .)}
        if (n >= 4) {p <- p + facet_grid(c3 ~ c4)}
        p + guides(x = guide_axis(angle = 25))
      }
    }
  
  })
  
  # EXAMINE ---------------------------------------------------
  observeEvent(input$table_vars_row_last_clicked, {
    updateSelectInput(session, "s_xaxis", choices = r$picked_vars)
    n_picked <- length(r$picked_vars)
    n_inserted <- length(r$inserted_vars)

    if (n_inserted == 0) {
      var <- r$picked_vars
    } else { 
      var <- union(setdiff(r$picked_vars, r$inserted_vars), 
                   setdiff(r$inserted_vars, r$picked_vars))
    }
    
    if (n_picked > n_inserted) {
      id <- paste0("e_", var)
      lbl <- tolower(NISPUF14_VARS$description[NISPUF14_VARS$key == var])
      vec <- NISPUF14[[var]]
      if (is.factor(vec)) {
        elem <- selectInput(id, lbl, choices = levels(vec), multiple = T, selectize = F)
      } else if (is.numeric(vec)) {
        elem <- sliderInput(id, lbl, round = 1, 
                            min =  min(vec, na.rm = T), 
                            max = max(vec, na.rm = T), 
                            value = mean(vec, na.rm = T))
      }
      insertUI(
        selector = "#div_filters",
        ui = span(id = var, elem)
      )
      r$inserted_vars <- c(r$inserted_vars, var)
    } else {
      removeUI(
        selector = paste0("#",var)
      )
      r$inserted_vars <- r$inserted_vars[r$inserted_vars != var]
    }
  })
  
  observeEvent(input$bt_examine, {
    if (is.null(r$picked_vars) || length(r$picked_vars) == 0) {
      showModal(modalDialog(title = NULL, footer = NULL, easyClose = T,
                            "Pick at least one indicator"))
    } else {
      updateNavbarPage(session, "navbar", selected = "tab_examine") 
    }
  })
})
