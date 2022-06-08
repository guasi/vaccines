shinyServer(function(input, output, session) {
  
  # GLOBAL & REACTIVE VARS -----------------------------------
  
  r <- reactiveValues(
    picked_vars = NULL,
    picked_df = NULL,
    redraw = NULL
  )
  
  # SELECT TABLE -----------------------------------------------
  output$table_vars <- renderDT({
    r$redraw
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
    if (length(r$picked_vars) > 0) {
      
      for(var in r$picked_vars) {
        removeUI(selector = paste0("#s_",var))
      }
      updateSelectInput(session, "s_xaxis", choices = character(0))
      updateSelectInput(session, "s_measures", choices = character(0))
      r$picked_vars <- NULL
      r$picked_df <- NULL
      r$redraw <- input$bt_clear
    }
  })
  
  # QUICK SUMMARY ------------------------------------------------
  output$text_summary <- renderPrint({
    if (length(r$picked_vars) > 0) {
      
      get_summary <- function(vec) {
        if (!is.factor(vec)) {  
          quants <- unique(quantile(vec, na.rm = T))
          vec <- if (length(quants) > 1) cut(vec, quants) else factor(vec)
        }
        dat <- forcats::fct_count(vec)
        dat$`%` <- round(100*dat$n/sum(dat$n), 3)
        return(as.data.frame(dat))
      }
      
      lapply(r$picked_df, get_summary)
      
    }
  })
  
  # GROUPED TABLE ------------------------------------------------
  output$table_grouped <- renderDT({
    if (length(r$picked_vars) > 0) {
      
      dat <- r$picked_df %>% 
        group_by_if(is.factor) %>%
        summarise(Freq = n(),
                  Prop = round(Freq/nrow(r$picked_df),2),
                  across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                  .groups = "drop")
      
      datatable(dat,
                style = "auto",
                rownames = F,
                selection = "none",
                options = list(searching = F, paging = F, scrollY = "500", scrollX = T))
    }
  })
  
  # GROUPED PLOT ------------------------------------------------
  output$plot_grouped <- renderPlot({
    if(length(r$picked_vars) > 0) {
      
      dat <- select(r$picked_df, where(is.factor))
      nm <- colnames(dat)
      n <- ncol(dat)
      
      if(n > 0) {
        colnames(dat) <- paste0("c",1:n)
        if (n >= 1) {p <- ggplot(dat, aes(c1)) + geom_bar() + labs(x = nm[1])} 
        if (n >= 2) {p <- p + geom_bar(aes(fill = c2)) + labs(fill = nm[2])}
        if (n >= 3) {p <- p + facet_grid(c3 ~ .)}
        if (n >= 4) {p <- p + facet_grid(c3 ~ c4)}
        p + guides(x = guide_axis(angle = 25))
      }
    }
  
  })
  
  # EXAMINE ---------------------------------------------------
  observeEvent(input$table_vars_row_last_clicked, {
    var <- NISPUF14_VARS$key[input$table_vars_row_last_clicked]
    
    # update plot inputs
    m <- if(length(r$picked_vars) > 1) r$picked_vars[2] else r$picked_vars[1]
    updateSelectInput(session, "s_xaxis", choices = r$picked_vars)
    updateSelectInput(session, "s_measures", choices = r$picked_vars, selected = m)
    
    # update filter inputs; add/remove last clicked
    if (var %in% r$picked_vars) {
      lbl <- paste0(var,": ",tolower(NISPUF14_VARS$lbl[NISPUF14_VARS$key == var]))
      vec <- r$picked_df[[var]]
      if (is.factor(vec)) {
        elem <- selectInput(var, lbl, choices = levels(vec), multiple = T, selectize = T)
      } else if (is.numeric(vec)) {
        elem <- sliderInput(var, lbl, round = 1, 
                            min =  min(vec, na.rm = T), 
                            max = max(vec, na.rm = T), 
                            value = range(vec, na.rm = T))
      }
      insertUI(selector = "#div_filters", 
               ui = span(id = paste0("s_",var), elem))
    } else {
      removeUI(selector = paste0("#s_",var))
    }
  })
  
  observeEvent(input$bt_examine, {
    if (length(r$picked_vars) > 0) {
      updateNavbarPage(session, "navbar", selected = "tab_examine") 
    }
  })
  
  output$plot_examine <- renderPlot({
    n <- length(r$picked_df)
    if (n > 0) {
      
      m1 <- input$s_xaxis
      m2 <- input$s_measures
      dat <- r$picked_df
      p <- dat %>% ggplot(aes(x = .data[[m1]]))
      
      if (is.factor(dat[[m1]])) {
        if (n == 1) {p <- p + geom_bar()}
        if (n >= 2) {p <- p + geom_bar(aes(fill = .data[[m2]]), position = input$plot_pos)}
      } else if (is.numeric(dat[[m1]])) {
        if (n == 1) {p <- p + geom_density()} 
        if (n >= 2 && input$plot_type == "jitter") {p <- p + geom_jitter(aes(y = .data[[m2]]))}
        if (n >= 2 && input$plot_type == "density") {p <- p + geom_density(aes(fill = .data[[m2]], color = .data[[m2]]), alpha = 1/4)}
      }
      p + guides(x = guide_axis(angle = 25)) +
        theme(legend.position = "bottom")
    }
    
  })
  
  observeEvent(input$bt_apply, {
    filter_vars <- r$picked_vars[sapply(r$picked_vars, \(x) !is.null(input[[x]]))]
    
    if (length(filter_vars) > 0) {
      r$picked_df <- NISPUF14 %>%
        select(r$picked_vars) %>% 
        filter((if_all(
            .cols = all_of(filter_vars),
            .fns  = ~ .x %in% input[[cur_column()]])))
    }
  })
})