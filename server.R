shinyServer(function(input, output, session) {
  
  # GLOBAL & REACTIVE VARS -----------------------------------
  r <- reactiveValues(
    picked_vars = NULL,
    picked_df   = NULL,
    redraw      = NULL
  )
  
  # SELECT TABLE -----------------------------------------------
  output$table_vars <- renderDT({
    r$redraw
    datatable(MDATA_VARS,
              style     = "auto",
              rownames  = F,
              selection = list(mode = "multiple"),
              options   = list(searching = T, paging = F, scrollY = "500"))
  })
  
  observeEvent(input$bt_clear, {
    req(r$picked_vars)
    
    for(var in r$picked_vars) {
      removeUI(selector = paste0("#s_",var))
      removeUI(selector = paste0("#p_",var))
    }
    updateSelectInput(session, "plot_x", choices = character(0))
    updateSelectInput(session, "plot_y", choices = character(0))
    r$picked_vars <- NULL
    r$picked_df   <- NULL
    r$redraw      <- input$bt_clear
    
  })
  
  # SELECT EVENT ---------------------------------------------------
  observeEvent(input$table_vars_row_last_clicked, {
    r$picked_vars <- MDATA_VARS$key[input$table_vars_rows_selected]
    r$picked_df   <- MDATA[r$picked_vars]
    var           <- MDATA_VARS$key[input$table_vars_row_last_clicked]

    # update plot inputs
    freezeReactiveValue(input, "plot_x")
    freezeReactiveValue(input, "plot_y")
    m <- if (length(r$picked_vars) > 1) r$picked_vars[2] else r$picked_vars[1]
    updateSelectInput(session, "plot_x", choices = r$picked_vars)
    updateSelectInput(session, "plot_y", choices = r$picked_vars, selected = m)  
    
    # update filter inputs and mini plots
    if (var %in% r$picked_vars) {
      lbl <- paste0(var,": ",tolower(MDATA_VARS$lbl[MDATA_VARS$key == var]))
      vec <- r$picked_df[[var]]
      
      if (is.factor(vec)) {
        plt  <- renderPlot(plot(vec, 
                                main = var, 
                                las = 3, 
                                cex.axis = .6, 
                                cex.names = .6, 
                                mgp = c(1.5,.5,0)),
                           width = 170, height = 200)
        elem <- selectInput(var, lbl, 
                            choices   = levels(vec), 
                            multiple  = T, 
                            selectize = T)
        
      } else if (is.numeric(vec)) {
        plt  <- renderPlot(hist(vec, 
                                main = var, 
                                las = 3, 
                                cex.axis = .6, 
                                cex.lab = .6, 
                                xlab = NULL, 
                                mgp = c(1.5,.5,0)), 
                           width = 170, height = 200)
        elem <- sliderInput(var, lbl, 
                            round = 1, 
                            min   = min(vec, na.rm = T), 
                            max   = max(vec, na.rm = T), 
                            value = range(vec, na.rm = T))
      }
      
      insertUI(selector = "#div_miniplots",
               ui       = div(id = paste0("p_",var), plt))
      insertUI(selector = "#div_filters", 
               ui       = div(id = paste0("s_",var), elem))
    } else {
      removeUI(selector = paste0("#s_",var))
      removeUI(selector = paste0("#p_",var))
    }
    
    # Switch tabs if on Info
    if (input$mainbox == "Info") showTab("mainbox", target = "CTabs", select = T)
    
  })
  
  observeEvent(input$bt_fapply, {
    filter_vars <- r$picked_vars[sapply(r$picked_vars, \(x) (!is.null(input[[x]])))]
    
    if (length(filter_vars) > 0) {
      r$picked_df <- MDATA %>%
        select(r$picked_vars) %>% 
        filter((if_all(
          .cols = all_of(filter_vars),
          .fns  = ~ if (is.numeric(.x)) {
                       .x >= input[[cur_column()]][1] & .x <= input[[cur_column()]][2]
                     } else {
                       .x %in% input[[cur_column()]]
                     }
        )))
    }
  })
  
  observeEvent(input$bt_fclear, {
    r$picked_df <- select(MDATA, r$picked_vars)
    
    for(var in r$picked_vars) {
      vec <- r$picked_df[[var]]
      if (is.factor(vec))
        updateSelectInput(session, var, selected = character(0))
      else if (is.numeric(vec))
        updateSliderInput(session, var, value = range(vec, na.rm = T))
    }
  })
  
  # TAB SUMMARY ------------------------------------------------
  output$table_summary <- renderTable({
    req(r$picked_vars)
    
    r$picked_df %>% 
      df_summary() %>% 
      mutate_all(linebreak_html)
    
  }, striped = T, hover = T, spacing = "s", align  = "llrr", 
  sanitize.text.function = identity)
  
  # TAB FACETS -------------------------------------------------
  output$plot_grouped <- renderPlot({
    req(r$picked_vars)
    
    dat <- select(r$picked_df, where(is.factor))
    nm  <- colnames(dat)
    n   <- ncol(dat)
    
    if(n > 0) {
      colnames(dat)  <- paste0("c",1:n)
      if (n >= 1) {p <- ggplot(dat, aes(c1)) + geom_bar() + labs(x = nm[1])} 
      if (n >= 2) {p <- p + geom_bar(aes(fill = c2)) + labs(fill = nm[2])}
      if (n >= 3) {p <- p + facet_grid(c3 ~ .)}
      if (n >= 4) {p <- p + facet_grid(c3 ~ c4)}
      p + guides(x = guide_axis(angle = 25))
    }
    
  })
  
  # TAB CONTINGENCY -------------------------------------------------
  output$plot_examine <- renderPlot({
    req(r$picked_vars, input$plot_x)

    n   <- length(r$picked_df)
    m1  <- sym(input$plot_x)
    m2  <- sym(input$plot_y)
    dat <- r$picked_df
    p   <- dat %>% ggplot(aes(x = !!m1))
    
    if (is.factor(dat[[m1]])) {
      if (n == 1) {p <- p + geom_bar()}
      if (n >= 2) {
        if (is.factor(dat[[m2]]))       {p <- p + geom_bar(aes(fill = !!m2), position = input$plot_pos)}
        else if (is.numeric(dat[[m2]])) {p <- p + geom_boxplot(aes(y = !!m2))}
      }
      p <- p + scale_x_discrete(labels = \(x) stringr::str_wrap(x, width = 20))
    } else if (is.numeric(dat[[m1]])) {
      if (n == 1)  {p <- p + geom_density()} # cannot be jitter
      if (n >= 2 && input$plot_type == "jitter")  {p <- p + geom_jitter(aes(y = !!m2))}
      if (n >= 2 && input$plot_type == "density") {p <- p + geom_density(aes(fill = !!m2, color = !!m2), alpha = 1/4)}
    }
    p + theme(legend.position = "bottom")
  })
  
  output$table_examine <- renderTable({
    req(r$picked_vars, input$plot_x)
    
    m1  <- sym(input$plot_x)
    m2  <- sym(input$plot_y)
    dat <- r$picked_df %>% select(!!m1,!!m2)
    dat <- as.data.frame(lapply(dat, make_factor))
    
    if (ncol(dat) == 1) {
      Freq <- table(dat)
      `%`  <- 100*prop.table(Freq)
      cbind(Freq, `%`)
    } else {
      f           <- xtabs(~., dat, drop.unused.levels = T, addNA = T)
      p           <- round(100*prop.table(f, 1), 0)
      fp          <- paste0(f, " (", p, "%)") 
      rownames(f) <- tidyr::replace_na(rownames(f), "NA") # fix for xtable 
      matrix(fp, nrow = nrow(f), ncol = ncol(f), dimnames =  dimnames(f))
    }
  }, rownames = T, striped = T, hover = T, spacing = "s", align = "r")
  
})
