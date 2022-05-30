shinyServer(function(input, output, session) {

  r <- reactiveValues(
    picked_vars = NULL
  )
  
  # EXPLORE -----------------------------------------------------
  output$tbl_vars <- renderDT({
    input$bt_clear
    
    datatable(NISPUF14_VARS,
              style = "auto",
              rownames = F,
              selection = list(mode = "multiple"),
              options = list(searching = T, paging = F, scrollY = "500"))
  })
  
  observeEvent(input$tbl_vars_rows_selected, {
    r$picked_vars <- NISPUF14_VARS$key[input$tbl_vars_rows_selected]
  })
  
  output$tbl_my_summary <- renderPrint({
    if(!is.null(r$picked_vars)) {
    
      get_summary <- function(vect) {
        if(n_distinct(vect, na.rm = T) == 1) {  # if only one value
          return(summary(vect))
        } else if(!is.factor(vect)) { # if is not a vector make into one
          vect <- cut(vect, unique(quantile(vect, na.rm = T)))
        }
        dat <- fct_count(vect)
        dat$`%` <- round(100*dat$n/sum(dat$n), 3)
        return(as.data.frame(dat))
      }
    
      df <- select(NISPUF14, r$picked_vars)
      lapply(df, get_summary)
    }
  })
  
  observeEvent(input$bt_clear, {
    r$picked_vars <-  NULL
  })
  
  # VISUALIZE ---------------------------------------------------------
  #insterted <- c()
  
  observeEvent(input$bt_visualize, {
    if(is.null(r$picked_vars) | length(r$picked_vars) == 0) {
      showModal(modalDialog(NULL,size = "s", footer = NULL, easyClose = TRUE,
        "Pick at least one indicator"
      ))
    } else {
      df <- NISPUF14 %>% 
        select(r$picked_vars) %>% 
        select(where(is.factor))
      factor_vars <- names(df)
      updateSelectInput(session,"s_group", choices = factor_vars)
      updateTabsetPanel(session, "tabs", selected = "visualize")
      
      make_selects <- function(fvar) {
        id = paste0("s_",fvar)
        lbl = fvar
        lvls =  levels(df[[fvar]])
        return(selectInput(id,lbl, choices = lvls, selectize = F, multiple = F))
      }
      select_bundle <- lapply(factor_vars,make_selects)

      insertUI(
        selector = "#dynamic_selects",
        multiple = T,
        ui = select_bundle
      )
      #inserted <<- c(id, inserted)
    }
  })
  
  observeEvent(input$bt_apply, {
  
  })
  
  output$table_main <- renderTable({
    
   NISPUF14 %>% 
      select(r$picked_vars) %>% 
      group_by_if(is.factor) %>% 
      summarise(freq = n(), 
                across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

  })
  
  output$plot_main <- renderPlot({
    #factor_vars <- names(select(r$df_main,where(is.factor)))
    #str(factor_vars)
    #r$df_main %>% 
    #ggplot(aes(x = .data[[factor_vars[1]]], y = freq, fill = .data[[factor_vars[2]]])) +
    #  geom_bar(stat = "identity") +
    #  facet_grid(rows = vars(.data[[factor_vars[3]]]), cols = vars(.data[[factor_vars[4]]]))
    
    
    df <- NISPUF14 %>% select(r$picked_vars) 
    factor_vars <- names(select(df,where(is.factor)))
    
    df %>% 
      select(where(is.factor)) %>% 
      ggplot(aes(x = .data[[factor_vars[1]]], fill = .data[[factor_vars[2]]])) +
        geom_bar(position = "fill") +
        facet_grid(rows = vars(.data[[factor_vars[3]]]), cols = vars(.data[[factor_vars[4]]])) +
        guides(x = guide_axis(angle = 25))
      
      
  })
})
