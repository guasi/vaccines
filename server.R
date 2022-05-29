shinyServer(function(input, output, session) {

  r <- reactiveValues(
    picked_keys = NULL
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
    row_selected <- input$tbl_vars_rows_selected
    r$picked_keys <- NISPUF14_VARS$key[row_selected]
  })
  
  output$tbl_my_summary <- renderPrint({
    if(!is.null(r$picked_keys)) {
    
      get_summary <- function(vect) {
        if(n_distinct(vect, na.rm = T) == 1) {  # if only one value
          return(summary(vect))
        } else if(!is.factor(vect)) { # if is not a vector make into one
          vect <- cut(vect, unique(quantile(vect, na.rm = T)))
        }
        dat <- fct_count(vect)
        dat$`%` <- round(100*dat$n/sum(dat$n),3)
        return(as.data.frame(dat))
      }
    
      df <- select(NISPUF14,r$picked_keys)
      lapply(df, get_summary)
    }
  })
  
  observeEvent(input$bt_clear, {
    r$picked_keys <-  NULL
  })
  
  observeEvent(input$bt_visualize, {
    if(is.null(r$picked_keys) | length(r$picked_keys) == 0) {
      showModal(modalDialog(NULL,size = "s", footer = NULL, easyClose = TRUE,
        "Pick at least one indicator"
      ))
    } else {
      updateTabsetPanel(session, "tabs", selected = "visualize")
    }
  })
  
  # VISUALIZE ---------------------------------------------------------
  
  # make a filter for factors in picked_keys
  output$factor_filters <- renderUI({
    df_factors <- NISPUF14 %>% 
      select(r$picked_keys) %>% 
      select(where(is.factor))
    lbls <- NISPUF14_VARS[names(df_factors),"description"]
    selectInput("s_group","group by", choices = lbls, multiple = F, selectize = F)
  })

})


