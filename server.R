shinyServer(function(input, output, session) {

  r <- reactiveValues(
    notpicked_dt = NISPUF14_VARS,
    picked_dt = data.frame()
  )
  
  # NOTPICKED -----------------------------------------------------
  output$tbl_notpicked <- renderDT({
    dt <- r$notpicked_dt %>% 
      mutate(`>` = "<button class='btn-sm btn-primary'><i class='fa fa-arrow-right'></i></button>")
    
    datatable(dt,
              style = "auto",
              rownames = F,
              selection = "none",
              escape = F,
              options = list(searching = T, paging = F, scrollY = "500")) %>% 
      formatStyle(3, cursor = "pointer")
  })
  observeEvent(input$tbl_notpicked_cell_clicked, {
    info <- input$tbl_notpicked_cell_clicked
    if(length(info) != 0) {
      if(info$col == 2){
        r$picked_dt <- rbind(r$picked_dt, r$notpicked_dt[info$row,])
        r$notpicked_dt <- r$notpicked_dt[-info$row,]
      }
    }
  })
  
  # PICKED --------------------------------------------------------
  output$tbl_picked <- renderDT({
    if(length(r$picked_dt) == 0) {
      dt <- r$picked_dt
    } else {
      dt <- r$picked_dt %>% 
        mutate(`<` = "<button class='btn-sm btn-primary'><i class='fa fa-arrow-left'></i></button>") %>% 
        select(`<`,key,description)
    }
    
    datatable(dt,
              style = "auto",
              rownames = F,
              selection = "none",
              escape = F,
              options = list(searching = F, paging = F, scrollY = "500"))
  })
  observeEvent(input$tbl_picked_cell_clicked, {
    info <- input$tbl_picked_cell_clicked
    if(length(info) != 0) {
      if(info$col == 0) {
        r$notpicked_dt <- rbind(r$notpicked_dt, r$picked_dt[info$row,])
        r$picked_dt <- r$picked_dt[-info$row,]
      }
    }
  })
  
  # VISUALIZE -----------------------------------------------------
  output$tbl_glimpse <- renderPrint({
    list_of_vars <- r$picked_dt$key
    
    #get_summary <- function(var) {  
    #  NISPUF14 %>% 
    #    group_by(.data[[var]]) %>% 
    #    summarise(n = n()) %>% 
    #    mutate(`%` = round(n/sum(n)*100,4))
    #}
    #lapply(list_of_vars,get_summary)
    
    df <- select(NISPUF14, list_of_vars)
    summary(df)
    
  })
  
  # ACTION BUTTONS -----------------------------------------------
  observeEvent(input$bt_clear, {
    r$notpicked_dt <-  NISPUF14_VARS
    r$picked_dt <-  data.frame()
  })
  
  observeEvent(input$bt_apply, {
    updateTabsetPanel(session, "tabs", selected = "tab2")
  })

})


