
overviewServ <- function(env_serv) with(env_serv, local({
  
  
  # ValueBoxes
  newPlants = database[,c("IITA Cassava Kephis ICK","Unique Cassava ID UCID", "Date brought to KEPHIS","Number received", "Date of CMD visual symptoms")]
  
  output$n_plants <- renderValueBox({
    result <- newPlants %>%
      dplyr::group_by(`IITA Cassava Kephis ICK`) %>%
      dplyr::filter(is.na(`Date of CMD visual symptoms`),`Number received` > 0)
    
    box1<-valueBox(value=nrow(result),
                   color = "teal",
                   href="#",
                   subtitle="New Plants"
                   
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_n_plants"
    return(box1)
  })
  
  output$virusstatus  <- renderPlotly({
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    status = database[,c("IITA Cassava Kephis ICK","Virus status")]
    grp_status = data.frame(table(status$`Virus status`))
    colnames(grp_status) = c("Status","Number")
    row.names(grp_status) = grp_status$Status
    
    # 
    plot_ly(grp_status, labels = ~Status, values = ~Number, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~Number,
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE) %>%
      layout(title = 'Glasshouse Virus Status',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
 
  })
  
  
  
  # -------------------------------------
  # output$categories <- renderPlotly({
  #   colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  #   
  #   category = database[,c("IITA Cassava Kephis ICK","Category")]
  #   grp_category = data.frame(table(category$Category))
  #   grp_category = grp_category[!grp_category$Var1 == "Unknown",]
  #   colnames(grp_category) = c("Category","Number")
  #   row.names(grp_category) = grp_category$Category
  #   
  #   # 
  #   plot_ly(grp_category, x = ~Category, y = ~Number, type = 'bar') 
  #   
  # })
  js_category_bar_clicked <- JS("function(event) {Shiny.onInputChange('category_bar_clicked', [event.point.category]);}")
  
  output$categories <- renderHighchart({
    
      result <- database[,c("IITA Cassava Kephis ICK","Category")] %>%
        .[complete.cases(.),] %>%
        dplyr::group_by(Category) %>%
        dplyr::tally() %>%
        dplyr::arrange(n) %>%
        dplyr::collect()
      result %<>%
        dplyr::filter(!Category == "Unknown")
      
      # paste0("<a href='",result$FemaleGermplasmDbId,"'>",'photo',"</a>")
      highchart() %>%
        hc_add_series(data = result$n,type = "bar", name = "Category", events = list(click = js_category_bar_clicked)) %>%
        hc_xAxis(categories = result$Category) %>%
        hc_exporting(enabled = TRUE) %>% 
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", valueDecimals=0,
                   shared = TRUE, borderWidth = 2) %>%
        hc_title(text="Clone Category") %>%
        hc_add_theme(hc_theme_elementary())
    
  })
  
})
)
  