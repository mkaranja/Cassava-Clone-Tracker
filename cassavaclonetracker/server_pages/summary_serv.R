
summaryServ <- function(env_serv) with(env_serv, local({
  

  output$plantsInGlasshouse <- renderValueBox({
    result <- setDT(database)[,c("IITA Cassava Kephis ICK", "Date brought to KEPHIS","Bay in Glasshouse", "Number received glasshouse")]
    
    result <- plantsInGlasshouseInput() %>%
      dplyr::group_by(Mother, Father) %>%
      dplyr::tally()
    
    box1<-valueBox(value=nrow(bunchesBox()),
                   width=1,
                   color = "teal",
                   href="#",
                   subtitle=HTML("<b>Number of plants in glasshouse</b><br>", nrow(result), " Unique ICK")
    )
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_plantsInGlasshose"
    return(box1)
  })
  
  observeEvent(input$button_plantsInGlasshose, {
    updateTabItems(session, "tabs", selected = "table")
  })
  
  
  
  num = which(sapply(database, is.numeric))
  fac = which(sapply(database, is.factor))
  dat <- database[, c(fac, num)] # keep only numeric and factors
  
  # boxes
  output$boxes <- renderUI({
    a <- lapply(1:ncol(dat), function(x) {
      
      column(width = 3, style='border: 1px solid #98FB98; padding:5px', offset = 1, 
          id = paste0("box",x),
          title = names(dat)[x], br(),
          plotOutput(paste0("plot",x), height = "250px") %>% withSpinner()
      )
    })
    tagList(a)
  })
  
  
  # plots in boxes ####
  for(i in 1:ncol(dat)){
    local({
      ii <- i
      output[[paste0("plot",ii)]] <- renderPlot({
        if(is.numeric(dat[[ii]]) && length(unique(dat[[ii]]))>19){ 
          if(input[[paste0("radio",ii)]] == "density"){ # continous var
            ggplot(dat, aes_string(names(dat)[ii])) + 
              geom_density(fill = "seashell", color = "seashell") + 
              stat_density(geom = "line", size = 1) + 
              theme_minimal() + theme(axis.title = element_text(size = 16))
          }else{
            ggplot(dat, aes_string(names(dat)[ii])) + 
              geom_histogram(bins = input[[paste0("slider",ii)]]) + 
              theme_minimal() + theme(axis.title = element_text(size = 16))
          }
        }else{
          dat[[".x"]] <- 
            factor(dat[[ii]], levels = names(sort(table(dat[[ii]]), 
                                                  decreasing=TRUE)))
          gg <- ggplot(dat, aes(.x)) + geom_bar(fill = "steelblue", color ="steelblue") + 
            geom_text(stat="count", aes(label=..count..), vjust=-0.5) + 
            xlab(names(dat)[ii]) + theme_minimal()
          if(max(nchar(levels(dat$.x)))*nlevels(dat$.x)>40){
            gg <- gg + theme(axis.text.x = 
                               element_text(size = 12, # angle = 45, 
                                            vjust = 0.5, hjust = 0.5)) + coord_flip()
          }else{
            gg <- gg + theme(axis.text.x = element_text(size = 12))
          }
          gg + theme(axis.title = element_text(size = 16))
        }
      })
    })
  }

})
)
  