
chartsServ <- function(env_serv) with(env_serv, local({

  # For one continuous variable:
  #   geom_area() for area plot
  # geom_density() for density plot
  # geom_dotplot() for dot plot
  # geom_freqpoly() for frequency polygon
  # geom_histogram() for histogram plot
  # stat_ecdf() for empirical cumulative density function
  # stat_qq() for quantile - quantile plot
  
  
  
  ## Two variables: Continuous X, Continuous Y
  # geom_point() for scatter plot
  # geom_smooth() for adding smoothed line such as regression line
  # geom_quantile() for adding quantile lines
  # geom_rug() for adding a marginal rug
  # geom_jitter() for avoiding overplotting
  # geom_text() for adding textual annotations
  
  # update inputs
  observeEvent(input$Barplot,{
    updateSelectInput(session, "xaxis","",  choices =  which(sapply(database, is.discrete)))},
    ignoreInit = TRUE
    )
  
   output$chart <- renderPlot({
    if(input$plot=="Barplot"){
                        
      database[[".v"]] <- factor(database[[input$xaxis]], levels=names(sort(table(database[[input$xaxis]]), descreasing=T)))
      p <- ggplot(database, aes(.v)) + geom_bar(fill = "steelblue", color ="steelblue") + xlab(input$xaxis) + theme_minimal()
    }
     
     if(input$plot=="Scatter"){
     p <- ggplot(database, aes_string(x = database[[input$xaxis]], y = database[[input$yaxis]])) + geom_point()
     
     if(!is.null(input$color)){
       p + geom_point(aes_string(database[[input$color]], shape=database[[input$color]]))
        }
      }
     
   p
  })
})
)



# if(is.numeric(database[[input$xaxis]]) && length(unique(database[[input$xaxis]]))>19){ 
#   if(input[[paste0("radio",input$xaxis)]] == "density"){ # continous var
#     ggplot(database, aes_string(names(database)[input$xaxis])) + 
#       geom_density(fill = "seashell", color = "seashell") + 
#       stat_density(geom = "line", size = 1) + 
#       theme_minimal() + theme(axis.title = element_text(size = 16))
#   }else{
#     ggplot(database, aes_string(names(database)[input$xaxis])) + 
#       geom_histogram(bins = input[[paste0("slider",input$xaxis)]]) + 
#       theme_minimal() + theme(axis.title = element_text(size = 16))
#   }
# }else{
#   database[[".x"]] <- 
#     factor(database[[input$xaxis]], levels = names(sort(table(database[[input$xaxis]]), 
#                                                         decreasing=TRUE)))
#   gg <- ggplot(database, aes(.x)) + geom_bar(fill = "steelblue", color ="steelblue") + 
#     geom_text(stat="count", aes(label=..count..), vjust=-0.5) + 
#     xlab(names(database)[input$xaxis]) + theme_minimal()
#   if(max(nchar(levels(database$.x)))*nlevels(database$.x)>40){
#     gg <- gg + theme(axis.text.x = 
#                        element_text(size = 12, angle = 45, 
#                                     vjust = 0.5, hjust = 0.5))
#   }else{
#     gg <- gg + theme(axis.text.x = element_text(size = 12))
#   }
#   gg + theme(axis.title = element_text(size = 16))
# }