# Initialize a ggplot ggplot()
# Scatter plot geom_point()
# Box plot geom_boxplot()
# Violin plot geom_violin()
# strip chart geom_jitter()
# Dot plot geom_dotplot()
# Bar chart geom_bar() or geom_col()
# Line plot geom_line()
# Histogram geom_histogram()
# Density plot geom_density()

panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

chartsTab <- 
  tabPanel(title = "CHARTS", value = "charts", iicon = icon("bar-chart-o"),
            
     sidebarLayout(
       sidebarPanel(width=3,
                    
                    panel_div(class_type = "default",
                              content = tags$div(
                                
           selectInput("plot", "", c("Barplot","Histogram","Density","Boxplot","Violin plot", "strip chart",
                                       "Line plot", "", "Area")),
           selectInput('xaxis', 'X-axis',names(summaryTable)), 
           
            conditionalPanel(
              condition = "input.plot=='Density'",
              selectInput("dvar", "Letter", c("Select var" = "", which(sapply(summaryTable, is.numeric))))
            ),
            conditionalPanel(
              condition = "input.plot=='Scatter'",
                 selectInput('yaxis', "Y-axis", which(sapply(summaryTable, is.numeric))),
                 prettySwitch("color", "Color/ group")
            ),
            conditionalPanel(
              condition = "input.plot=='Barplot'",
              selectInput('discrete_var', '',c("Select var"="", names(summaryTable))) #which(sapply(summaryTable, is.discrete))
              
                 
            ),
            conditionalPanel(
              condition = "input.plot=='Histogram'",
              selectInput('color', 'Color (Group by)',names(summaryTable)),
              numericInput('nbars', 'Number of bars', value = 5, min=2, max=100)
            ),
           textInput("main","",placeholder = "Title"),
           textInput("subtitle","",placeholder = "sub title"),
           actionBttn('run','Run', icon = icon('play'))
                              ))
       ),
       
       mainPanel(
         panel_div(class_type = "default",
                   content = tags$div(
                     plotOutput('chart')# %>% withSpinner(), br()
                     
                   )
         )
         
       )
     )
  ) 
  
  
  