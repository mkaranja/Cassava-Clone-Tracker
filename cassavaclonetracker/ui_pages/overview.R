panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}
overviewTab <- 
  tabPanel(title = "OVERVIEW", value = "overview",
           includeCSS("www/AdminLTE.css"),
           fluidRow(
             tags$style("#n_plants .small-box, #n_categories .small-box, #n_plants .small-box, #n_germination .small-box, #n_openfield_plantlets .small-box {cursor: pointer;}"),
             valueBoxOutput("n_source", width = 3),tags$style("#n_source"),
             valueBoxOutput("n_categories", width = 3),tags$style("#n_categories"),
             valueBoxOutput("n_plants", width = 3),tags$style("#n_plants") # tags$style("#n_crosses {width:220px;}")
             
           ), br(),
           
           fluidRow(
             
           ),
           fluidRow(
             
             
              panel_div(class_type = "default",
                        content = tags$div(
                          p("Glasshouse"),
                          column(4,
                                 highchartOutput("source")
                                 ),
                          column(4,
                                highchartOutput('categories')# %>% withSpinner()
                                ),
                          column(4,
                                 plotlyOutput('virusstatus')
                                 )
                        )
                )
             ),
           fluidRow(
             panel_div(class_type = "default",
                              content = tags$div(
                                
                              )
             )
          )
    )
