panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}


labelsTab <- 
  tabPanel("LABELS", value = "labels",
           column(1),
           column(11,
           
           sidebarLayout(position = "right",
             sidebarPanel(width = 4,
                          fluidRow(
                            useShinyjs(),
                            uiOutput("controlsOut")
                          )
               
             ),
             mainPanel(width = 8,
                       
                 tabsetPanel(id = "labeltabs", type = "pills",
                             
                     tabPanel("Assigning ICK",br(),
                              panel_div(class_type = "default",
                                        content = tags$div(
                                          DTOutput("assignTbl"), br(),
                                          uiOutput('saveOut23')
                                      )
                              )
                     ),           
                     
                     tabPanel("Add New Plants Labels",br(),
                              panel_div(class_type = "default",
                                        content = tags$div(
                                          DTOutput("newplantsTbl"), br(),
                                          uiOutput('saveOut24')
                                        )
                              )
                     ), 
                     
                     tabPanel("Meristems & Tips", br(),
                              panel_div(class_type = "default",
                                        content = tags$div(
                                          div(style = 'overflow-x: scroll',
                                              DTOutput("meristemsTable"),
                                              tableOutput('tbl')
                                          )
                                          
                                        )
                              )
                     )
                 ))
                       
             )
           )
  )


    
  