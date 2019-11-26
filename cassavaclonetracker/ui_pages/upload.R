source("busy_indicator.R")
uploadTab <- 
  tabPanel("UPLOAD DATA", value = "upload",
           introjsUI(),
    sidebarLayout(position = "left",
        sidebarPanel(
          style = "position:fixed;width:inherit;",
          width = 3,
              panel_div(class_type = "default",
                  content = tags$div(
                    column(11, offset = 1,
                                
                      fluidRow(
                        column(4, offset = 8,
                        actionBttn('fformat', 'File format', style = "material-flat", size = "xs")),
                        shinyBS::bsTooltip("fformat", "Click here to see the right file format to use",
                                  "right", options = list(container = "body")),
                        tags$head(tags$style(".modal-dialog{ width:800px}")),
                        fileInput('file1', tags$b('Select data file:'), multiple = F, width = "100%"),
                        
                         br(),br(),br(),
                        
                        uiOutput("updateOut")
                        
                      )
                    ))     
                    )
                  ),
      mainPanel(width = 9,
                uiOutput("contentsOut")
      )
    )
  )