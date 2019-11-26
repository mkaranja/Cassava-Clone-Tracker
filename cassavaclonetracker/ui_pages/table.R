panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}

tableTab <- 
  
  
  tabPanel(title = "DATA TABLES", value = "table",
        shinyjs::useShinyjs(),
        
        
        sidebarLayout(
          sidebarPanel(width = 2,
                       panel_div(class_type = "default",
                                 content = tags$div(
                                   uiOutput("variablesOut"),br(),br(),
                                   uiOutput('random_labels_Out'), br(),
                                   uiOutput("downloadDataOut"),
                                   verbatimTextOutput('txt3'),
                                   verbatimTextOutput('txt4')
                                   
                               )
                       )
                       
                       
          ),
          
          mainPanel(width = 10,
              fluidRow(
                tabsetPanel(id = "ttabs", type = "pills",
                    tabPanel("Summary Table",br(), 
                         panel_div(class_type = "default",
                               content = tags$div(
                                div(style = 'overflow-x: scroll',
                                    dataTableOutput("summaryTbl") %>% withSpinner(),
                                    bsTooltip("summaryTbl", 
                                              paste(tags$div(tags$ul(
                                                tags$li(tags$span("Sort the data table by clicking up & down arrows next to column names")),
                                                tags$li(tags$span("Subset data table using the table filters below the column names")),
                                                tags$li(tags$span("Click on any data table row to generate barcode label for the ICK in that row. Look for 'LABELS' of the left")),
                                                tags$li(tags$span("Date_of_CBSV_diagnostics")),
                                                tags$li(tags$span("CBSV_Real_time")),
                                                tags$li(tags$span("UCBSV_Real_time"))
                                                  ))
                                                ), placement = "right", trigger = "hover"
                                              )
                                    ),
                                uiOutput("summary_rows"), br()
                               )
                            )
                    ),
                    tabPanel("Meristems Data",br(), 
                             panel_div(class_type = "default",
                                       content = tags$div(
                                         div(style = 'overflow-x: scroll',
                                             dataTableOutput("meristemsTbl")  %>% withSpinner()
                                         ), 
                                         uiOutput("meristems_rows"), br()
                                       )
                             )
                    )
                   
            ) # End TabSet
        )
      )
        )
  )


