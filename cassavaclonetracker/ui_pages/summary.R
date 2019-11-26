summaryTab <-
  tabPanel(title = "Summary", value = "summary", icon = icon("arrow-right", lib="font-awesome"),
           
           # tags$hr(style="height:5px;border:2px;color:#000;background-color:#998900;"),
           
           fluidRow(
             tags$style("#plantsInGlasshouse .small-box, #n_totalseeds .small-box, #n_rescued .small-box, #n_germination .small-box, #n_openfield_plantlets .small-box {cursor: pointer;}"),
             valueBoxOutput("plantsInGlasshouse", width = 2),tags$style("#plantsInGlasshouse") # tags$style("#n_crosses {width:220px;}")
             
             #uiOutput("boxes")
               # print(dfSummary(result, graph.magnif = 1.0), 
               #      method = 'render',
               #      omit.headings = TRUE,
               #      bootstrap.css = FALSE)
           )
    )