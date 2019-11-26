source("busy_indicator.R")
uploadServ <- function(env_serv) with(env_serv, local({
  
  # -------------------------------------------------------------------------------------File headers 
  observeEvent(input$fformat,{
    showModal(modalDialog(
      title = tags$h4("File Headers for the uploaded file"),
      
      rHandsontableOutput('formatInput'), br(),
      helpText("The above table shows how the file headers should be formated. Copy and paste above table headers into your excel file which will then be uploaded."),
      br(),
      p(tags$b("Required: "),"Lab_number"),
      
      tags$b("Optional: "),
      
        tags$div(tags$ul(
          tags$li(tags$span("Date_of_CMD_diagnostics")),
          tags$li(tags$span("EACMV_End_point")),
          tags$li(tags$span("ACMV_End_point")),
          tags$li(tags$span("Date_of_CBSV_diagnostics")),
          tags$li(tags$span("CBSV_Real_time")),
          tags$li(tags$span("UCBSV_Real_time"))
          )),
      
      easyClose = TRUE
    ))
  })
  
 
  output$formatInput <- renderRHandsontable({
    cols = c("Lab_number","Date_of_CMD_diagnostics","EACMV_End_point","ACMV_End_point","Date_of_CBSV_diagnostics","CBSV_Real_time","UCBSV_Real_time")
    dt = data.frame(t(cols))
    rhandsontable(dt, colHeaders = NULL)
  })
  
  # Initialize a variable to count how many time btn1 is clicked
  
  values <- reactiveValues(data = 1)
  
   
  # ------------------------------------------------------------------------------------------------- Step 1: File Input
  
  fileInput <- reactive({ # -------------- read file
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath, skip = 0, sheet = 1, col_names = TRUE, na = "", trim_ws = T, progress = readxl_progress())
    
  })
  
  # ---------------------------------------------------------------------------------------------------------- Step 2: Contents in DT
  
  output$contentsOut <- renderUI({
    
    div(
      if(is.null(input$file1)){return(NULL)}
      else {
        div(
          column(10,
                 panel_div(class_type = "default",
                     content = tags$div(
                       h5("File contents read"),
                       div(style = 'overflow-x: scroll',
                           DTOutput('contentsTbl')), # -------------------------------------- Raw data DT
                       uiOutput('contentsTbl_rows')
                     )), br(),br()
                 )
          )
        }
    )
  })
  
  # Show file contentsTbl
  
  contentsInput <- reactive({
    dt = fileInput()
    dt = dt[!duplicated(dt$Lab_number),] # remove duplicates by Lab number
    colnames(dt) = trimws(names(dt), which = "both")
    dt$Lab_number = as.integer(dt$Lab_number)
    dt[dt=="Positive"] <- 'positive'
    dt[dt=="Negative"] <- 'negative'
    dt %<>% mutate_all(as.character)
    return(dt)
  })
  
  output$contentsTbl <- renderDT({
    contentsInput()
    
  })
  
  # Show number of entries per page
  output$contentsTbl_rows <- renderUI({
    paste("Showing ", min(input$contentsTbl_rows_current), " to ", max(input$contentsTbl_rows_current), " of ", max(input$contentsTbl_rows_all), " entries.")
  })
  

  # ----------------------------------------------------------------------------------------------------------- Step 4: update
  output$updateOut <- renderUI({
    if(!is.null(input$file1)){
      div(
        column(7,
        withBusyIndicatorUI(
            actionBttn("update","Update", style = "jelly", color = "success", size = "sm")
            ),
        p("Click the update button to add",br()," the information to the existing data")
        ),
        column(5, br(),br(),br(),br(),
               uiOutput("goToTblOut")
        
        )
        
      )
    }
  })
  
  
  output$goToTblOut <- renderUI({
    Sys.sleep(1)
    if(input$update == 1){
      actionBttn("goTbl", "Go to Data Table", style = "material-flat", size="xs")
      
    }
  })
  observeEvent(input$update,{
    
    withBusyIndicatorServer("update", {
      Sys.sleep(1)
          if(file.exists("data/updatedMeristems.csv")){
            meristems = fread("data/updatedMeristems.csv")
          }else {
            meristems = meristems
            colnames(meristems) = gsub(" ","_", names(meristems))
          }
          meristems %<>% mutate_all(as.character)
          
          cols = names(contentsInput())[names(contentsInput()) %in% names(meristems)]
          updatedMeristems = merge(meristems,contentsInput(), by=cols, all.x=T)
          
          updatedMeristems <- updatedMeristems %>% 
            dplyr::group_by(Lab_number) %>%
            fill(everything(), .direction = "down") %>%
            fill(everything(), .direction = "up") %>%
            slice(1)
          
          fwrite(updatedMeristems, file = "data/updatedMeristems.csv", row.names = F)
    })
  })

})
)