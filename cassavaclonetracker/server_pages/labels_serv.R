panel_div <- function(class_type, content) {
  div(class = sprintf("panel panel-%s", class_type),
      div(class = "panel-body", content)
  )
}
library(shinyWidgets)

labelsServ <- function(env_serv) with(env_serv, local({
  
  
  # Controls
  
  output$controlsOut <- renderUI({
    
    div(
      if(input$labeltabs == 'Assigning ICK'){
        div(
          panel_div(class_type = "default",
                    content = tags$div(
                      helpText("This table show the most recent ID generated for the new plants"),
                      numericInput("n1plants", "Number of ICKs (unique plants)", value=1),
                      numericInput("n1sticks", "# Copies per plant", value = 1),
                      downloadButton("downloadassigned","PDF", color = "primary", size="sm", style = "unite")
                    ))
        )
      },
      
      
      if(input$labeltabs == 'Add New Plants Labels'){
        div(
          panel_div(class_type = "default",
                    content = tags$div(
                      helpText("This table show the most recent ID generated for the new plants"),
                      
                      numericInput("copies", "Number of copies", value = 1),br(),
                      
                      downloadButton("downloadnewplants","PDF", color = "primary", size="sm", style = "unite"), br(),br(),
                      bsTooltip("downloadnewplants",title = paste("Paper: US-letter with 10 rows by 1 column.", br(), 
                        " In the print dialogue box, select paper size 'US-letter' and change 'custom scale' to 98%"), trigger = "hover")
                    ))
        )
      },
      
      
      if(input$labeltabs == 'Meristems & Tips'){
        div(
          panel_div(class_type = "default",
                    content = tags$div(
                      helpText("This section requires you to download data in excel file and print barcode labels using the desktop label printer ZT410."), br(),
                      
                      selectizeInput("select_meristem", "Stage: ", c("", unique(as.character(cleantable$Stage)))),
                      uiOutput("meristemCopiesOut"),
                      br(),
                      downloadButton("downloadmeristems","Excel File", color = "primary", size="sm", style = "unite")
                    ))
        )
      }
    )
  })
  
  

  # ----------------------------------------------------------- Assign ICKs ------------------------------------------------------------------------
  
  
  assignICKInput <- reactive({
    req(input$n1plants); req(input$n1sticks)
    
    dt = database[,c("IITA Cassava Kephis ICK","Unique Cassava ID UCID")] %>%
      setorder(., -"IITA Cassava Kephis ICK")
    dt = dt[1,]
    dt$status = 'Current'
    
    # Number of ICKs
    dt = as.data.frame(dt)[rep(row.names(dt), input$n1plants),] 
    dt = data.table::setDT(dt)
    dt = dt[,index := 1:.N, by = `IITA Cassava Kephis ICK`]
    dt$`IITA Cassava Kephis ICK` = paste0("ICK",(as.numeric(gsub("ICK","", dt$`IITA Cassava Kephis ICK`))+dt$index))
    
    # Number of sticks per ICK
    dt = data.frame(dt)
    dt = dt[rep(row.names(dt), input$n1sticks),] 
    colnames(dt) = c("IITA Cassava Kephis ICK","Unique Cassava ID UCID","status","index")
    dt = setorder(dt,"IITA Cassava Kephis ICK")
    
    if(input$n1plants>1){
      dt$status = ''
    }
    dt[,c("Unique Cassava ID UCID","status")] = ""
    dt
  })
  
  
  output$assignTbl <- renderDT({
    
    DT::datatable(assignICKInput(), filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE, orderClasses = TRUE
                  ))
  })
  
  output$downloadassigned <- downloadHandler(
    filename = paste("Assign ICK Labels.pdf"),
    
    content = function(file) {
      pdf(file, width=8.5, height = 11, paper = 'a4', pagecentre=F) # right align width=6.0 # left width=2.0,
      par(mfrow=c(10, 4),mar=c(1,1,1,1), oma=c(0.5,0.5,0.5,0.5)) # right align mar=c(0,30,3,0)
      for(i in 1:(nrow(assignICKInput()))){
        image(qrencode_raster(as.character(assignICKInput()[i,1])), # QRcode
              cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F, 
              xlab="", ylab="", subtitle = mtext(as.character(assignICKInput()[i,1]), side = 3, line = 0, outer = F, at = NA, adj = .5, padj = 0, cex = 1, col = 1, las=1, font = 10))
        
      }
      dev.off()
    }
  )
  
  # ----------------------------------------------------------- New plants ------------------------------------------------------------------------
 
  
  output$newplantsTbl <- renderDT({
    dt = database[,c("IITA Cassava Kephis ICK","Unique Cassava ID UCID","Category", "Date brought to KEPHIS","Number received", "Date of CMD visual symptoms")] %>%
      dplyr::filter(is.na(`Date of CMD visual symptoms`),`Number received` > 0) %>%
      dplyr::select(-`Date of CMD visual symptoms`)
    
    DT::datatable(dt, filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE, orderClasses = TRUE))
  })
  
  downloadNewPlantsInput <- reactive({
    dt = database[,c("IITA Cassava Kephis ICK","Unique Cassava ID UCID","Category", "Date brought to KEPHIS","Number received", "Date of CMD visual symptoms")] %>%
      dplyr::filter(is.na(`Date of CMD visual symptoms`), `Number received` > 0) %>%
      dplyr::select(-c(`Date brought to KEPHIS`,`Date of CMD visual symptoms`, `Number received`) )
    colnames(dt) = gsub(" ","_", names(dt))
    # selected dt rows
    if(!is.null(input$newplantsTbl_rows_selected)){
      dt = dt[input$newplantsTbl_rows_selected,]
    }
    dt = data.frame(dt)
    dt = dt[rep(row.names(dt), input$copies),]
    dt %>% dplyr::arrange(IITA_Cassava_Kephis_ICK)
  })
  
  output$downloadnewplants <- downloadHandler(
    filename = function(){paste("New Plants Labels.pdf")},
    
    content = function(file) {
      pdf(file, width=2.0, height = 11, paper = 'letter', pagecentre=F) # right align width=6.0 # left width=2.0,
      par(mfrow=c(10, 1),mar=c(0,0,3,0), oma=c(0.5,1,0.5,0)) # right align mar=c(0,30,3,0)
      for(i in 1:(nrow(downloadNewPlantsInput()))){
        image(qrencode_raster(as.character(downloadNewPlantsInput()[i,1])), # QRcode
              cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F, 
              xlab="", ylab="", subtitle = mtext(paste(as.character(downloadNewPlantsInput()[i,1]),"\n", 
                                                       downloadNewPlantsInput()[i,2],"\n", 
                                                       downloadNewPlantsInput()[i,3]), side = 4, line = 0,
                                                 outer = F, at = NA, adj = 0, padj = 0.5, cex = 1, col = 1, las=1, font = 10))
        
      }
      dev.off()
    }
  )
 
  # ---------------------------------------------- Meristem & Tips -----------------------------------------------------------------------------
  output$meristemCopiesOut <- renderUI({
    
    div(
      
      if(input$select_meristem !=""){
        div(numericInput("enterNumber", "Number of copies", value = 0),
        if(input$select_meristem=="Tips" || input$select_meristem=="Multiplication" || input$select_meristem=="Hardening"){
          prettyCheckbox(inputId = "numberRecorded",label = 'Per number of meristems/ tips recorded', value = FALSE,icon = icon("check"),
            status = "primary",animation = "rotate", bigger = TRUE,
          )
        })
      }
    )
  })
  # reset prettycheckbox to false if number is entered
  observeEvent(input$enterNumber,{
    updatePrettyCheckbox(session, "numberRecorded", label = 'Per number of meristems/ tips recorded', value = FALSE)
  })
  
  # reset numeric input to null if prettycheckbox is checked
  observeEvent(input$numberRecorded,{
    updateNumericInput(session, "enterNumber", label = "Number of copies", value = 0)
  })
  
  meristemInput <- reactive({
    req(input$select_meristem)
    dt = cleantable
    if(input$select_meristem==""){
     dt
    }
    if(input$select_meristem=="Meristems Excised"){
      dt = dt[dt$Stage=="Meristems Excised",-c("Test Tubes","Baby Jars","Number of plants","Number of tips")]
    }
    if(input$select_meristem=="Tips"){
      dt = dt[dt$Stage=="Tips",-c("Test Tubes","Baby Jars")]
      dt = dt[dt$`Number of tips` > 0]
    }
    if(input$select_meristem=="Multiplication"){
      dt = dt[dt$Stage=="Multiplication",-c("Number of plants", "Number of tips")]
      dt = dt[dt$`Test Tubes` > 0 | dt$`Baby Jars` > 0]
    }
    if(input$select_meristem=="Hardening"){
      dt = dt[dt$Stage=="Hardening",-c("Test Tubes","Baby Jars","Number of tips")]
      dt = dt[dt$`Number of plants` > 0,]
    }
    if(input$select_meristem=="Laboratory CMD Diagnostics"){
      dt = dt[dt$Stage=="Laboratory CMD Diagnostics",-c("Test Tubes","Baby Jars","Number of plants", "Number of tips")]
    }
    if(input$select_meristem=="Laboratory CBSD Diagnostics"){
      dt = dt[dt$Stage=="Laboratory CBSD Diagnostics",-c("Test Tubes","Baby Jars","Number of plants", "Number of tips")]
    }
    dt
  })
  
  output$meristemsTable <- renderDT({
    
    DT::datatable(meristemInput(), filter = 'top', rownames = FALSE, escape = FALSE, 
                  options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                                 searchHighlight=T, stateSave = TRUE, orderClasses = TRUE))
  })
  
  downloadMeristemInput <- reactive({
    dt = meristemInput()
    colnames(dt) = gsub(" ","_", names(dt))
    dt$UCID_Meristem = paste0(dt$UCID,substring(dt$MeristemID,7,9))
    dt$UCID = NULL
    
    # selected dt rows
    if(!is.null(input$meristemsTable_rows_selected)){
      dt = dt[input$meristemsTable_rows_selected,]
    }
     dt = data.frame(dt)
     if(input$enterNumber > 0 ){
       dt = dt[rep(row.names(dt), input$enterNumber),]
     }
    
    if(input$numberRecorded){
      if(input$select_meristem=="Tips"){
        dt = dt[rep(row.names(dt), dt$`Number_of_tips`),]
      }
      if(input$select_meristem=="Multiplication"){
        dt$Number = as.integer(na.omit(dt$`Test_tubes`)) +  as.integer(na.omit(dt$`Baby_jars`))
        dt = dt[rep(row.names(dt), dt$Number),]
      }
      if(input$select_meristem=="Hardening"){
        dt = dt[rep(row.names(dt), dt$`Number_of_plants`),]
      }
    }
    dt = dt[,c("MeristemID","UCID_Meristem","Category")]
    dt 
  })
  
 
  output$downloadmeristems <- downloadHandler(
    filename = function(){
      paste(input$select_meristem,'-', Sys.Date(), '.xlsx')
    },
    content = function(file) {
      writexl::write_xlsx(downloadMeristemInput(), path = file, col_names = T, format_headers = T )
    }
  )
  
})
)