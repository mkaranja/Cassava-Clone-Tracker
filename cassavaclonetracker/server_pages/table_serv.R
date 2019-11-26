

tableServ <- function(env_serv) with(env_serv, local({
    
  # Sidebar - Select Variables
  output$variablesOut <- renderUI({
    div(
      conditionalPanel(
        condition = "input.ttabs == 'Summary Table'",
        varSelectInput("variables1", "Variables:", database, multiple = T, width = "100%"),
        bsTooltip("variables1", "Select specific columns to view", placement = "right", trigger = "hover")
      ),
      conditionalPanel(
        condition = "input.ttabs == 'Meristems Data'",
        varSelectInput("variables2", "Variables:", meristems, multiple = T, width = "100%"),
        bsTooltip("variables2", "Select specific columns to view", placement = "right", trigger = "hover")
      )
    )
  })
  
#========================================================================================================= Summary Table TAB
 
  summaryInput <- reactive({
    dt = database
    if (length(input$variables1) > 0){
    dt = dt %>% dplyr::select(!!!input$variables1)
    }
    janitor::remove_empty(dt,"cols")
  })
  
  output$summaryTbl <- renderDataTable({
    datatable(summaryInput(), 
              filter = 'top', rownames = FALSE, selection = 'multiple', 
              options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                             searchHighlight=T, stateSave = F, orderClasses = TRUE)
    )
  })
    
  # Entries per page
  
  output$summary_rows <- renderUI({
    paste("Showing ", min(input$summaryTbl_rows_current), " to ", max(input$summaryTbl_rows_current), " of ", max(input$summaryTbl_rows_all), " entries.")
  })

    
#========================================================================================================== MERISTEMS TAB
 
    output$vars1 <- renderPrint({
      input$variables1
    })
    output$vars2 <- renderPrint({
      input$variables2
    })
    
    meristemsInput <- reactive({
      req(input$variables2)
      
      dt = meristems
      if(length(input$variables2) > 0){
        dt = dt %>% dplyr::select(!!!input$variables2)
      }
      janitor::remove_empty(dt,"cols")
    })
    
    output$meristemsTbl <- renderDataTable({
      
      datatable(meristems, 
                filter = 'top', rownames = FALSE, selection = 'multiple', 
                options = list(pageLength = 10, lengthMenu = c(5, 10, 50, 100, 500,1000),
                               searchHighlight=T, stateSave = F, orderClasses = TRUE)
      )
      
    })
    
    output$meristems_rows <- renderUI({
      paste("Showing ", min(input$meristemsTbl_rows_current), " to ", max(input$meristemsTbl_rows_current), " of ", max(input$meristemsTbl_rows_all), " entries.")
    })
    
    
    # ------------------------------ Download Data Button
    
    output$downloadDataOut <- renderUI({
      if(input$ttabs == "Download Labels"){
        if(length(input$summaryTbl_rows_selected)>0){
          downloadButton("downloadDataLabels1","PDF", size="xs", color = "success")
        }else
        if(length(input$meristemsTbl_rows_selected)>0){
          downloadButton("downloadDataLabels2","Excel File", size="xs", color = "success")
        }
      }else {
      downloadButton("downloadData", size="xs", color = "success")
      }
    })
    
    # Download Data table
    
    downloadDataInput <- reactive({
      if(input$ttabs=="Meristems Data"){
        result = meristemsInput()
        if(!is.null(input$meristemsTbl_rows_selected)){
          result = result[input$meristemsTbl_rows_selected,]
        }
      }
      if(input$ttabs=="Summary Table"){
        result = summaryInput()
        if(!is.null(input$summaryTbl_rows_selected)){
          result = result[input$summaryTbl_rows_selected,]
        }
      }
      result
    })
    
   output$downloadData <- downloadHandler(
      filename = function(){paste(input$ttabs,'-', Sys.time(), '.csv')},
      content = function(file) {
        write.csv(downloadDataInput(), file, row.names = F)
      }
    )
    #===================================================================================================== DOWNLOAD LABELS TAB
    
   
    output$random_labels_Out <- renderUI({
      shinyjs::useShinyjs()
      
      if(length(input$summaryTbl_rows_selected) > 0 && length(input$meristemsTbl_rows_selected)==0 || length(input$summaryTbl_rows_selected) == 0 && length(input$meristemsTbl_rows_selected) > 0){
        
          if(input$ttabs == "Download Labels"){
            uiOutput('removeTabOut') # show Remove Bttn only when Tab is active
          }else {
            div(
              actionBttn('random_labels', 'Labels', style = "material-flat", size = "xs"),
              shinyBS::bsTooltip("random_labels", "To generate labels from this table, select the rows with the desired fields.",
                                 "right", options = list(container = "body"))
            )
          }
          
        
      }
    })
    
    
    observeEvent(input$random_labels, {
      shinyjs::disable(input$random_labels)
      
      # Insert a Tab
      insertTab(inputId = "ttabs", # -------------------- on click insert tabPanel
                tabPanel("Download Labels",
                         uiOutput("labelTemplateOut"),
                        column(8, 
                         panel_div(class_type = "default",
                                   content = tags$div(
                                     DTOutput('random_labelsTbl')
                                   )
                         )
                        )
                      ),
                         
                target = "Meristems Data",
                position = "after"
                )
      # Update tabset
      updateTabsetPanel( # -------------------------------focus on the inserted tabPanel
        session, inputId = "ttabs", selected = "Download Labels"
      )
      
      
      # show remove button
      output$removeTabOut <- renderUI({
        div(
          actionBttn('remove_tab', 'Remove', style = "material-flat", size = "xs"),
          shinyBS::bsTooltip("remove_tab", "Click to remove the appended tab",
                             "right", options = list(container = "body")))
        
      })
    })
    
    # ---------------------- selected rows in a table
    random_labelsInput <- reactive({
      if(length(input$summaryTbl_rows_selected)>0){
        dt = database[input$summaryTbl_rows_selected,] # from summary Table tab
        dt = dt[,c("IITA Cassava Kephis ICK","Unique Cassava ID UCID", "Category")]
      }
      
      if(length(input$meristemsTbl_rows_selected)>0){
        dt = meristems[input$meristemsTbl_rows_selected] # from Meristems tab
        dt$UCID_Meristem = paste0(dt$`Unique Cassava ID UCID`,substring(dt$MeristemID,7,9))
        dt = dt[,c("MeristemID", "UCID_Meristem","Category")]
      }
      colnames(dt) = gsub(" ","_", names(dt))
      dt
    })
    
    
    output$random_labelsTbl <- renderDT({
      dt = random_labelsInput()
      colnames(dt) = gsub("_"," ", names(dt))
      dt
    })
    
  # Remove Download Tab
   observeEvent(input$remove_tab, {
      removeTab(inputId = "ttabs", target = "Download Labels")
    })
   
   
# ---------------------------------------------------------------------------------- DOWNLOAD: labels
   
   output$downloadDataLabels1 <- downloadHandler(
     filename = function(){
       paste("Plants Labels-",Sys.Date(),".pdf")
       },
       
     content = function(file) {
         pdf(file, width=2.0, height = 11, paper = 'letter', pagecentre=F) # right align width=6.0 # left width=2.0,
         par(mfrow=c(10, 1),mar=c(0,0,3,0), oma=c(0.5,1,0.5,0)) # right align mar=c(0,30,3,0)
         for(i in 1:(nrow(random_labelsInput()))){
           image(qrencode_raster(as.character(random_labelsInput()[i,1])), # QRcode
                 cex.main = 1.5, cex.sub = 1, asp=1, col=c("white", "black"), axes=F, 
                 xlab="", ylab="", subtitle = mtext(paste(as.character(random_labelsInput()[i,1]),"\n",
                                                          as.character(as.factor(random_labelsInput()[i,2])),"\n", 
                                                          as.character(random_labelsInput()[i,3])), side = 4, line = 0,
                                                    outer = F, at = NA, adj = 0, padj = 0.5, cex = 1, col = 1, las=1, font = 10))
           
         }
         dev.off()
       
     }
   )
   
   output$downloadDataLabels2 <- downloadHandler(
     filename = function(){
       paste("Meristems Labels-",Sys.Date(),".xlsx")
       
     },
     
     content = function(file) {
         writexl::write_xlsx(random_labelsInput(), path = file, col_names = T, format_headers = T )
      
     }
   )
 
   # Label Templates - Meristems
   
   output$labelTemplateOut <- renderUI({
     
     if(length(input$meristemsTbl_rows_selected) > 0){
       
       div(
         column(3, offset = 5,
              downloadLink('labelTemplate','Download meristems labels printing template',
                           onclick ="window.open('http://google.com', '_blank')")
       ))
     }
   })
   
  
})
)