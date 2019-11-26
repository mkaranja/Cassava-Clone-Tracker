


library(shiny)
source("server_pages/overview_serv.R")
source("server_pages/table_serv.R")
source("server_pages/charts_serv.R")
source("server_pages/labels_serv.R")
source("server_pages/upload_serv.R")

# Define server logic required to draw a histogram

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

shinyServer(function(input, output, session) {

  session$onSessionEnded(stopApp)# Automatically stop a Shiny app when closing the browser tab
  # Navbar ------------------------------------------------------------------
  # shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  # DT Options --------------------------------------------------------------
  options(DT.options = list( lengthMenu = c(10, 20),
                             dom = 'tl'
  ))  # table and lengthMenu options
  
  
  # userpanel ----------------------------------------------------------------
  output$userpanel <- renderUI({
    
    if (!is.null(session$user)) {
      span(
        paste(stringr::str_to_sentence(session$user),"|"),
        a(icon("sign-out"), "Logout", href="__logout__")
      )
    }
    
  })
  
  # loading page animation---- ------------------------------------------------
  load_data() 
  
  # environment variable for loading server files
  env_serv = environment()
  
  # Overview Tab --------------------------------------------------------------
  #overviewServ(env_serv)
  
  # Table Tab -----------------------------------------------------------------
  tableServ(env_serv)
  
  # Charts Tab ----------------------------------------------------------------
  chartsServ(env_serv)
  
  # Labels Tab-----------------------------------------------------------------
  labelsServ(env_serv)
  
  # Upload Tab
  uploadServ(env_serv)
  
})
