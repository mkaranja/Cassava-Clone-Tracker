
source("carousels.R")

homeTab <-
  tabPanel("HOME", value = "home",
         
         shinyjs::useShinyjs(),
         
         fluidRow(
           HTML("
                <section class='banner'>
                <h2 class='parallax'>CASSAVA CLONE TRACKER</h2>
                
                </section>
                ")
           ## <p class='parallax_description'></p>
            
         ),
         
         
         
         # WHAT
         fluidRow(
           column(3),
           column(6,
                  shiny::HTML("<br><br><center> <h1>What you'll find here</h1> </center><br>"),
                  shiny::HTML("<h5>An interactive tool to help you keep track of cassava clones from collection through diagnostics, multiplication to conservation.</h5>")
           ),
           column(3)
         ),
         
         fluidRow(
           
           style = "height:50px;")
)