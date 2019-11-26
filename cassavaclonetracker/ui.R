
# UI files
source("ui_pages/home.R")
source("ui_pages/overview.R")
source("ui_pages/table.R")
source("ui_pages/labels.R")
source("ui_pages/upload.R")
source("ui_pages/about.R")


shinyUI(
    fluidPage(
        
        navbarPage(id = "navBar",
                   title = img(src="img/cct.png", height = "40px"), theme = "paper.css", collapsible = TRUE, inverse = TRUE,
                   windowTitle = "CCT", position = "fixed-top",
                   
                   header = tags$style(
                       ".navbar-right {
                   float: right !important;
                   }",
                       "body {padding-top: 75px;}"),
                   
                   # Navbar Tabs  -----------------------------------------------
                   homeTab,
                   tableTab,
                   navbarMenu("MANAGE",
                       labelsTab,
                       uploadTab
                   )
                   
                   
        ),
        HTML(paste("<script>var parent = document.getElementsByClassName('navbar-nav');
               parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\"><strong>",
                   uiOutput("out_id"),"</strong></a></li><li class=\"disabled\"><a href=\"#\"><strong>",
                   uiOutput('userpanel'),"</strong></a></ul>' );</script>"))
        
        
    )
)