"****************************************************

      TESTER DASHBOARD RSHINY CODE
      DATE: JUNE 13th 2018
      AUTHOR: BRYAN WALSER NONNI

****************************************************"

library(shiny)
library(shinydashboard)
library(rhandsontable)



#
  
  header = dashboardHeader()



  sidebar = dashboardSidebar()


  body = dashboardBody()

  
server = function(input, output){}
  
  
  shinyApp(dashboardPage( header = header,
                          sidebar = sidebar,
                          body = body ),
                          server = server)