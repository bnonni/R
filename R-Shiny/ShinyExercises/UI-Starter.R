# UI STARTER CODE FOR BRYAN

library(shiny)
library(shinydashboard)

# HEADER FUNCTION
header = dashboardHeader( title = "Data Viewer")

#SIDEBAR FUNCTION
sidebar = dashboardSidebar(fileInput("file", "Upload a file to view"),
                           helpText("Default max file size is 5MB"),
                           tags$hr(),
                           h5(helpText("Select the read.table parameters below")),
                           checkboxInput(inputId= "header", label = "Header", value = FALSE),
                           checkboxInput(inputId= "stringsAsFactors", label = "stringsAsFactors", value = FALSE),
                           br(),
                           radioButtons(inputId = "sep", label = "Seperator", choices = c(Comma=",", Semicolon=";",Tab="\t",Space=" "), selected = ",")
                           
                   
)


# BODY FUNCTION
body = dashboardBody(uiOutput("tb"))

#OPTIONS increase default file upload size
options(shiny.maxRequestSize = 9*1024^2)

# SERVER FUNCTION
server = function(input, output){
  #this reactive function takes the inputs from the sidebar above and uses them for read.table() to read the data from the file 
  #file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file=file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringsAsFactors)
  })
  
  #function outputs the dataset and displays the data summary in table format - returning the data frame which contains the file type and data path
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file
  })
  
  #function ouputs the summary of the dataset and displays that summary in table format - returning the actual data summary
  output$sum <- renderTable({
    if(is.null(data())){return()}
    summary(data())
  })
  
  #function returns the data in the table format
  output$table <- renderTable({
    if(is.null(data())){return()}
    data()
  })
  
  #function renders the sidebar
  output$tb <- renderUI({
    tabsetPanel(tabPanel("About file", tableOutput("filedf")), tabPanel("Data", tableOutput("table")), tabPanel("Summary", tableOutput("sum")))
  })
}

# SHINY APP
shinyApp( ui =  dashboardPage( header = header,
                               sidebar = sidebar,
                               body = body),
          server = server)

