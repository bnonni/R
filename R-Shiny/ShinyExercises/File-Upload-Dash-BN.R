"****************************************************

              SIMPLE TREND DASHBOARD
              DATE: JUNE 13th 2018
              AUTHOR: BRYAN WALSER NONNI

****************************************************"

library(shiny)
library(shinydashboard)
library(pwr)

# HEADER FUNCTION
header = dashboardHeader( title = "Statistical Testing")

#SIDEBAR FUNCTION
sidebar = dashboardSidebar( 
  sidebarMenu(menuItem( text = "Upload Data", 
                        icon = icon("user"),
                        tabName = "upload_dropdown",
                        fileInput("file", "Please Upload CSV")
                        ),
             menuItem( text = "Filters",
                       icon = icon("th"),
                       checkboxInput(inputId= "header", label = "Show Data Headers", value = FALSE),
                       checkboxInput(inputId= "stringsAsFactors", label = "Strings as Factors", value = FALSE),
                       radioButtons(inputId = "sep", label = "Seperator", choices = c(Comma=",", Semicolon=";",Tab="\t",Space=" "), selected = ","),
                       br()
                       )
             )
  )


# BODY FUNCTION
                                  
body = dashboardBody(  tabItems( 
  #"upload_dropsdown" tab connects above to first menu_item, then connects output of that tab via "raw_data" 
  #to output$raw_data in server function which adds a Raw Data tab and connects to the output$raw_data_output
  #function which renders data
  tabItem( tabName = 'upload_dropdown'), uiOutput("raw_data"),
  
  tabItem( tabName = "test_segmentation", uiOutput('test_segementation_page')),
  
  tabItem( tabName = "statistical_analysis", uiOutput('statistical_analysis_page')))
)

#OPTIONS increase default file upload size
options(shiny.maxRequestSize = 9*1024^2)

# SERVER FUNCTION
server = function(input, output){
  output$raw_data = renderUI({   
    
    tabsetPanel(  tabPanel("Data Viewer", uiOutput("raw_data_output")),
                  tabPanel("T-Power Test", uiOutput("t_power_test"))
                  )
    
  })
  output$raw_data_output = renderUI({
    
    fluidPage(   
      fluidRow(
        column( width = 12,
                offset = 0,
                br(),
                div( class = "text-center", 
                     h4("Raw Data Viewer"), br(),
                     p( align='left', style='padding-left:2px;',
                        "This is a simple data viewer allowing you to upload you data set with headers to ensure that your data is able to be uploaded,
                        and that it looks correct once uploaded into the app. Please use the Upload Data dropdown to your right to upload your csv file. ")))),
                     tableOutput("table"))
      
    
  })
  output$t_power_test = renderUI({
    fluidPage(   
      fluidRow(
        column( width = 12,
                offset = 0,
                br(),
                div( class = "text-center", 
                     h4("T-Power Test"), br(),
                     p( align='left', style='padding-left:2px;',
                        "This panel allows the user to upload their data set, input the T-Power test parameters, and run a T-Power test on their dataset.",
                        br(), br()),
                        p(align='left', style='padding-left:5px;', "Please input your data's parameters below."),
                        div(style='width: 20%', numericInput( inputId = "observations",label = "Number of Observations", value = 0, min = 1, max = 1000000)),
                        div(style='width: 20%', numericInput( inputId = "observations",label = "Effect Size", value = 0, min = 0.1, max = 1.0)),
                        p(align='left', style='padding-left:10px;', "3. sig.level = Significance level (Type I error probability)"),
                        p(align='left', style='padding-left:10px;', "4. power = Power of test (1 minus Type II error probability)"),
                        p(align='left', style='padding-left:10px;', "5. type = Type of t test : one- two- or paired-samples"),
                        p(align='left', style='padding-left:10px;', "6. alternative = a character string specifying the alternative hypothesis, must be one of 'two.sided' (default), 'greater' or 'less'")
                        ))),
      
      renderTable("tpowertest"))
  })
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

  #function returns the data in the table format
  output$table <- renderTable({
    if(is.null(data())){return()}
    data()
  })
  
  output$tpowertest <- function(n,d,s,p,t,a){
    if(is.null(data())){return()}
    pwr.t.test(n, d, s, p, t, a)
  }
  
  }

  

# SHINY APP
shinyApp( ui =  dashboardPage( header = header,
                               sidebar = sidebar,
                               body = body),
          server = server)

