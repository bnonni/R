"****************************************************

              SIMPLE TREND DASHBOARD
              DATE: JUNE 13th 2018
              AUTHOR: BRYAN WALSER NONNI

****************************************************"


library(markdown)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(rhandsontable)
library(pwr)

library(sqldf)
library(dplyr)
library(reshape)
library(ggplot2)
library(lpSolveAPI)
library(lubridate)

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
  tabItem( tabName = 'upload_dropdown'), uiOutput("raw_data"))
  
  )


#OPTIONS increase default file upload size
options(shiny.maxRequestSize=30*1024^4)

# SERVER FUNCTION
server = function(input, output){
  output$raw_data = renderUI({   #renders multiple tabs in the main body of the app
    
    tabsetPanel(  tabPanel("Data Viewer", uiOutput("raw_data_output")),
                  tabPanel("T-Power Test", uiOutput("t_power_test")),
                  tabPanel("Chi-Square Test", uiOutput("chi_square_test")),
                  tabPanel("T-Test", uiOutput("t_test"))
                  )
    
  })
  output$raw_data_output = renderUI({ #raw data table viewer tab
    
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
  #function returns the data in the table format
  output$table <- renderTable({
    if(is.null(data())){return()}
    data()
  })
  
  #tPowerTest tab output
  output$t_power_test = renderUI({
    fluidPage(   
      fluidRow(
        column( width = 12,
                offset = 0,
                br(),
                div( class = "text-center", 
                     h4("T-Power Test"), br(),
                     p( align='center', style='padding-left:2px;',
                        "This panel allows the user to upload their data set, input the T-Power test parameters, and run a T-Power test on their dataset.",
                  br(), br())),
                box(width = 10,
                      status = 'primary',
                      solidHeader = TRUE,
                      title = "T-Power Test Parameters",
                  splitLayout( cellWidths = c("100%"),
                    #div(style='width: 100%', numericInput( inputId = "n",label = "Number of Observations", value = 60, min = 1, max = 1000000)),
                    
                    #div(style='width: 100%', numericInput( inputId = "d",label = "Effect Size", value = .02, min = 0.01, max = 1, step=.01)),
                    
                    
                    div(style='width: 100%', numericInput( inputId = "s",label = "Significance Level (Recommended .05):", value = .05, min = 0.01, max = .3, step=.01))),
                  
                  splitLayout( cellWidths = c("33%", "33%", "34%"),
                    #At least one must be null
                    #div(style='width: 100%', numericInput( inputId = "p", label = "Power Level (Recommended 0.8):", value = 0.8, min = 0.1, max = 1, step=.01)),
                    
                    
                    radioButtons( inputId = "t", label = "Type of t test", choices = c("one.sample", "two.sample", "paired"), width="100%", inline=TRUE),
                    
                    radioButtons( inputId = "a",label = "Alternative Hypothesis:",choices = c("two.sided", "greater", "less"), width="100%", inline=TRUE),
                    
                    fileInput(inputId = "tPowerFile", label = "Upload a CSV:", accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv"))
                        )))), textOutput("tpowertest", inline = FALSE)
      )})
  #tpowerTest function to compute results of a pwr.t.test() and send back to the tPowerFile function
  tpowertest = function(file){
    treatment = file[sample(1:nrow(file), 100, replace=FALSE),]
      treatMean = sum(treatment)/length(treatment)
    
                      
    control = file[sample(1:nrow(file), 100, replace=FALSE),]
      contMean = sum(control)/length(control)
      
    sampleSize = length(treatment + control)
    stdev = sd(treatment + control)
    
    if((length(treatment) > length(control))){
      
      effect = (treatMean - contMean)/stdev
    
    final_results = pwr.t.test(n = sampleSize, d = effect, sig.level = input$s, type = input$t, alternative = input$a) 
    
    return(final_results)
    
    }else if((length(control) > length(treatment))){
      effect = (contMean - treatMean)/stdev
    
    final_results = pwr.t.test(n = sampleSize, d = effect, sig.level = input$s, type = input$t, alternative = input$a) 
    return(final_results)
    
    }
  }
  #Takes in the datafile from the tpowertest tab, reads it in, and sends it to the tpowertest function
  tPowerFile = reactive({  
    
    file = input$tPowerFile
    if (is.null( file ))
    { return() }
    else
    {
      data = as.data.frame( read.csv(file = file$datapath)  )
      results = tpowertest(data)
      return(results)
    } 
  })
  #Renders the output of the tPowerFile function and prints it to the output window
  output$tpowertest = renderPrint({ 
    
    paste('n = ', tPowerFile()[[1]], 'd = ', tPowerFile()[[2]], 'power = ', tPowerFile()[[4]]) 
  
  })
  
  
  #Chi square test tab output
  output$chi_square_test = renderUI({
    fluidPage(   
      fluidRow(
        column( width = 12,
                offset = 0,
                br(),
                div( class = "text-center", 
                     h4("Chi-square Test"), br(),
                     p( align='center', style='padding-left:2px;',
                        "This panel allows the user input the Chi-square test parameters and run a test.",
                        br(), br())),
                box(width = 12,
                    status = 'primary',
                    solidHeader = TRUE,
                    title = "Please input test parameters below.",
                    cellWidths = c("33%", "33%", "33%"),
                                 div(style='width: 100%', numericInput( inputId = "x",label = "Number of Rows (in data set)", value = 50, min = 1, max = 1000000),
                                     numericInput(inputId = "y", label = "Number of Columns", value=50, min=1, max=1000000)),
                fileInput(inputId = "chiSquareFile", label = "Upload a CSV:", accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv")), 
                textOutput("chisquaretest", inline = FALSE)
          ))))})
  
  #chisquaretest function to compute results of a chisq.test() and send back to the chiSquareFile function
  chisquaretest = function(file){
    test = subset(file, select=c("Test", "Group"));
    
    return(test)

   # control = file[sample(1:nrow(file), 100, replace=FALSE),]
    
  #  sampleSize = length(treatment + control)
   # stdev = sd(treatment + control)
    
    #if((length(treatment) > length(control))){
      
     # effect = (treatMean - contMean)/stdev
      
      #final_results = pwr.t.test(n = sampleSize, d = effect, sig.level = input$s, type = input$t, alternative = input$a) 
      
    #  return(final_results)
      
    #}else if((length(control) > length(treatment))){
     # effect = (contMean - treatMean)/stdev
      
      #final_results = pwr.t.test(n = sampleSize, d = effect, sig.level = input$s, type = input$t, alternative = input$a) 
      #return(final_results)
    }
  
  #Takes in the datafile from the chisquaretest tab, reads it in, and sends it to the chisquaretest function
  chiSquareFile = reactive({  
    
    file = input$chiSquareFile
    if (is.null( file ))
    { return() }
    else
    {
      data = as.data.frame( read.csv(file = file$datapath)  )
      results = chisquaretest(data)
      return(results)
    } 
  })
  
  #Renders the output of the chisquarefile function and prints it to the output window
  output$chiSquareFile = renderPrint({ 
    
    paste(results) 
    
  })
  
  
  
  
  "*******************************************************************"
  output$t_test = renderUI({
    fluidPage(   
      fluidRow(
        column( width = 12,
                offset = 0,
                br(),
                div( class = "text-center", 
                     h4("Chi-square Test"), br(),
                     p( align='center', style='padding-left:2px;',
                        "This panel allows the user input the Chi-square test parameters and run a test.",
                        br(), br())),
                box(width = 12,
                    status = 'primary',
                    solidHeader = TRUE,
                    title = "Please input test parameters below.",
                    cellWidths = c("33%", "33%", "33%"),
      div(style='width: 100%', numericInput( inputId = "x",label = "Number of Rows (in data set)", value = 50, min = 1, max = 1000000)),
      div(style='width: 100%; text-align: center', radioButtons( inputId = "a",label = "Alternative Hypothesis:",choices = c("two.sided", "greater", "less"), width="100%", inline=TRUE)),
      div(style='width: 100%; text-align: center', radioButtons( inputId = "a",label = "Mean",value = 0, min = 0, max = 1000000, step = .01, width="100%", inline=TRUE)),        
      div(style='width: 100%; text-align: center;', radioButtons( inputId = "P", label = "Paired?", choices = c("TRUE", "FALSE"), width="100%", inline=TRUE)),        
      div(style='width: 100%; text-align: center;', radioButtons( inputId = "v", label = "Two equal variances?", choices = c("TRUE", "FALSE"), width="100%", inline=TRUE)),        
      div(style='width: 100%', numericInput( inputId = "s",label = "Significance Level (Recommended .05):", value = .1, min = 0.01, max = .3, step=.01)),        
            
                  rHandsontableOutput("Ttest", width = "100%", height = "100%")
                ))))})
  
  output$Ttest <- renderRHandsontable({
    results = t.test(x = input$x, y = NULL, alternative = input$a, mu = input$m, paired=input$P, var.equal=input$v )
    return(rhandsontable(results))
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

    }

  

# SHINY APP
shinyApp( ui =  dashboardPage( header = header,
                               sidebar = sidebar,
                               body = body),
          server = server)

