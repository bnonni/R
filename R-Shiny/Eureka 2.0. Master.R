"****************************************************

      EUREKA MASTER TESTING APPLICATION CODE
             DATE: MAY 9th 2018
        AUTHOR: SIFAEL SEBASTIAN NDANDALA
****************************************************"

# NECESSARY PACKAGES
library(data.table)
library(shiny)
library(markdown)
library(shinyWidgets)
library(rhandsontable)
library(shinydashboard)

# DATA MUNGING PACKAGES
library(zoo)
library(dplyr)
library(reshape)
library(ggplot2)
library(lpSolveAPI)
library(lubridate)

# MODELING PACKAGES
library(CausalImpact)

"******************************
  
        HEADER SECTION

******************************"
header = dashboardHeader( title = "Eureka 2.0." )


"******************************
  
      SIDEBAR SECTION

******************************"
sidebar = dashboardSidebar(
  
          # SIDEBARMENU
          sidebarMenu(   
                       menuItem( text = "Eureka 2.0. Information",
                                 tabName = "eureka_info")),
          sidebarMenu(  
                       menuItem( text = "Sample Size Estimation",
                                 icon = icon('th'),
                                 tabName = "sample_size")),
          sidebarMenu(   
                       menuItem( text = "Test Segmentation",
                                 icon = icon('adjust'),
                                 tabName = "test_segmentation")),
          sidebarMenu(
                       menuItem( text = "Statistical Analysis",
                                 icon = icon('fire'),
                                 tabName = "statistical_analysis"))
)


"******************************
  
        BODY SECTION

******************************"

body = dashboardBody(   
  
        tabItems( tabItem( tabName = 'eureka_info', includeHTML('welcome.html')),
                  tabItem( tabName = "sample_size", uiOutput('sample_size_pages') ),
                  tabItem( tabName = "test_segmentation", uiOutput('test_segementation_page')),
                  tabItem( tabName = "statistical_analysis", uiOutput('statistical_analysis_page')))

)


"**********************************
  
        SERVER SECTION

**********************************"
server = function(input, output){
  
# SET FILE UPLOAD CAPACITY
options(shiny.maxRequestSize=30*1024^4)

"********************************************
1.1. SAMPLE CALCULATOR UI
1.2. PROPORTION SAMPLE ESTIMATE UI
1.3. CONTINUOUS SAMPLE ESTIMATE UI
********************************************"

  
#1.1.
output$sampleCalculatorPage = renderUI({
  
fluidPage(   
          fluidRow(
                    column( width = 12,
                            offset = 0,
                            br(),
                            div( class = "text-center", 
                                 h4("Sample Size Calculator Playground"), br(),
                                 p( align='left', style='padding-left:2px;',
                                    "The sample size calculator playground is designed to provide quick and intuitive sample size
                                    estimation based on various scenarios from observational and statistical metrics. It is also
                                    intended to provide information about the relationships between statistical parameters and sample size
                                    to aid decision making when choosing against the statistical parameter threshold", br(), br(),
                                    "In the section below, you can adjust observational and statistical metrics to determine sample size requirements.")))),
          fluidRow(
                    column( width = 6,
                            offset = 0,
                            br(),
                            div(  class="text-center", 
                                  box( width = 12,
                                       status = "primary",
                                       solidHeader = TRUE, 
                                       title = "Baseline Metrics",
                                       p( "Select baseline metrics below:", align='left'),
                                       splitLayout( cellWidths = c("50%", "50%"),
                                                    cellArgs = list(style="padding:6px;"),  
                                                    numericInput( inputId = "propBaseline",
                                                                  label = "Baseline Proportion:",
                                                                  value = .5, min=.01, max=1, step = .01),
                                                    numericInput( inputId = "propEffectSize",
                                                                  label = "Projected Effect Size:",
                                                                  value = .05, min = .01, max = 1, step = .01)),
                                        splitLayout( cellWidths = c("50%", "50%"),
                                                     cellArgs = list(style="padding: 6px;"),
                                                     numericInput( inputId = "propSplitRatio",
                                                                   label = "A|B Split Ratio (Recommended 50%)",
                                                                   value = .5, min = .5, max =.9 , step = .05 ),
                                                     numericInput( inputId = "propDailyImpressions",
                                                                   label = "Average Daily Impressions:",
                                                                   value = 500, min = 1, max =NA , step = 1 ))))),
          
                    column( width = 6,
                            offset = 0,
                            br(),
                            div(  class='text-center', 
                                  box( width = 12,
                                       status = 'primary',
                                       solidHeader = TRUE, 
                                       title = "Tolerance Parameters",
                                       p("Select tolerance paramters below:", align='left', br(), br()),
                                       numericInput( inputId = "propPowerLevel",
                                                     label = "Power Level (Recommended 80%):",
                                                     value = .80, min = .70, max = .90, step = .01),
                                       numericInput( inputId = "propSigLevel",
                                                     label = "Significance Level (Recommended .05):",
                                                     value = .05, min = .01, max = .3, step = .01))))),
          
          fluidRow(  
                    column( width = 12,
                            offset = 0,
                            br(),
                            div(  class="text-center",
                                  box( width = 12,
                                       status = "info", 
                                       solidHeader = TRUE,
                                       title = "Sample Size Estimates and Parameter Information",
                                       splitLayout( cellWidths = c("50%", "50%"),
                                                    cellArgs = list(style=''),
                                                    div( style="display: inline-block; vertical-align: text-top;",
                                                         #br(),
                                                         h5("Sample Size Estimates:"),
                                                         rHandsontableOutput("sampleSizeOutput"),
                                                         br()),
                                                    div(  class="text-center",
                                                          uiOutput('metricInfo') ) ))))),
          
          fluidRow( 
                    column( width = 12,
                            offset = 0,
                            br(),
                            div( class='text-center',
                                 box( width = 12,
                                      status = "info",
                                      solidHeader = TRUE,
                                      title = "Sample Size and Parameter Visualization",
                                      splitLayout( cellWidths = c("50%", "50%"),
                                                   cellArgs = list(style='padding:6px;'),
                                                   plotOutput("samplePowerPLot"),
                                                   plotOutput("sampleSigPLot"))))))
          
  
)
  
  
})


"************************************
          SAMPLE FUNCTIONS

1.1. CONVERT RATIO 
1.2. DAILY CONVERSION
1.3. SAMPLE SIZE CALCULATOR
*************************************"
# 1.1. 
ratioCalculator = function(split_ratio){ return(round(( (split_ratio*10)/(1-split_ratio)/10 ),1)) }

# 1.2.
dailyDenominator = function(sample_size , daily_average){ return( sample_size/daily_average ) }

# 1.3. 
playGroundSampleCalculator = function(baseline, effect_size, split_ratio, power_level, sig_level)
{
  # Z SCORE FOR POWER AND SIGNIFICANCE LEVEL
  z_power = round(qnorm( power_level ),2)
  z_siglevel = round(abs(qnorm(sig_level/2)), 2)
  
  # CONVERT RATIO
  sample_ratio = ratioCalculator(split_ratio)
  
  # COMPUTE SAMPLES
  part_1 = (sample_ratio + 1)/sample_ratio
  part_2 = ((baseline+effect_size)*(1-(baseline + effect_size))*( z_power + z_siglevel)^2)/(effect_size)^2
  
  sample_size = part_1*part_2
  return( ceiling(sample_size) )
  
}

"*******************************
    OUTPUT FOR PLAYGROUND

1.1. SAMPLE SIZE OUTPUT
1.2. METRICS DEFINITION
1.3. SAMPLE POWER PLOT
1.4. SAMPLE SIGLEVEL PLOT
*******************************"

# 
output$sampleSizeOutput = renderRHandsontable({
  
    # COMPUTE SAMPLE
    lower_sample = 100 + playGroundSampleCalculator( input$propBaseline, input$propEffectSize, input$propSplitRatio,
                                                     input$propPowerLevel, input$propSigLevel )
    
    
    larger_ratio = ratioCalculator(input$propSplitRatio)
    
    
    control = c( larger_ratio*lower_sample, 
                 ceiling (dailyDenominator(larger_ratio*lower_sample, input$propDailyImpressions ))  )
    test = c(lower_sample,  ceiling(dailyDenominator(lower_sample, input$propDailyImpressions )) )  
    
    # RETURN DATA FRAME
    final = rbind(control, test)
    row.names(final) = c("Control", "Test")
    colnames(final) = c("Estimated Sample Size", "Estimated Days")
    rhandsontable(as.data.frame(final), colWidths = 160, rowHeaderWidth=70, strict = TRUE)
  
})


#
output$samplePowerPLot = renderPlot({
  
  # COMPUTING SAMPLES 
  power_levels = seq(.5, .9, .05)
  sig_level = .05
  
  samples_sizes = 100 + mapply(playGroundSampleCalculator, input$propBaseline, 
                                                           input$propEffectSize, 
                                                           input$propSplitRatio,
                                                           power_levels, 
                                                           sig_level)
  data = as.data.frame(cbind(power_levels, samples_sizes))
  
  # COMPUTING ONE SAMPLE
  optimum_sample = 100 + playGroundSampleCalculator( input$propBaseline, input$propEffectSize, input$propSplitRatio,
                                                     input$propPowerLevel, input$propSigLevel )
  
  optimum_frame = as.data.frame( cbind( optimum_sample, input$propPowerLevel) )
  colnames(optimum_frame) = c("sample_size", "power_level")
  
  # BUILDING THE PLOTS
  ggplot( data = data, aes(x=samples_sizes, y = power_levels)) + 
          geom_point(color='blue', size=3) +
          geom_point(data = optimum_frame, aes(x = sample_size, y = power_level ), colour="#009E73", size=4) +
          labs( title= paste( "Sample Size vs Power at Baseline =", input$propBaseline, ",Significance Level ", input$propSigLevel),
                x = "Estimated Sample Size",
                y = "Statistical Power") +
          theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"))

})

# SAMPLE SIGNIFICANCE PLOT
output$sampleSigPLot = renderPlot({
  
  # COMPUTING THE METRICS
  sig_levels = seq(.01, .9, .04)
  power_level = .8
  samples_sizes = 100 + mapply(playGroundSampleCalculator, input$propBaseline, 
                                                           input$propEffectSize, 
                                                           input$propSplitRatio,
                                                           power_level, 
                                                           sig_levels)
  data = as.data.frame(cbind(sig_levels, samples_sizes))
  
  # COMPUTING ONE SAMPLE
  optimum_sample = 100 + playGroundSampleCalculator( input$propBaseline, input$propEffectSize, input$propSplitRatio,
                                                     input$propPowerLevel, input$propSigLevel )
  
  optimum_frame = as.data.frame( cbind( optimum_sample, input$propSigLevel) )
  colnames(optimum_frame) = c("sample_size", "sig_level")
  
  # BUILD THE PLOT
  ggplot( data = data, aes(x=samples_sizes, y = sig_levels)) + 
          geom_point( color='blue', size=3) +
          geom_point( data = optimum_frame, aes(x = sample_size, y = sig_level ), colour="#009E73", size=4) +
          labs( title= paste("Sample Size vs Significant Level at Baseline =", input$propBaseline, ",Power Level ", input$propPowerLevel),
                x = "Estimated Sample Size",
                y = "Significance Level") +
                theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"))
  
})


"******************************************
        INFORMATION ABOUT METRICS

1.1. BASELINE
1.2. Effect Size
1.3. Power Level
1.4. Significance Level
1.5. METRIC INFORMATION OUTPUT
******************************************"

# 1.1. 
output$baseline_info = renderUI({
  
  div( style='width:100%;',
       p(),
       p( align='left', 
          "The baseline metric is the probability of success in a binomial trail.", br(),
          "For a specific campaign, this can be the average CTR or conversion rate.", br(), br(),
          tags$b("Enter the number in a proportion, for example: .5 = 50% CTR")))
  
})

# 1.2.
output$effectSizeInfo = renderUI({
  
  div( p(), 
       p( align='left',
          "Effect size is the detectable lift given the sample, power and significance.",br(),
          "Higher effects size increases the power (given significance level and ME) ", br(), br(),
          tags$b("Enter the number in a proportion, for example: .01 = 1% Lift")))
  
})

# 1.3.
output$powerInfo = renderUI({
  
  div( p(),
       p( align='left',
          "Statistical Power is the probability of correctly detecting a significant lift.", br(),
          "Typical threshold is 80% but can be changed to accomodate client needs.", br(),
           br(),
          tags$b("Enter the number in a proportion, for example: .8 = 80% Power")))  
})

# 1.4.
output$signLevelInfo = renderUI({
  
  div( p(),
       p( align='left',
          "Significance level is the tolerance set for Type I error.", br(),
          "Typical threshold is .05% but can be changed to accomodate client needs.", br(),
          br(),
          tags$b("Enter the number in a proportion, for example: .05 = 5% Sig. Level")))
})


# 1.5.
output$metricInfo = renderUI({
  
    tabsetPanel( tabPanel("Baseline", uiOutput("baseline_info")),
                 tabPanel("Effect Size", uiOutput('effectSizeInfo')),
                 tabPanel("Power", uiOutput('powerInfo')),
                 tabPanel("Significance Level", uiOutput('signLevelInfo')))
  
})
  

"********************************************
      
   PROPORTION BASED MULTIPLE ESTIMATION 

1.1. MAIN MULTIPROP PAGE
1.2. CHECKING THE BUTTON
1.3. CHECK FOR EXECUTION BUTTON 
1.4. RENDER MULTIPROP RESULTS TABLE
********************************************"

# 1.1.
output$mainMultiPropPage = renderUI({
  
    fluidPage(  
               fluidRow(
                         column( width = 12,
                                 offset = 0,
                                 br(),
                                 div( class = "text-center", 
                                      h4("Proportion Variable Sample Estimate"), br(),
                                       p("This is the multiple Sample Estimation Page. To begin, enter a csv file in the format below:")),
                                 br(),
                                 div( class = "text-center",
                                      box( width = 12,
                                           status = 'primary',
                                           solidHeader = TRUE,
                                           title = "Estimation Inputs",
                                           br(),
                                           splitLayout( cellWidths = c("34%", "33%", "33%"),
                                                        cellArgs = list(style='padding:6px;'),
                                                        fileInput( inputId = "multiPropfile",
                                                                   label = "Upload a CSV:",
                                                                   accept = c("text/csv",
                                                                              "text/comma-separated-values, text/plain",
                                                                              ".csv")),
                                                        numericInput( inputId = "multiPropPower",
                                                                      label = "Power Level (Recommended .8):",
                                                                      value = .8, min = .6, max = .9, step = .01),
                                                        
                                                        numericInput( inputId = "multiPropSig",
                                                                      label = "Significance Level (Recommended .05):",
                                                                      value = .05, min = .01, max = .3, step = .01)),
                                           p("Set the metrics and Run Estimation to compute Sample Size Requirements:"),
                                           actionButton( style="padding-top: -20px;",
                                                         inputId = "multiPropSampleEstimation", 
                                                         label = "Run Estimation", icon = icon('fire') ))))),
              uiOutput('multiPropSampleResults'),
              uiOutput('multiPropSamplebySplitExecute')
              )
               
              
})


# 1.2. SAMPLE COMPUTING FUNCTION
propSampleComputation = function(datafile)
{
  groups = as.vector(unique(datafile[, 1]))
  lift_levels = c(1, 2, 3, 4, 5, 10, 15)
  all_results = list()
  
  for (group in groups)
  {
    proportion = datafile[datafile[, 1] == group, 2:length(datafile)]
    for (lift_level in lift_levels)
      { 
          sample_estimate = max(100 + mapply( playGroundSampleCalculator, proportion, 
                                                                          lift_level/100, 
                                                                          .5,
                                                                          input$multiPropPower, 
                                                                          input$multiPropSig))
          all_results[[ paste(group,lift_level)]] = c(group, lift_level, sample_estimate)
      } 
  }
  final = as.data.frame(do.call(rbind, all_results))
  colnames(final) = c('Group', 'Lift','Samples')
  
  # CONVERTING RESULTS INTO MAIN
  final_results =  cast(final, Group ~ Lift, value = "Samples")
  final_results = final_results[, c("Group", lift_levels)]
  return(final_results)
  
}


# 1.3. - CHECK FOR THE EXECUTE BUTTON
multiPropFile = eventReactive( input$multiPropSampleEstimation , {  
    file = input$multiPropfile
    if (is.null( file ))
    { return() }
    else
    {
      data = as.data.frame( read.csv(file = file$datapath)  )
      results = propSampleComputation(data)
      row.names(results) = results[, 1]
      return(results)
    } 
})


# 1.4. 
output$multiPropTableResults = renderRHandsontable({ 

rhandsontable( as.data.frame( multiPropFile())[,-1], height="100%", width = "100%", rowHeaderWidth=180, colWidths=80 ) 
  
})

# 1.5.
multiPropSampleExecute = eventReactive( input$multiPropSampleEstimation, {

fluidRow( 
         column( width = 12,
                 offset = 0,
                 br(),
                 div( class = 'text-center',
                      box( width = 12,
                           status = 'primary',
                           solidHeader = TRUE,
                           title = "Sample Size Output",
                           br(),
                           div(  style = "display: inline-block; vertical-align: text-top;",
                                 h5("Sample Size Estimates by Group and Lift Estimation 1-15%:"),
                                 p( tags$b("Note:"), 
                                 ("Sample Estimates are for a individual group. Both Test and Control require the estimated sample size")),
                                 rHandsontableOutput('multiPropTableResults'), br() ),
                           div( p("Click to download file"),
                                actionButton(inputId = "download" , 
                                             label = "Download", 
                                             icon = icon("download"),
                                             style="color: #fff; background-color: #337ab7;")), br() ))))
})


# 1.6.
output$multiPropSampleResults = renderUI({  multiPropSampleExecute()  })

"********************************************** 
      SAMPLE ESTIMATES BY SPLIT RATIO

1.1. SAMPLE ESTIMATES BY SPLIT RATIO
1.3. REACTIVE FUNCTION TO RETURN OUTPUT

**********************************************"

# 1.1.
propSamplebySplit = function(datafile)
{
  groups = as.vector(unique(datafile[, 1]))
  lift_levels = c(1 , 2 , 3, 4, 5, 10, 15)
  split_ratio = c(.5, .6, .7, .8, .9)
  all_results = list()
  for (group in groups) 
  { proportion = datafile[datafile[, 1] == group, 2:length(datafile)]
  for (lift_level in lift_levels)
  { for (ratio in split_ratio )
  {
    estimate = max(100 + mapply( playGroundSampleCalculator, proportion, lift_level/100, ratio, input$multiPropPower, input$multiPropSig ))
    all_results[[ paste(group, lift_level, ratio, sep = "_")]] = c(group, lift_level, paste(ratio*100, (1-ratio)*100, sep = "/") , estimate)
  }}}
  
  final = as.data.frame(do.call(rbind, all_results))
  row.names(final) = c()
  colnames(final) = c("Group", "Lift", "Split", "Samples")
  return(final) 
}


# 1.2.
multiPropbyRatio = eventReactive( input$multiPropSampleEstimation , {
  
  file = input$multiPropfile
  if (is.null( file ))
  { return() }
  else
  {
    data = as.data.frame( read.csv(file = file$datapath)  )
    results = propSamplebySplit(data)
    return(results)
  }
})


# 1.3. - REACTIVE UI
multiPropSamplebySplitExecute = eventReactive( input$multiPropSampleEstimation,
{
  fluidRow(  
            column( width = 12,
                    offset = 0,
                    br(),
                    div( class = 'text-center',
                         box( width = 12, 
                              status = 'primary',
                              solidHeader = TRUE,
                              title = "Sample Size Estimates by Split Ratio",
                              br(),
                              div( radioGroupButtons( inputId = "sampleGroup",
                                                      label = "Select a Group:",
                                                      choices = as.vector(unique(multiPropbyRatio()[, 1])))),
                               div(  style = "display: inline-block; vertical-align: text-top;",
                                     br(),
                                     p( tags$b("Note:"), 
                                      ("Sample Estimates are for a individual group with the lowest proportion. Adjust by ratio to estimate sample for larger proportion")),
                                     div( style="padding-left:10%;", 
                                          rHandsontableOutput("multiPropSamplebySplit"), br() ))))
                          )  
    )
})



# SAMPLE OUTPUT
output$multiPropSamplebySplit = renderRHandsontable({
  
    data =  as.data.frame(multiPropbyRatio())
    if ( is.null(data) ){ return() }
    else
    {
      sample_results = data %>% 
                       filter( Group == input$sampleGroup) %>%
                       cast(Split ~ Lift, value = "Samples")
    }  
    sample_results = sample_results[, c("Split", 1,2,3,4,5,10,15)]
    rhandsontable(sample_results, rowHeaders = FALSE, height="100%", width = "100%", rowHeaderWidth=180, colWidths=80)    
})

# ALL OUTPUT
output$multiPropSamplebySplitExecute = renderUI({ multiPropSamplebySplitExecute() })


"******************************
        DOWNLOAD
1.2. EASY-DOWNLOAD
******************************"


# EASY-DOWNLOAD
observe({  
if (is.null( input$download)) 
{ return() } else  
{  
  file = paste("SampleSizeEstimates.csv")
  write.csv( multiPropFile() , file, row.names = FALSE)
}  

})


"********************************************
      
  CONTINUOUS BASED MULTIPLE ESTIMATION 

1.1. RETURN CONTINUOUS DATA FILE
1.2. CONTINUOUS SAMPLE EST. CALCULATOR
1.3. CONTINUOUS SAMPLE EST. EXECUTE
1.4. CONVERT MEAN DIFFERENCE 
1.5. REACTIVE SAMPLE EST. TABLE OUTPUT
1.6. REACTIVE SAMPLE EST. BOX PAGE
********************************************"

# 1.1.
continuous_sample_file = reactive({
  file = input$multiContSample
  if ( is.null(file)){ return() }else{ read.csv(file=file$datapath) }
})

# 1.2.
continuousSampleCalc = function(cont_series, mean_difference, power, significance, ratio )
{
  z_power = round(qnorm( power ),2)
  z_siglevel = round(abs(qnorm(significance/ 2)), 2)
  variance = var(cont_series)
  sample_ratio = ratioCalculator(ratio)
  sample_size = variance*(z_power + z_siglevel)**2/(mean_difference**2)
  return( ceiling(sample_size) )
}

# 1.3.
contiSampleCompute = function( conti_test_data )
{
    groups =  conti_test_data[, 1]
    metrics = names(conti_test_data[sapply(conti_test_data, is.numeric )]) 
    mean_differences = c(.01, .02, .03, .04, .05, .1, .15)
    split_ratios = c(.5, .6, .7, .8, .9)
    all_results = list()
    
    for (group in groups)
    {
      for (metric in metrics)
      {
        series = conti_test_data[conti_test_data[, 1]== group, metric]
        for ( difference in mean_differences )
        {
          est_sample = continuousSampleCalc(series, convertMeanDifferences(series, difference) , .08, .05, .5)
          all_results[[ paste(group, metric, difference) ]] = c(group, metric ,100*difference, est_sample)
        }
        
      } 
    }
    final = as.data.frame(do.call(rbind, all_results))
    colnames(final) = c('Group', 'Metric','EffectSize', 'Sample')
    return(final)

}

# 1.3.
convertMeanDifferences = function(series, difference) { differences =  mean(series)*(1.00+ difference) - mean(series) }

# 1.4.
RunContiSampleEstimation = eventReactive( input$multiContiSampleEstimation, { 
  
  if ( is.null( continuous_sample_file() )) { return()} else { return( contiSampleCompute( continuous_sample_file() )) } 
})

output$continuous_sample_data = renderTable(
{  
      sample_results = RunContiSampleEstimation()
      final_results = sample_results %>% filter(Metric == input$conti_result_metric) %>% cast(Group ~ EffectSize, value = "Sample")
      final_results = final_results[, c("Group", 1, 2,3,4, 5, 10, 15)]
      
})

# 1.5
ContiReactivePage = eventReactive( input$multiContiSampleEstimation, 
{
    if ( !is.null( continuous_sample_file() )) 
    {   
          metrics = as.vector(unique(RunContiSampleEstimation()[, 'Metric']))
          div(
               class = "text-center",
               box( width = 12,
                    solidHeader = TRUE,
                    status = 'primary',
                    title = "Continuous Sample Size Estimation",
                    div( style="padding-left:40%; padding-right:40%;",
                         selectInput( inputId = "conti_result_metric",
                                      label = "Select Metric:",
                                      choices = metrics ), br()),
                    splitLayout(  cellWidths = c("50%", "50%"),
                                  cellArgs = list(style="padding:6px;"),
                                  div( br(),
                                       p( tags$b("Note:"), "The Sample Size output refers to the estimated days needed to detect lift.", br(),
                                          "Lift estimated in percentage from 1-15 computed across all unique groups to be estimated.",br(),
                                          "Use the metric selection above to determine estimated days of test for all metrics") ),
                                  div(  style="padding-left:10%",
                                        tableOutput('continuous_sample_data') )
                    ) )) }
  
})

output$continous_sample_results = renderUI({  ContiReactivePage()   })


"=================================================
          CONTINUOUS SAMPLE BY SPLIT

1.1. SAMPLE BY SPLIT RATIO
1.2. EXECUTE SAMPLE BY SPLIT RATIO
1.3. REACTIVE SAMPLE BY SPLIT RATIO PAGE
1.4. OUTPUT TABLE RESULTS
1.5. MAIN CONTINUOUS SAMPLE OUTPUT BY PAGE
================================================="

# 1.1.
contiSamplebyRatioCompute = function( conti_test_data )
{
  groups =  conti_test_data[, 1]
  metrics = names(conti_test_data[sapply(conti_test_data, is.numeric )]) 
  mean_differences = c(.01, .02, .03, .04, .05, .1, .15)
  split_ratios = c(.5, .6, .7, .8, .9)
  all_results = list()
  
  for (group in groups)
  {
    for (metric in metrics)
    {
      series = conti_test_data[conti_test_data[, 1]== group, metric]
      for ( difference in mean_differences )
      {
        for ( ratio in split_ratios )
        { 
        est_sample = continuousSampleCalc(series, convertMeanDifferences(series, difference) , .08, .05, ratio)
        all_results[[ paste(group, metric, ratio, difference) ]] = c(group, metric, ratio ,100*difference,  est_sample)
        }
      }
    } 
  }
  final = as.data.frame(do.call(rbind, all_results))
  colnames(final) = c('Group', 'Metric', 'Split', 'EffectSize', 'Sample')
  return(final)
  
}

# 1.2. REACTIVE RUN
RunContiSamplebySplitEstimation = eventReactive(input$multiContiSampleEstimation, 
{  
      if(is.null( continuous_sample_file() )){return()} 
      else{ contiSamplebyRatioCompute( continuous_sample_file() ) }  
})


# 1.3. 
ContibySplitReactivePage = eventReactive( input$multiContiSampleEstimation,
{
  
  if( !is.null( continuous_sample_file() ))
  {
    group_level = as.vector(unique(RunContiSamplebySplitEstimation()[, "Group"]))
    metrics = as.vector(unique(RunContiSamplebySplitEstimation()[, "Metric"]))
    div(
        class = "text-center",
        box( width = 12,
             solidHeader = TRUE,
             status = 'primary',
             title = "Continuous Sample Size Estimation By Split",
             div( br(), br(),
                  splitLayout( cellWidths = c("50%", "50%"),
                               cellArgs = list(style="padding=6px;"),
                               div( style="padding-left:15%;padding-right:15%;", 
                                    selectInput( inputId = "cont_group_metric",
                                                 label = "Select Group:",
                                                 choices = group_level, selected = group_level[1])),
                               div( style="padding-left:15%;padding-right:15%;",  
                                    radioButtons( inputId = "cont_metric_split",
                                                  label = "Select Continuous Metric:",
                                                  choices = metrics, inline = TRUE)))
                ),
             div( 
                   splitLayout(  cellWidths = c("50%", "50%"),
                                 cellArgs = list(style="padding:6px;"),
                                 div( br(), br(), br(),
                                      p( tags$b("Note:"), "The Sample Size output refers to the estimated days needed to detect lift.", br(),
                                         "Lift estimated in percentage from 1-15 computed across all unique groups to be estimated.",br(),
                                         "Use the Group selection above to determine estimated days of test for all metrics", br(),
                                         "Notice that split are proportions: .5 = 50-50, .6 = 60-40, .7 = 70-30") ),
                                 div(  style="padding-left:25%",
                                       tableOutput('continuous_results_split'))))))
    
  }
  
})


# 1.3. OUTPUT TEST
output$continuous_results_split = renderTable({ 
  
  results = RunContiSamplebySplitEstimation() 
  results = results %>% filter( Metric == input$cont_metric_split, Group == input$cont_group_metric) %>%
            cast(Split ~ EffectSize , value = "Sample")
  results[, c("Split", 1, 2, 3, 4, 5, 10, 15)]
  
})
output$continuous_results_by_split = renderUI({  ContibySplitReactivePage()  })

# 1.6.
output$continuousMultiPowerPage = renderUI({
  
  fluidPage( 
            fluidRow(
                      column( width = 12,
                              offset = 0,
                              br(),
                              div( class = "text-center", 
                                   h4("Continuous Variable Sample Estimate"), br(),
                                   p("This is the multiple Sample Estimation Page. To begin, enter a csv file in the format below:")),
                              br(),
                              div( class = "text-center",
                                   box( width = 12,
                                        status = 'primary',
                                        solidHeader = TRUE,
                                        title = "Estimation Inputs",
                                        br(),
                                        splitLayout( cellWidths = c("34%", "33%", "33%"),
                                                     cellArgs = list(style='padding:6px;'),
                                                     fileInput( inputId = "multiContSample",
                                                                label = "Upload a CSV:",
                                                                accept = c("text/csv",
                                                                           "text/comma-separated-values, text/plain",
                                                                           ".csv")),
                                                     numericInput( inputId = "multiContPower",
                                                                   label = "Power Level (Recommended .8):",
                                                                   value = .8, min = .6, max = .9, step = .01),
                                                     
                                                     numericInput( inputId = "multiContSig",
                                                                   label = "Significance Level (Recommended .05):",
                                                                   value = .05, min = .01, max = .3, step = .01)),
                                        p("Set the metrics and Run Estimation to compute Sample Size Requirements:"),
                                        actionButton( style="padding-top: -20px;",
                                                      inputId = "multiContiSampleEstimation",
                                                      icon = icon('fire'),
                                                      label = "Run Estimation"))),
                              uiOutput('continous_sample_results'),
                              uiOutput('continuous_results_by_split')))
            )
 
  
})


# MAIN SAMPLE SIZE PAGE LAYOUT
output$sample_size_pages = renderUI({   
  
  tabsetPanel(  tabPanel("Sample Calculator Playground", uiOutput("sampleCalculatorPage")),
                tabPanel("Proportion Sample Size Estimator", uiOutput("mainMultiPropPage")), 
                tabPanel("Continuous Variable Sample Estimation", uiOutput("continuousMultiPowerPage")))  
  
})


"*****************************************************************************

         TEST SEGMENTATION UI AND FUNCTIONS

1.0. REACTIVE DATA OBJECT FOR UPLOAD
1.1. RENDER SELECTION CRITERIA
1.2. GENERAL UI WITH INFORMATION AND DATA UPLOAD

******************************************************************************"

# CHECKING DATA
seg_data = reactive({ 
  
  file = input$seg_file
  if (is.null(file))
    return()
  else
    read.csv(file = file$datapath)
})

# SEGMENTATION CRITERIA
output$segmentation_criteria = renderUI({
  
  # EVALUATE DATA
  if (is.null(seg_data())){ return() }
  else { 
        seg_dataframe = as.data.frame( seg_data() )
        numeric_col = names(seg_dataframe[sapply(seg_dataframe, is.numeric)])
        segmentation_columns = seg_dataframe[, !(colnames(seg_dataframe) %in% c(numeric_col, "Date"))]
        div( 
            splitLayout( cellWidths = c("33%", "33%", "33%"),
                         cellArgs = list(style='padding:6px;'),
                         div( br(), br(),
                              radioButtons( inputId = "segSplitColumn",
                                            label = "Select Segmentation Column:",
                                            choices = names(segmentation_columns), 
                                            inline=TRUE)),
                         div( br(), br(),
                              checkboxGroupInput( inputId = "segmentationMetrics",
                                                  label = "Select Segmentation Metric(s):",
                                                  choices = numeric_col, 
                                                  inline=TRUE)),
                         div( br(),br(),
                              numericInput( inputId = "segSplitRatio",
                                            label = "Select a Split Ratio (.5 = 50-50)",
                                            value = .5, min = .5, max =.9 , step = .05, width = "100%"))),
            br(),
            actionButton( style="padding-top: -20px;",
                          inputId = "executeSegmentation", 
                          icon = icon('fire'),
                          label = "Run Segmentation"), br(), br())
      
  }

})

# SEGMENTATION MODEL
computeSegments = function(alldata)
{
    # SEGMENTATION PARAMETERS
    segment_metrics = input$segmentationMetrics
    split_column = input$segSplitColumn
    
    # FILTERING DATAFRAME
    datafile = alldata[, c(split_column, segment_metrics)]
    train_df = as.data.frame(datafile %>% group_by(get(split_column)) %>% summarise_if(is.numeric,sum, na.rm=TRUE))
    names(train_df) = c(split_column, segment_metrics)
    
    # BUILDING THE MODEL
    segment_length = nrow(train_df)
    
    lprogram = make.lp(nrow = 0, ncol = segment_length*2)
    objective_function = c( as.numeric(train_df[, segment_metrics[1]]), -as.numeric(train_df[, segment_metrics[1]]))
    set.objfn(lprec = lprogram, objective_function)
    set.type(lprec = lprogram, seq(1, segment_length*2), "binary")
    add.constraint(lprogram, objective_function, ">=", 0)
    
    # ADDING CONSTRAINTS
    for (segment_metric in segment_metrics[2:length(segment_metrics)])
    { 
      constraint_new = c( as.numeric(train_df[, segment_metric]), -as.numeric(train_df[, segment_metric]) )
      add.constraint(lprec = lprogram, constraint_new, ">=", -100000)
      add.constraint(lprec = lprogram, constraint_new, "<=", 100000)
    }
    
    # Additional Constraints
    for (i in 1:segment_length){ add.constraint(lprec = lprogram, xt = c(1,1), type = "=", rhs = 1, indices = c(i,segment_length+i)) } 
    
    # SOLVING THE PROBLEM
    solve(lprogram)
    
    # RETRIEVE DATA
    AB = head(get.variables(lprogram), segment_length)
    train_df = cbind(train_df, AB)
    train_df$Segment = if_else(train_df$AB == 0, "Control", "Test")
    
    # RETURNING RESULTS
    segments = train_df[, c(split_column, "Segment")]
    final_results = merge(x = alldata, y = segments, by = split_column, all.x=TRUE )
    return(final_results)
}

# EVENT REACTIVE FOR TEST SEGMENTATION
executeTestSegmentation = eventReactive( input$executeSegmentation, 
{
     results = computeSegments(seg_data())                                  
     return(results)                                  
})

# EXECUTING SEGMENTATATOIN RESULTS
#segmentationResultsFile = executeTestSegmentation()

# OUTPUT PLOT
output$segmentation_plot = renderPlot({
  
    split_column = input$segSplitColumn
    data = executeTestSegmentation()
    data$Date = mdy(data$Date)
    if ( is.null(data) ){ return() }
    else {
      
      plotting_data = as.data.frame(data %>% group_by(Date, Segment) %>% summarise_if(is.numeric, sum, na.rm=TRUE))
      ggplot( data = plotting_data, aes(x = Date , y=get(input$segPlotMetric), group=Segment, color=Segment)) + 
              geom_line() +
              labs( title= paste( "Time Series Split of", input$segPlotMetric),
                    x = "Date",
                    y =input$segPlotMetric) +
              theme(plot.title = element_text(hjust = 0.5, size = 13, face="bold"))
      
    }
  
})


# OUTPUT TEST CONTROL TABLE
output$test_control_table = renderRHandsontable({
  
    split_column = input$segSplitColumn
    data = executeTestSegmentation()
    if ( is.null(data) )
     { return() }
    else {
    
      summary_tbl = as.data.frame(table(unique(data[, c("Segment", split_column)])[, "Segment"]))
      names(summary_tbl) = c("Split", "Metric Count")
      rhandsontable(summary_tbl, colWidths = 160, rowHeaders = FALSE)
    }
})

# SEGMENTATION REPORT DOWNLOAD
observe({  
  
    if (is.null( input$downloadSegmentation)) 
    { return() } else  
    {  
      file = paste("SegmentatoinReport.csv")
      write.csv( executeTestSegmentation() , file, row.names = FALSE)
    }  
})


# REACTIVE UI FOR SEGMENTATION RESULTS
segmentationResults = eventReactive(input$executeSegmentation,
{
  executeTestSegmentation()
  fluidRow( 
           column( width = 12,
                   offset = 0,
                   div( class='text-center', 
                        box( width = 12,
                             status = 'info',
                             solidHeader = TRUE,
                             title = "Test Segmentation Results",
                             br(),
                             splitLayout( cellWidths = c("60%", "40%"),
                             div(  
                                  div( style="padding-left:30%;",
                                       selectInput( inputId = "segPlotMetric",
                                                     label = "Select Metric to Plot",
                                                     choices = input$segmentationMetrics,
                                                     width = "50%")),
                                   plotOutput("segmentation_plot")),
                             div( style="display: inline-block; vertical-align: text-top; ",
                                  br(), br(), br(), br(), br(), br(),  
                                  h5("General Segmentation Summary and Download"), br(),
                                  div(),
                                  rHandsontableOutput("test_control_table"),
                                  br(), br(),
                                  actionButton(inputId = "downloadSegmentation" , 
                                               label = "Download Full Segmentation Report", 
                                               icon = icon("download"),
                                               style="color: #fff; background-color: #337ab7;")) )
                             ))))
  
  
})

# RESULTS UI
output$segmentationResults = renderUI({ segmentationResults()  })


# GENERAL MAIN PAGE 
output$segmentation_ui = renderUI({
  
  fluidPage(
             fluidRow( 
                       column(width = 12,
                              offset = 0,
                              br(),
                              h4("Test Segmentation", align='center'),
                              div( class='text-center',
                                   p("Test Segmentation will split the traffic based on a specified
                                      splitting column selected by the user and provided by the algorithm")), br(),
                              div( class = 'text-center',  
                                   box( width = 12,
                                        status = 'primary',
                                        solidHeader = TRUE,
                                        title = "Test Segmentation Metric Selection",
                                        div( class = 'text-center',
                                             style="display:inline-block;padding-left:30%;padding-right:30%",
                                             br(),
                                             fileInput( inputId = "seg_file",
                                                        label = "Upload Segmentation CSV:",
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values, text/plain",
                                                                   ".csv"),
                                                        width = "100%")),
                                        div( uiOutput('segmentation_criteria') ))))),
               uiOutput("segmentationResults")
  )
  
})


# MAIN TEST SEGMENTATION PAGE LAYOUT
output$test_segementation_page = renderUI({
  
    tabsetPanel(  tabPanel("Test Segmentation", uiOutput('segmentation_ui')))
  
})


"*************************************************************

         STATISTICAL ANALYSIS UI AND FUNCTIONS

1.1. GENERAL TESTING - TRADITIONAL TESTING  
1.2. CAUSAL IMPACT ANALYSIS PAGE 
*************************************************************"

"**********************************************
    GENERAL TESTING FUNCTIONS

1.1. REACTIVE OBJECT TO READ DATA
1.2. BUILD CUSTOM METRICS OUTPUT
1.3. OUTPUT GENERAL TESTING METRICS
1.4. RENDER CUSTOM METRIC UI CHOICES
1.5. TEST EXECUTION BUTTON
**********************************************"

# 1.1.
general_test_data = reactive({
  
    file = input$general_test_file
    if ( is.null( file) ){ return() } else { read.csv(file = file$datapath ) }      

})

# 1.2.
custom_metrics = eventReactive( input$general_test_metrics,
{
  if ("Custom Metric" %in% input$general_test_metrics)
  {
    div(
      splitLayout( cellWidths = c("50%", "50%"),
                   cellArgs = list(style='padding:6px;'), 
                   div( 
                     splitLayout( cellWidths = c("50%", "50%"),
                                  cellArgs = list(style='padding:6px;'),
                                  div( br(),
                                       textInput( inputId = "numerator_1", label = "Metric Numerator:", placeholder = 'Clicks'),
                                       textInput( inputId = "numerator_2", label = "Metric Numerator: ", placeholder = 'Conversions'),
                                       textInput( inputId = "numerator_3", label = "Metric Numerator:", placeholder = 'Conversions')),
                                  div( br(),
                                       textInput( inputId = "denominator_1", label = "Metric Denominator:", placeholder = 'Impressions'),
                                       textInput( inputId = "denominator_2", label = "Metric Denominator:", placeholder = 'Clicks'),
                                       textInput( inputId = "denominator_3", label = "Metric Denominator:", placeholder = 'Impressions')))),
                   div(  p( style='align:left', br(),
                            h5("Custom Metric Information:"), br(),
                            "Custom Metric allows for assigning specific proportion metrics you need to test.", br(),
                            tags$b("For example:"),
                            "Conversions/Impressions can be specified respectively as numerator and denominator", br(), br(),
                            tags$b("Metric Numerator:"), "Conversions", br(),
                            tags$b("Metric Denominator:"), "Impressions", br(), br(),
                            tags$b("Note:"), "Metrics selected should match column names from the data upload.")
                   )))
  } else { return()}
})

# 1.3.
output$general_custom_metrics = renderUI({ custom_metrics() })

# 1.4.
output$general_testing_parameter = renderUI({
  
  if( is.null(general_test_data())){ return() } 
  else {
         numeric_columns = names(as.data.frame( general_test_data() )[sapply( as.data.frame( general_test_data() ), is.numeric)])
         numeric_columns = numeric_columns[!numeric_columns == 'Date']
         if ( input$general_test_options == "Proportion Based Test" ) { test_metrics = c("Click-Through Rate"="Clicks/Impressions", 
                                                                                         "Conversion Rate"="Conversions/Clicks", 
                                                                                         "Conversions-Impressions"="Conversions/Impressions",
                                                                                         "Custom Metrics"="Custom Metric") }
         else { test_metrics = numeric_columns }
         return(  
                   div(  splitLayout( 
                                      cellWidths = c("50%", "50%") ,
                                      cellArgs = list(style="padding:6px"),
                                      div( 
                                           checkboxGroupInput( inputId = "general_test_metrics" ,
                                                               label = "Select Testing Metric:",
                                                               choices = test_metrics, inline = TRUE)), 
                                      div(  
                                            style="padding-left:10%;padding-right:10%",
                                            numericInput( inputId = "general_test_alpha",
                                                          label = "Select Signficance Level - (Recommended = .05):",
                                                          value = .05, min = .01, max = .5, step = .01) )),
                         uiOutput('general_custom_metrics'),
                         uiOutput('test_execution_button'))
           )
}
})

# 1.5
output$test_execution_button = renderUI({ 
  
  if ( length(input$general_test_metrics) > 0 )
  {   
    return( div( style="padding-top: 10px;",
                 actionButton( style="padding-top: -20px;",
                               inputId = "run_general_test", 
                               label = "Run Test", icon = icon('fire'))))
  } else { return() }  
  
})

"========================================
     PROPORTION TESTING FUNCTION   
1.1. INPUT TABLE FOR TEST
1.2. PROPORTION VECTOR 
1.3. TESTING FUNCTION
1.4. DIFFERENCE FUNCTION
========================================="
# 1.1.
propInputTable = function(data, dimension, metric)
{
  input_tbl = data[ data[, 1] == dimension, c( trimws(strsplit(metric, '/')[[1]][1]), trimws(strsplit(metric, '/')[[1]][2]))]
  row.names(input_tbl) = data[ , 2][data[ , 1] == dimension]
  return(input_tbl)
}

# 1.2.
proportionTable = function(data, dimension, metric)
{
  input_tbl = data[data[, 1]== dimension, c( trimws(strsplit(metric, '/')[[1]][1]), trimws(strsplit(metric, '/')[[1]][2]))]
  row.names(input_tbl) = data[ , 2][data[ , 1] == dimension]
  
  proportion = input_tbl[ , 1]/input_tbl[ , 2]
  names(proportion) = data[ , 2][data[ , 1] == dimension]
  return(proportion)
}

# 1.3.
propTest = function(input_tbl, conf_level, alternative_type)
{
  general_prop_test = prop.test( x = input_tbl[,1],  n = input_tbl[,2],  conf.level = conf_level,  alternative = alternative_type)
  return(general_prop_test)  
}

# 1.4.
DifferenceFunction = function(p_value , proportion_level)
{
  if ( p_value < input$general_test_alpha )
  { 
    if( length(proportion_level) > 2 ){ return("Post Hoc") }
    else { return( names(which.max(proportion_level))) }
  }
  else { return("No Difference")  }
}


"==========================================
     PROPORTION TESTING FUNCTION

1. FULL PROP TESTING FUNCTION
2. FULL PROP STATISTIC FUNCTION
3. FULL CONTINUOUS TESTING FUNCTION
4. FULL CONTINUOUS STATISTIC FUNCTION
5. FULL CUMULATIVE TESTING FUNCTION
6. FULL CUMULATIVE STATISTIC FUNCTION
=========================================="
# 1.
FullPropTestFunction = function(prop_data_input)
{
  metrics = c( input$general_test_metrics[!input$general_test_metrics == 'Custom Metric'], paste( input$numerator_1, '/', input$denominator_1), 
               paste( input$numerator_2, '/', input$denominator_2), paste( input$numerator_3, '/', input$denominator_3) )
  
  dimensions = as.vector(unique(prop_data_input[, 1]))
  Results_df = data.frame()
  for ( dimension in dimensions )
  {
    for (metric in metrics)
    {
      if ( (strsplit(metric, '/')[[1]][1] == " " ) || (strsplit(metric, '/')[[1]][2] == " ") )
      { Results_df = Results_df }
      else {
        test_tbl = propInputTable(data = prop_data_input, dimension = dimension, metric = metric )
        prop_tbl = proportionTable(data = prop_data_input, dimension = dimension, metric = metric )
        prop_test_results = propTest( input_tbl = test_tbl, conf_level = 1 - input$general_test_alpha, 
                                      alternative_type = "two.sided" )
        winner = DifferenceFunction(p_value = prop_test_results$p.value, prop_tbl)
        Results_df[dimension, metric] = winner
      }
    }
  }
  return(Results_df)
}

# 2
FullPropStatFunction = function(prop_data_input)
{
  metrics = c( input$general_test_metrics[!input$general_test_metrics == 'Custom Metric'], 
               paste( input$numerator_1, '/', input$denominator_1), paste( input$numerator_2, '/', input$denominator_2), 
               paste( input$numerator_3, '/', input$denominator_3) )
  dimensions = as.vector(unique(prop_data_input[, 1]))
  Results_df = data.frame()
  for ( dimension in dimensions )
  {
    for (metric in metrics)
    {
      if ( (strsplit(metric, '/')[[1]][1] == " " ) || (strsplit(metric, '/')[[1]][2] == " ") )
      { Results_df = Results_df }
      else {
        test_tbl = propInputTable(data = prop_data_input, dimension = dimension, metric = metric )
        prop_tbl = proportionTable(data = prop_data_input, dimension = dimension, metric = metric )
        prop_test_results = propTest( input_tbl = test_tbl, conf_level = 1 - input$general_test_alpha, 
                                      alternative_type = input$general_test_alternative )
        Results_df[dimension, metric] = paste("P-Value:", round(prop_test_results$p.value, 3), ",",
                                              "Chi-Square:", round(prop_test_results$statistic[['X-squared']],3), 
                                              collapse = " ")
      }
    }
  }
  return(Results_df)
}


# 3
FullContinuousTest = function(continuous_test_file)
{
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_test_file[,2]))
  for (dimension in dimensions)
  { 
    testing_data = continuous_test_file[continuous_test_file[,2]== dimension, ]  
    for (metric in input$general_test_metrics)
    {
      test =  cast(testing_data, Date ~ Group, value = metric )[, "Test"]
      control = cast(testing_data, Date ~ Group, value = metric )[, "Control"]
      test_results = wilcox.test(x = control, y = test, 
                                 alternative = "two.sided", 
                                 conf.int = 1 - input$general_test_alpha )
      if (test_results$p.value < input$general_test_alpha )
      {
        if ( (mean(test) - mean(control)) > 0 ) { Winner = "Test"}
        else if ( (mean(test) - mean(control)) < 0 ) { Winner = "Control" } 
      } else { Winner = "No Difference" }
      
      Results_df[dimension, metric] = Winner
    }
  }
  return(Results_df)
}

# 4
FullContinuousStat = function(continuous_test_file)
{
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_test_file[,2]))
  for (dimension in dimensions)
  { 
    testing_data = continuous_test_file[continuous_test_file[,2]== dimension, ]  
    for (metric in input$general_test_metrics)
    {
      test =  cast(testing_data, Date ~ Group, value = metric )[, "Test"]
      control = cast(testing_data, Date ~ Group, value = metric )[, "Control"]
      test_results = wilcox.test(x = control, y = test, 
                                 alternative = input$general_test_alternative, 
                                 conf.int = 1 - input$general_test_alpha )
      Results_df[dimension, metric] = paste("P-Value:", round(test_results$p.value, 3), ",",
                                            "Statistic:", round(test_results$statistic[['W']],3), 
                                            collapse = " ")
    } 
  }
  return(Results_df)
}

# 5.
FullCumulativeTest = function(continuous_test_file)
{
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_test_file[,2]))
  print(dimensions)
  for (dimension in dimensions)
  { 
    testing_data = continuous_test_file[continuous_test_file[,2]== dimension, ]  
    for (metric in input$general_test_metrics)
    {
      test =  cast(testing_data, Date ~ Group, value = metric )[, "Test"]
      control = cast(testing_data, Date ~ Group, value = metric )[, "Control"]
      test_results = ks.test(x = control, y = test,
                             #conf.int = 1 - input$general_test_alpha,
                             alternative = 'two.sided')
      if (test_results$p.value < input$general_test_alpha )
      {
        if ( (sum(test) - sum(control)) > 0 ) { Winner = "Test"}
        else if ( (sum(test) - sum(control)) < 0  ) { Winner = "Control" } 
      } else { Winner = "No Difference" }
      
      Results_df[dimension, metric] = Winner
    }
  }
  return(Results_df)
}

# 6.
FullCumulativeStat = function(continuous_test_file)
{
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_test_file[,2]))
  for (dimension in dimensions)
  { 
    testing_data = continuous_test_file[continuous_test_file[,2]== dimension, ]  
    for (metric in input$general_test_metrics)
    {
      test =  cast(testing_data, Date ~ Group, value = metric )[, "Test"]
      control = cast(testing_data, Date ~ Group, value = metric )[, "Control"]
      test_results = ks.test( x = control, y = test, 
                              #conf.int = 1 - input$general_test_alpha,
                              alternative = 'two.sided' )
      Results_df[dimension, metric] = paste("P-Value:", round(test_results$p.value, 3), ",",
                                            "Statistic:", round(test_results$statistic[['D']],3), collapse = " ")
    }
  }
  return(Results_df)
}

"==========================================
     GENERAL TESTING EXECUTION  

1. EXECUTE TEST
2. EXECUTE STATISTICS
3. TEST AND STATISTIC OUTPUT ID
4. RENDER/DISPLAY TEST RESULTS
5. OVERAL GENERAL TESTING PAGE
=========================================="
# 1.
RunGeneralTest = eventReactive( input$run_general_test, 
{
  if ( input$general_test_options == "Proportion Based Test" ){ return(FullPropTestFunction( general_test_data() )) } 
  else if (input$general_test_options == "Continuous Variable Test") { return( FullContinuousTest( general_test_data() )) }
  else if ( input$general_test_options == "Cumulative Variable Test" ){ return( FullCumulativeTest( general_test_data() ) ) }

})

# 2.
RunGeneralStat = eventReactive( input$run_general_test, 
{
  if ( input$general_test_options == "Proportion Based Test" ){ return(FullPropStatFunction( general_test_data() )) } 
  else if (input$general_test_options == "Continuous Variable Test") { return( FullContinuousStat( general_test_data() )) }
  else if ( input$general_test_options == "Cumulative Variable Test" ){ return( FullCumulativeStat( general_test_data() ) ) }
  
} )


# 3 
output$general_test = renderDataTable({  RunGeneralTest()  })
output$general_stat = renderDataTable({ RunGeneralStat() })

# 4
DisplayTestResults = eventReactive(input$run_general_test,
{
  div(
    box( width = 12,
         offset= 0,
         solidHeader = TRUE,
         status = 'primary',
         title = "Test Results",
         tabsetPanel( tabPanel('Test Results', dataTableOutput('general_test')),
                      tabPanel('Test Statistics', dataTableOutput('general_stat')))
    ))
})

output$general_test_results = renderUI({ DisplayTestResults()  })


# 5
output$general_analysis_main_page = renderUI({

    fluidPage(  
               fluidRow( 
                        column( width = 12,
                                offset = 0,
                                br(), 
                                h4("General Statistical Analysis" ,align='center'),
                                div( class="text-center",
                                     p("General Statistical Analysis provides a variety of testing options based on traditional frequentist techniques", 
                                        br(),
                                        "Use this methods to examine difference between proportions of CTR and Conversion Rate, analyze the difference in
                                        means for Average Order Value or test for significance for cumulative metric such as Revenue or Profit.")),
                                br(),
                                div(  class = "text-center",
                                      box( width = 12,
                                           status = 'primary',
                                           solidHeader = TRUE,
                                           title = "Data Input and Test Selection",
                                           splitLayout( cellWidths = c("50%", "50%"),
                                                        cellArgs = list( style="padding:6px;" ),
                                                        div( br(),
                                                             style="display:inline-block;padding-left:20%;padding-right:20%",
                                                             fileInput( inputId = "general_test_file",
                                                                        label = "Enter Test Data CSV File:",
                                                                        accept = c("text/csv",
                                                                                   "text/comma-separated-values, text/plain",
                                                                                   ".csv"),
                                                                        width="100%")),
                                                         div( br(),
                                                              radioButtons( inputId = "general_test_options",
                                                                            label = "Testing Options:",
                                                                            choices = c("Proportion Based Test",
                                                                                        "Continuous Variable Test",
                                                                                        "Cumulative Variable Test"),
                                                                            width='100%', inline=TRUE)  
                                                        )),
                                           uiOutput('general_testing_parameter'))),
                                           uiOutput('general_test_results'))))
                                           
})


"********************************************

    CAUSAL IMPACT MAIN UI

1.1. READING CAUSAL IMPACT FILE
1.2. RETURN DATES FROM THE FILE
1.3. PROCESSING CAUSAL DATA
1.4. GENERIC CAUSAL IMPACT MODEL
1.5. CUSTOM CAUSAL IMPACT MODEL
********************************************"

# 1.1.
causal_impact_data = reactive({
  
  file = input$causal_impact_file
  if (is.null(file)) { return() } else { read.csv(file = file$datapath) }
})

# 1.2.
testing_period = function(datafile){ return( c(min(ymd(datafile$Date)), max(ymd(datafile$Date)))) }

# 1.3. 
processing_causal_data = function( datafile, target_variable )
{
    # CHANGE THE DATE AS NEEDED
    datafile$Date = ymd(datafile$Date)
    if ( 'Group' %in% names(datafile) )
    {   test_data = cast(datafile, Date ~ Group, value = target_variable, fun.aggregate = sum)   
        return( zoo( test_data[, c('Test', 'Control')], test_data$Date) )
    } 
    else 
    { 
        covariates = names(test_data)[!names(test_data) %in% c(target_variable, 'Date')]
        return( zoo( test_data[, c(target_variable, covariates )], test_data$Date  )  )
    }
}

# 1.4.
generic_model = function( model_data_input, full_dates, test_date )
{
  # BUILDING THE CAUSAL IMPACT MODEL
  impact_model = CausalImpact( model_data_input, 
                               pre.period = c( full_dates[1], ymd(test_date)-1),
                               post.period = c(ymd(test_date), full_dates[2]),
                               model.args = list(niter = 1000, nseasons = 52, season.duration=7 ))
  return(impact_model)
}

# 1.5.
custom_model = function(model_data_input, full_dates, test_date, niter ,seasons, season_duration)
{
  impact_model = CausalImpact( model_data_input, pre.period = c( full_dates[1], ymd(test_date)-1),
                               post.period = c(ymd(test_date), full_dates[2]),
                               model.args = list(niter = niter, nseasons = seasons, season.duration=season_duration ))
  return(impact_model)
}

# FUNCTION TO CLEAN THE DATA
# 1.1.
causal_effects = function(data_file, metric_1, metric_2, metric_type, type)
{
  if ( type == 'relative' )
  {
    return( paste(  round( data_file[ metric_1, metric_type]*100, 1), '%',
                    " ", '(',
                    round( data_file[ metric_2, metric_type]*100, 1), '%',')',
                    collapse = "", sep = ""  ) )
  } else {
    
    return( paste(  round( data_file[ metric_1, metric_type], 0),
                    " ", '(',
                    round( data_file[ metric_2, metric_type], 0), ')',
                    collapse = "", sep = ""  ) )
  }
}

# 1.2.
causal_intervals = function(data_file, metric_1, metric_2, metric_type, type)
{
  if (type == 'relative')
  {
    return( paste( '[',  
                   round( data_file[ metric_1 , metric_type ]*100, 1 ), "%", ",", " ",  
                   round( data_file[ metric_2 , metric_type ]*100, 1 ), "%", ']',
                   collapse = "", sep = "")  )
  } else {
    return( paste( '[',  
                   round( data_file[ metric_1 , metric_type ], 0 ), ",", " ",  
                   round( data_file[ metric_2 , metric_type ], 0 ), ']',
                   collapse = "", sep = "") )
    
  }
}

# 1.3.
CausalModelSummary = function( results_data )
{
  # Transpose Dataframe
  causal_model_results = t(results_data)
  actual = round( causal_model_results['Actual', 'Average'], 0)
  cumulative = round( causal_model_results['Actual', 'Cumulative'], 0)
  
  # ACTUAL PREDICTIONS
  actual_avg_predictions = causal_effects(causal_model_results, 'Pred', 'Pred.sd', 'Average', 'absolute')
  actual_cum_predictions = causal_effects(causal_model_results, 'Pred', 'Pred.sd', 'Cumulative', 'absolute')
  actual_avg_confint = causal_intervals(causal_model_results, 'Pred.lower', 'Pred.upper', 'Average', 'absolute')
  actual_cum_confint = causal_intervals(causal_model_results, 'Pred.lower', 'Pred.upper', 'Cumulative', 'absolute')
  
  # ABSOLUTE EFFECT
  absolute_avg_predictions = causal_effects( causal_model_results, 'AbsEffect' , 'AbsEffect.sd' , 'Average', 'absolute' )
  absolute_cum_predictions = causal_effects( causal_model_results, 'AbsEffect', 'AbsEffect.sd', 'Cumulative', 'absolute')
  absolute_avg_confint = causal_intervals( causal_model_results, 'AbsEffect.lower', 'AbsEffect.upper', 'Average', 'absolute' )
  absolute_cum_confint = causal_intervals( causal_model_results, 'AbsEffect.lower', 'AbsEffect.lower', 'Cumulative', 'absolute' )
  
  relative_avg_predictions = causal_effects( causal_model_results, 'RelEffect', 'RelEffect.sd', 'Average', 'relative')
  relative_cum_predictions = causal_effects( causal_model_results, 'RelEffect', 'RelEffect.sd', 'Cumulative', 'relative' )
  relative_avg_confint = causal_intervals( causal_model_results, 'RelEffect.lower', 'RelEffect.upper', 'Average', 'relative' )
  relative_cum_confint = causal_intervals( causal_model_results, 'RelEffect.lower', 'RelEffect.upper', 'Cumulative', 'relative')
  
  # COMBINING
  actuals = c(actual, cumulative)
  predictions = c(actual_avg_predictions, actual_cum_predictions )
  conf_interval = c(actual_avg_confint, actual_cum_confint)
  absolute_effect = c(absolute_avg_predictions, absolute_cum_predictions)
  absolute_confint = c(absolute_avg_confint, absolute_cum_confint)
  relative_effect = c(relative_avg_predictions, relative_cum_predictions)
  relative_confint = c(relative_avg_confint, relative_cum_confint)
  
  all_data = rbind( actuals, predictions, conf_interval, absolute_effect, absolute_confint, relative_effect, relative_confint)
  colnames(all_data) = c( 'Average', 'Cumulative')
  row.names(all_data) = c( "Actual", "Prediction (s.d)", "Predition CI", "Absolute Effect (s.d)", "Absolute Effect CI", 
                           "Relative Effect (s.d)", "Relative Effect CI")
  return(as.data.frame(all_data))
}


# REACTIVE OBJECT FOR CAUSAL IMPACT
execute_Causal_Impact = eventReactive( input$run_causalImpact, {
  
  full_test_dates = testing_period(causal_impact_data())
  causal_data = processing_causal_data( as.data.frame(causal_impact_data()), input$target_variable )
  if ( input$model_type == "Generic Causal Impact" )
  {
    impact_model = generic_model(causal_data, full_test_dates, input$test_date )
    return(impact_model)
  } else if ( input$model_type == "Custom Causal Impact" )
  {
    impact_model = custom_model( causal_data, full_test_dates, input$test_date, 
                                 niter = input$niter_input,
                                 seasons = input$seasonal_input,
                                 season_duration = input$seasonal_duration)
    return(impact_model)
  }
  
  
})

# REACTIVE RESULTS
causal_reactive_results = eventReactive( input$run_causalImpact,
{
   column( width = 12,
           offset = 0,
           div( #class='text-center',
                box( width = 12,
                     status = 'info',
                     solidHeader = TRUE,
                     title = "Causal Impact Results",
                     br(),
                     splitLayout( cellWidths = c("50%", "50%"),
                                  cellArgs = list(style="padding:6px;"),
                                  div(  #br(),
                                        plotOutput( 'impact_plot'),
                                        div( class='text-center',
                                             style="display: inline-block; vertical-align: text-top; 'padding-left:20px; ",
                                             br(),
                                             dataTableOutput('impact_table')
                                             #rHandsontableOutput('impact_table')
                                             )),
                                  div(  br(),
                                        style="word-wrap: break-word;",
                                        verbatimTextOutput( 'impact_summary') )

          ))))
})


# GENERATING PLOT
output$impact_plot = renderPlot({ 
  
   causal_plot = plot(execute_Causal_Impact()) 
   causal_plot + theme_linedraw() + 
     ggtitle("Causal Impact Time Series View") + 
     theme(plot.title = element_text(hjust = 0.5, size = 17, face='bold'))
})

# GENERATING SUMMARY
output$impact_summary = renderText({  paste(execute_Causal_Impact()$report) })


output$impact_table = renderDataTable({ CausalModelSummary( execute_Causal_Impact()$summary) })
                                           # rhandsontable( CausalModelSummary( execute_Causal_Impact()$summary), 
                                          #                 colWidths = 160, 
                                           #                rowHeaderWidth=170, 
                                          #                 strict = TRUE )   })

# COMBINED OUTPUT
output$causal_overall_results = renderUI({ causal_reactive_results()  })


# 1.6. 
output$model_selection_inputs = renderUI({
  
    if ( input$model_type == "Generic Causal Impact")
    {
      div( p(" The Base Model uses Causal Impact pre-defined parameter") )
    } else if ( input$model_type == "Custom Causal Impact" )
    {
      splitLayout( cellWidths = c("30%", "30%", "30%"),
                   cellArgs = list(style="padding:3px;"),
                   numericInput( inputId = "niter_input",
                                 label = "Select Parameters:",
                                 value = 500, min = 1, max = 10000, step = 1),
                   numericInput( inputId = "seasonal_input",
                                 label = "Select Season Input:",
                                 value = 52, min = 100, max = 500, step = 1),
                   numericInput( inputId = "seasonal_duration",
                                 label = "Select Seasonal Duration:",
                                 value = 7, min = 1, max = 30, step = 1))
    }
  
})


# 1.7. 
output$model_specification = renderUI({
  
    if ( is.null( causal_impact_data() ) )
      return()
    else
      numeric_columns = names(as.data.frame(causal_impact_data())[sapply( as.data.frame(causal_impact_data()), is.numeric)])
      numeric_columns = numeric_columns[!numeric_columns == 'Date']
      div(
           div(
           splitLayout( cellWidths = c("50%", "50%"),
                        div( br(),
                             radioButtons( inputId = "model_type",
                                           label = "Select Model Type",
                                           choices = c("Generic Causal Impact", "Custom Causal Impact"),
                                           inline = TRUE),
                             br(),
                             uiOutput("model_selection_inputs")),
                        div( br(),
                             radioButtons( inputId = "target_variable",
                                           label = "Select Target Variable",
                                           choices = numeric_columns,
                                           inline = TRUE),
                             br(),
                             div( style="padding-left:30%;padding-right:30%",
                                  dateInput( inputId = "test_date",
                                             label = "Test Date (in format):",
                                             value = Sys.Date()
                                             )))
                      )),
           br(),
           div( actionButton( style="padding-top: -20px;",
                              inputId = "run_causalImpact", 
                              label = "Run Causal Impact", icon = icon('fire') ))
      )
  
})


# CAUSAL IMPACT PAGE
output$causal_impact_main_page = renderUI({
  
  fluidPage(
             fluidRow(
                       column( width = 12,
                               offset = 0,
                               br(),
                               h4("Causal Impact Analysis" ,align='center'),
                               div( class="text-center",
                                    p("Causal Impact analysis is a robust statistical analysis techiques", br()),
                                    div( class = 'text-center',  
                                         box( width = 12,
                                              status = 'primary',
                                              solidHeader = TRUE,
                                              title = "Causal Impact Analysis Specifications",
                                              div( class = 'text-center',
                                                   style="display:inline-block;padding-left:30%;padding-right:30%",
                                                   br(),
                                                   fileInput( inputId = "causal_impact_file",
                                                              label = "Upload Data in CSV:",
                                                              accept = c("text/csv",
                                                                         "text/comma-separated-values, text/plain",
                                                                         ".csv"),
                                                              width = "100%")),
                                          uiOutput('model_specification'),
                                          br())))), 
                       uiOutput('causal_overall_results')
             )
  )
})

# MAIN TEST ANALYSIS PAGE LAYOUT
output$statistical_analysis_page = renderUI({
  
  tabsetPanel( tabPanel("General Statistical Analysis", uiOutput('general_analysis_main_page')),
               tabPanel("Causal Impact Analysis", uiOutput('causal_impact_main_page')))
  
}) }



"******************************
  
        RUN APPLICATION

******************************"
shinyApp(  ui = dashboardPage( skin = 'blue',
                               header = header,
                               sidebar = sidebar,
                               body = body ),
           server = server)