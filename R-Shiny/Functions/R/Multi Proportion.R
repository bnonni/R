
# 1.2.
output$general_testing_metrics = renderUI({
  
  if ( is.null( general_test_data() ) ){ return() }
  else {
    numeric_columns = names(as.data.frame( general_test_data() )[sapply( as.data.frame( general_test_data() ), is.numeric)])
    numeric_columns = numeric_columns[!numeric_columns == 'Date']
    if ( input$general_test_options == "Proportion Based Test" )
    {
      return( div( br(), br(),
                   checkboxGroupInput( inputId = "general_prop_metrics",
                                       label = "Select Metrics for Test:", 
                                       choices = c( 'Click-Through Rate'="Clicks/Impressions",
                                                    'Conversion Rate'="Conversions/Clicks",
                                                    'Conversion-Impressions' = 'Conversions/Impressions',
                                                    'Custom Metrics' = "Custom Metric"), 
                                       inline = TRUE)))
    }
    else if ( input$general_test_options == "Continuous Variable Test" )
    {
      return(  div( br(), br(),
                    checkboxGroupInput( inputId = "general_continuous_metrics",
                                        label = "Select Metrics for Test:", 
                                        choices = numeric_columns,
                                        inline = TRUE)))
    }
    else if ( input$general_test_options == "Cumulative Variable Test" ) 
    {
      return(  div( br(), br(),
                    checkboxGroupInput( inputId = "general_cumulative_metrics",
                                        label = "Select Metrics for Test:", 
                                        choices = numeric_columns,
                                        inline = TRUE)))
    } 
  }
})

# 1.3.
general_custom_metrics = eventReactive( input$general_prop_metrics,
                                        {
                                          numeric_columns = names(as.data.frame( general_test_data() )[sapply( as.data.frame( general_test_data() ), is.numeric)])
                                          numeric_columns = numeric_columns[!numeric_columns == 'Date']
                                          if ( "Custom Metric" %in% input$general_prop_metrics)
                                          { 
                                            div(
                                              splitLayout( cellWidths = c("50%", "50%"),
                                                           cellArgs = list(style='padding:6px;'), 
                                                           div( 
                                                             splitLayout( cellWidths = c("50%", "50%"),
                                                                          cellArgs = list(style='padding:6px;'),
                                                                          div( br(),
                                                                               textInput( inputId = "numerator_1",
                                                                                          label = "Metric Numerator:",
                                                                                          placeholder = 'Clicks'),
                                                                               textInput( inputId = "numerator_2",
                                                                                          label = "Metric Numerator: ",
                                                                                          placeholder = 'Conversions'),
                                                                               textInput( inputId = "numerator_3",
                                                                                          label = "Metric Numerator:",
                                                                                          placeholder = 'Conversions')),
                                                                          div( br(),
                                                                               textInput( inputId = "denominator_1",
                                                                                          label = "Metric Denominator:",
                                                                                          placeholder = 'Impressions'),
                                                                               textInput( inputId = "denominator_2",
                                                                                          label = "Metric Denominator:",
                                                                                          placeholder = 'Clicks'),
                                                                               textInput( inputId = "denominator_3",
                                                                                          label = "Metric Denominator:",
                                                                                          placeholder = 'Impressions')))),
                                                           div(  p( style='align:left', br(),
                                                                    h5("Custom Metric Information:"), br(),
                                                                    "Custom Metric allows for assigning specific proportion metrics you need to test.", br(),
                                                                    tags$b("For example:"),
                                                                    "Conversions/Impressions can be specified respectively as numerator and denominator", br(), br(),
                                                                    tags$b("Metric Numerator:"), "Conversions", br(),
                                                                    tags$b("Metric Denominator:"), "Impressions", br(), br(),
                                                                    tags$b("Note:"), "Metrics selected should match column names from the data upload.")
                                                           ))
                                            )
                                          } else { return() }    
                                          
                                        })

# 1.4.
output$general_custom_metrics = renderUI({ if ( input$general_test_options == "Proportion Based Test") { general_custom_metrics() } })

# 1.5.
output$general_test_execution = renderUI({   
  
  if (  (length(input$general_prop_metrics) > 0 ) || 
        (length(input$general_continuous_metrics) > 0) || 
        ( length(input$general_cumulative_metrics) > 0)   )
  {  
    div( style="padding-top: 10px;",
         actionButton( style="padding-top: -20px;",
                       inputId = "general_prop_test_execute", 
                       label = "Run Test", icon = icon('fire'))) }
  
})


"**********************************
PROPORTION TESTING FUNCTIONS

*********************************" 
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
  general_prop_test = prop.test( x = input_tbl[,1], 
                                 n = input_tbl[,2], 
                                 conf.level = conf_level, 
                                 alternative = alternative_type)
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


# 1.5.
overallPropTesting = function(prop_data_input)
{
  # CUSTOM METRIC
  total_metrics = c( input$general_prop_metrics[!input$general_prop_metrics == 'Custom Metric'] , 
                     paste( input$numerator_1, '/', input$denominator_1), 
                     paste( input$numerator_2, '/', input$denominator_2), 
                     paste( input$numerator_3, '/', input$denominator_3) )
  
  dimensions = as.vector(unique(prop_data_input[, 1]))
  Results_df = data.frame()
  
  for ( dimension in dimensions )
  {
    for (metric in total_metrics)
    {
      if ( (strsplit(metric, '/')[[1]][1] == " " ) || (strsplit(metric, '/')[[1]][2] == " ") )
      { Results_df = Results_df }
      else {
        test_tbl = propInputTable(data = prop_data_input, dimension = dimension, metric = metric )
        prop_tbl = proportionTable(data = prop_data_input, dimension = dimension, metric = metric )
        prop_test_results = propTest( input_tbl = test_tbl, conf_level = 1 - input$general_test_alpha, 
                                      alternative_type = input$general_test_alternative )
        winner = DifferenceFunction(p_value = prop_test_results$p.value, prop_tbl)
        Results_df[dimension, metric] = winner
      }
    }
  }
  return(Results_df)
}

# 1.6.
overallPropStastics = function(prop_data_input)
{
  # CUSTOM METRIC
  total_metrics = c( input$general_prop_metrics[!input$general_prop_metrics == 'Custom Metric'] , 
                     paste( input$numerator_1, '/', input$denominator_1), 
                     paste( input$numerator_2, '/', input$denominator_2), 
                     paste( input$numerator_3, '/', input$denominator_3) )
  
  dimensions = as.vector(unique(prop_data_input[, 1]))
  Results_df = data.frame()
  
  for ( dimension in dimensions )
  {
    for (metric in total_metrics)
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

# PROP RUN ESTIMATION
#run_prop_test = eventReactive( input$general_prop_test_execute , { return( overallPropTesting( general_test_data()) ) } )
#statistic_prop_test = eventReactive( input$general_prop_test_execute, { return( overallPropStastics( general_test_data() ) )} )

# PROP REACTIVE OUTPUT
prop_test_result_reactive = eventReactive( input$general_prop_test_execute, {
  if (  input$general_test_options == "Proportion Based Test" )
  {
    column( width = 12,
            offset = 0,
            div( class="text-center", 
                 box( width = 12,
                      status = 'primary',
                      solidHeader = TRUE,
                      title = "Test Results and Statistics",
                      tabsetPanel(  
                        tabPanel( class="text-center", 'Test Results', br(),
                                  rHandsontableOutput('prop_test_results'), br()),
                        tabPanel( 'Test Statistics', br(),
                                  rHandsontableOutput('prop_test_statistics'),br()))))) }
  else if ( input$general_test_options == "Continuous Variable Test"  )
  {
    column( width = 12,
            offset = 0,
            div( class="text-center", 
                 box( width = 12,
                      status = 'primary',
                      solidHeader = TRUE,
                      title = "Test Results and Statistics",
                      tabsetPanel(  
                        tabPanel( class="text-center", 'Test Results', br(),
                                  rHandsontableOutput('continuous_test_results'), br()),
                        tabPanel( 'Test Statistics', br(),
                                  rHandsontableOutput('continuous_test_statistics'),br()))))) }
  else if ( input$general_test_options == "Cumulative Variable Test"  )
  {
    column( width = 12,
            offset = 0,
            div( class="text-center", 
                 box( width = 12,
                      status = 'primary',
                      solidHeader = TRUE,
                      title = "Test Results and Statistics",
                      tabsetPanel(  
                        tabPanel( class="text-center", 'Test Results', br(),
                                  rHandsontableOutput('cumulative_test_results'), br()),
                        tabPanel( 'Test Statistics', br(),
                                  rHandsontableOutput('cumulative_test_statistics'),br()))))) }
})


output$prop_test_overall_results = renderUI({ prop_test_result_reactive() })


# PROP TABULAR OUTPUT
output$prop_test_results = renderRHandsontable({ rhandsontable(overallPropTesting( general_test_data() ) , rowHeaderWidth=400)  })
output$prop_test_statistics = renderRHandsontable({ rhandsontable( overallPropStastics( general_test_data()), rowHeaderWidth=400 ) })


"************************************
TESTING - SELECTION
************************************"

# 1.6.
output$general_testing_parameter = renderUI({
  
  if ( is.null( general_test_data() ) ){ return() }  
  else {
    div( splitLayout( cellWidths = c("50%", "50%"),
                      cellArgs = list(),
                      div( 
                        uiOutput('general_testing_metrics')),
                      div( 
                        splitLayout( 
                          cellWidths = c("50%", "50%"),
                          cellArgs = list( style="padding:6px;" ),
                          div( br(),
                               numericInput( inputId = "general_test_alpha",
                                             label = "Select Significance Level:",
                                             value = .05, max = .5, min = .01, step = .01)),
                          div( br(),
                               radioButtons( inputId = "general_test_alternative",
                                             label = "Select Hypothesis:",
                                             choices = c( "Two Sided" = "two.sided",
                                                          "Greater" = "less",
                                                          "Less" = "greater"),
                                             inline = TRUE))))),
         uiOutput('general_custom_metrics'))
  }
})

"-----------------------------------------
CONTINUOUS DATA
-----------------------------------------"

## CONTINUOUS DISTRIBUTION TEST
overallContinuousStatistics = function(continuous_data_file)
{
  
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_data_file[,2]))
  for (dimension in dimensions)
  { 
    testing_data = continuous_data_file[continuous_data_file[,2]== dimension, ]  
    for (metric in input$general_continuous_metrics)
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


overallContinuousTesting = function(continuous_data_file)
{
  
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_data_file[,2]))
  for (dimension in dimensions)
  { 
    testing_data = continuous_data_file[continuous_data_file[,2]== dimension, ]  
    for (metric in input$general_continuous_metrics)
    {
      
      test =  cast(testing_data, Date ~ Group, value = metric )[, "Test"]
      control = cast(testing_data, Date ~ Group, value = metric )[, "Control"]
      test_results = wilcox.test(x = control, y = test, 
                                 alternative = input$general_test_alternative, 
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

output$continuous_test_results = renderRHandsontable({ rhandsontable(overallContinuousTesting( general_test_data()), 
                                                                     rowHeaderWidth=400, colHeaderWidth= 100 )  })
output$continuous_test_statistics = renderRHandsontable({ rhandsontable(overallContinuousStatistics( general_test_data()), 
                                                                        rowHeaderWidth=400, colHeaders = 100 )  })


## CUMULATIVE DISTRIBUTION TEST

overallCumulativeStatistics = function(continuous_data_file)
{
  
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_data_file[,2]))
  for (dimension in dimensions)
  { 
    testing_data = continuous_data_file[continuous_data_file[,2]== dimension, ]  
    for (metric in input$general_continuous_metrics)
    {
      
      test =  cast(testing_data, Date ~ Group, value = metric )[, "Test"]
      control = cast(testing_data, Date ~ Group, value = metric )[, "Control"]
      test_results = ks.test( x = control, y = test, 
                              #conf.int = 1 - input$general_test_alpha, 
                              alternative = input$general_test_alternative )
      
      Results_df[dimension, metric] = paste("P-Value:", round(test_results$p.value, 3), ",",
                                            "Statistic:", round(test_results$statistic[['D']],3), 
                                            collapse = " ")
    }
    
  }
  
  return(Results_df)
  
}

overallCumulativeTesting = function(continuous_data_file)
{
  
  Results_df = data.frame()
  dimensions = as.vector(unique(continuous_data_file[,2]))
  print(dimensions)
  for (dimension in dimensions)
  { 
    testing_data = continuous_data_file[continuous_data_file[,2]== dimension, ]  
    for (metric in input$general_continuous_metrics)
    {
      
      test =  cast(testing_data, Date ~ Group, value = metric )[, "Test"]
      control = cast(testing_data, Date ~ Group, value = metric )[, "Control"]
      test_results = ks.test(x = control, y = test,
                             conf.int = 1 - input$general_test_alpha,
                             alternative = input$general_test_alternative)
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

output$cumulative_test_results = renderRHandsontable({ rhandsontable( overallCumulativeTesting( general_test_data()), 
                                                                      rowHeaderWidth=400, colHeaderWidth= 100 )  })
output$cumulative_test_statistics = renderRHandsontable({ rhandsontable(overallCumulativeStatistics( general_test_data()), 
                                                                        rowHeaderWidth=400, colHeaderWidth = 100 )  })



continuousSampleCalc = function(cont_series, mean_difference, power, significance, ratio )
{
  z_power = round(qnorm( power ),2)
  z_siglevel = round(abs(qnorm(significance/ 2)), 2)
  variance = var(cont_series)
  sample_ratio = ratioCalculator(ratio)
  sample_size = variance*(z_power + z_siglevel)**2/(mean_difference**2)
  return( ceiling(sample_size) )
}

ratioCalculator = function(split_ratio){ return(round(( (split_ratio*10)/(1-split_ratio)/10 ),1)) }

continuousSampleCalc(c(31,24,34,33,32,33, 43,12,34,45,23,23,12,12,43,34), 10, .8, .05, .5)