

#####################################
#      CAUSAL IMPACT FUNCTIONS      #
#           JUNE 18th 2018          #
#####################################

# BUILDING BASE CAUSAL IMPACT
library(zoo)
library(dplyr)
library(reshape)
library(lubridate)
library(CausalImpact)

# SETTING THE WORKING DIRECTORY
setwd('~/Desktop/Projects/Products/Eureka/')

# FUNCTION TO READ AND CLEAN DATA
test_data = read.csv('CausalImpactTest_Demo.csv')

# FUNCTION FOR 
testing_periods = function(datafile){ return( c(min(ymd(datafile$Date)),max(ymd(datafile$Date)))) }

# PROCESSING DATA
processing_data = function( datafile, target_variable )
{
    # CHANGE THE DATE AS NEEDED
    datafile$Date = ymd(datafile$Date)
    if ( 'Group' %in% names(datafile) )
    {  
       test_data = cast(datafile, Date ~ Group, value = target_variable, fun.aggregate = sum)   
       return( zoo( test_data[, c('Test', 'Control')], test_data$Date) )
    } else { 
      # Returning Covariates
      covariates = names(test_data)[!names(test_data) %in% c(target_variable, 'Date')]
      return( zoo( test_data[, c(target_variable, covariates )], test_data$Date  )  )
    }
}

# GENERIC MODEL
generic_model = function( model_data_input, full_dates, test_date )
{
   # BUILDING THE CAUSAL IMPACT MODEL
   impact_model = CausalImpact( model_data_input, pre.period = c( full_dates[1], ymd(test_date)-1),
                                post.period = c(ymd(test_date), full_dates[2]),
                                model.args = list(niter = 1000, nseasons = 52, season.duration=7 ))
   return(impact_model)
}

custom_model = function(model_data_input, full_dates, test_date, niter ,seasons, season_duration)
{
  # BUILDING THE CAUSAL IMPACT MODEL
  impact_model = CausalImpact( model_data_input, pre.period = c( full_dates[1], ymd(test_date)-1),
                               post.period = c(ymd(test_date), full_dates[2]),
                               model.args = list(niter = niter, nseasons = seasons, season.duration=season_duration ))
  return(impact_model)
}


# EXECUTING THE FUNCTIONS
dates = testing_periods(test_data)
model_data = processing_data(datafile = test_data, target_variable = "Sessions")
generic_model = generic_model(model_data, dates, "2017-07-01")

typeof(generic_model)

generic_model$summary
generic_model$series
generic_model$report
generic_model$model

summary(generic_model)

"********************************************
            RESULTS OUTPUT
********************************************"

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
  print(as.data.frame(all_data))
}
