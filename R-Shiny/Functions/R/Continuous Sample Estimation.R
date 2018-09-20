"==========================================================

         CONTINUOUS LEVEL SAMPLE SIZE ESTIMATION

=========================================================="

# RATIO CALCULATOR
ratioCalculator = function(split_ratio){ return(round(( (split_ratio*10)/(1-split_ratio)/10 ),1)) }

# SAMPLE CALCULATOR
continuousSampleCalc = function(cont_series, mean_difference, power, significance, ratio )
{
  z_power = round(qnorm( power ),2)
  z_siglevel = round(abs(qnorm(significance/ 2)), 2)
  variance = var(cont_series)
  sample_ratio = ratioCalculator(ratio)
  sample_size = variance*(z_power + z_siglevel)**2/(mean_difference**2)
  return( ceiling(sample_size) )
}

# CONVERT DIFFENCES
convertMeanDifferences = function(series, difference) { differences =  mean(series)*(1.00+ difference) - mean(series) }


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
          all_results[[ paste(group, difference) ]] = c(group, metric ,100*difference, est_sample)
        }
        
    } 
  }
  
  final = as.data.frame(do.call(rbind, all_results))
  colnames(final) = c('Group', 'Metric','Effect Size', 'Sample')
  print(final)
  
}

sample_data = read.csv('~/Desktop/Eureka Data Prep/Continuos_SampleDemo.csv')

contiSampleCompute(sample_data)