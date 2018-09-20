

library(reshape)

# SET WD
setwd("/Users/sifael.ndandala/Desktop/Projects/Products/Eureka/")
datafile = read.csv('Sample_file.csv')

ratioCalculator = function(split_ratio){ return(round(( (split_ratio*10)/(1-split_ratio)/10 ),1)) }

# CALCULATOR
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
  return( ceiling(sample_size))
  
}


propSampleSplitResults = function(datafile)
{
  # GROUPS 
  groups = as.vector(unique(datafile[, 1]))
  lift_levels = c(1 , 2 , 3, 4, 5, 10, 15)
  split_ratio = c(.5, .6, .7, .8, .9)
  all_results = list()
  
  for (group in groups)
  {
      #print(group) 
      proportion = datafile[datafile[, 1] == group, 2:length(datafile)]
      for (lift_level in lift_levels)
      {
         for (ratio in split_ratio )
         {
           # print(paste(group, lift_level, ratio))
           estimate = max(100 + mapply( playGroundSampleCalculator, proportion, lift_level/100, ratio, .8, .05 ))
           all_results[[ paste(group, lift_level, ratio)]] = c(group, lift_level, paste(ratio*100, (1-ratio)*100, sep = "/") , estimate)
         }
         
      }
  }
  #print(all_results)
  test = as.data.frame(do.call(rbind, all_results))
  colnames(test) = c('Group', 'Lift', 'Split' , 'Samples')
  row.names(test) = c()
  return(test)
}

data = propSampleSplitResults(datafile)

one_file = data[data[, 1] == "Brand_Group_One", ]


cast(one_file, Split ~ Lift, value = "Samples")