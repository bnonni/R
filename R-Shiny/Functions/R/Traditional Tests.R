"********************************************

       TRADITIONAL TESTING FUNCTIONS
           Date: JUNE 25th 2018

Objective: Traditional Testing Functions
********************************************"

library(fifer)


# WORKING DIRECTORY
setwd('~/Desktop/Products/Eureka/Datasets/')

# PROPORTION DATA
proportion_file = read.csv('Sample_TraditionalTest_demo.csv')
proportion_file

metric = "Clicks/Impressions"

# FUNCTION TO PROCESS PROPORTION
propInputTable = function(data, dimension, metric)
{
  input_tbl = data[ data[, 1] == dimension, 
                    c(strsplit(metric, '/')[[1]][1], strsplit(metric, '/')[[1]][2])]
  row.names(input_tbl) = data[ , 2][data[ , 1] == dimension]
  return(input_tbl)
}


# Proportion Testing Table
propTestingTable = function(data, dimension, metric)
{
  input_tbl = data[data[, 1]== dimension, c(strsplit(metric, '/')[[1]][1], strsplit(metric, '/')[[1]][2])]
  row.names(input_tbl) = data[ , 2][data[ , 1] == dimension]
  
  proportion = input_tbl[ , 1]/input_tbl[ , 2]
  names(proportion) = data[ , 2][data[ , 1] == dimension]
  return(proportion)
}


# RUNNING THE TEST
propTest = function(input_tbl, conf_level, alternative_type)
{
   general_prop_test = prop.test( x = input_tbl[,1], 
                            n = input_tbl[,2], 
                            conf.level = conf_level, 
                            alternative = alternative_type)
   return(general_prop_test)  
}


# WINNER FUNCTION
WinnerFunction = function()
{}



dimensions = as.vector(unique(proportion_file$AdGroup))
for ( dim in dimensions[1])
{
   input_tbl = propInputTable(proportion_file, dim, metric)
   prop_tbl = propTestingTable(proportion_file, dim, metric)
   prop_test_results = propPValue(input_tbl)
   print(round(prop_test_results$p.value, 5))
}



# CONTINUOUS DATA
continuous_file = read.csv('')
continuous_file

# CUMULATIVE DATA
cumulative_file = read.csv('')
cumulative_file




