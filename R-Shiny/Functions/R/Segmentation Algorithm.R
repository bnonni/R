
"******************************************
     EUREKA SEGMENTATION ALGORITHM

******************************************"

library(slqdf)
library(dplyr)
library(dbplyr)
library(lpSolveAPI)

library(ggplot2)

# SETTING THE WORKING DIRECTORY
setwd("/Users/sifael.ndandala/Desktop/Projects/Products/Eureka/")

# READING THE DATA
data = read.csv("USCards_GeoPerf.csv")
str(data)
head(data)

# CONVERTING METRICS
data$Week = as.Date(data$Week, format="%m/%d/%y")
data$CTR = as.numeric(gsub("%", "", data$CTR))
data$Avg..CPM = as.numeric(data$Avg..CPM)
data$Cost = as.numeric(gsub(",","", data$Cost))


# RETURNING THE TRAIN SET
train_df = sqldf("SELECT [City], SUM([Impressions]) AS Impressions,
                  SUM([Clicks]) AS Clicks, SUM([Cost]) AS Cost
                  FROM data
                  WHERE Week < '2016-04-11'
                  GROUP BY City
                  ORDER BY City")

# DEVELOPING THE MODEL
len = nrow(train_df)
objective_function = c(as.numeric(train_df$Clicks), -as.numeric(train_df$Clicks))
constraint_1 = c(as.numeric(train_df$Impressions), -as.numeric(train_df$Impressions))
constraint_2 = c(as.numeric(train_df$Cost), -as.numeric(train_df$Cost))

lprec = make.lp(nrow=0, ncol=len*2)
set.objfn(lprec, objective_function)
set.type(lprec = lprec, seq(1, len*2), "binary")
add.constraint(lprec, objective_function, ">=", 0)
add.constraint(lprec, constraint_1, ">=", -100000)
add.constraint(lprec, constraint_1, "<=", 100000)
add.constraint(lprec, constraint_2, ">=", -100000)
add.constraint(lprec, constraint_2, "<=", 100000)

for (i in 1:len){ add.constraint(lprec, xt=c(1,1), type="=", rhs=1, indices = c(i, i+len) ) }

# SOLVING THE PROBLEM
solve(lprec)

AB = head(get.variables(lprec), len)
train_df = cbind(train_df, AB)
train_df$Segment = if_else(train_df$AB == 0, "Control", "Test")

# RETURNING THE RESULTS
segments = train_df[, c("City","Segment")]
final = merge(x = data, y = segments, by = "City", all.x = TRUE)

# PLOTTING THE RESULTS
impressions_data = final %>% group_by(Week, Segment) %>% summarise(Impressions = sum(Impressions),
                                                                   Clicks = sum(Clicks),
                                                                   Cost = sum(Cost))

ggplot(data = impressions_data, aes(x = Week, y = Impressions, group=Segment, color=Segment)) + geom_line()
ggplot(data = impressions_data, aes(x = Week, y = Clicks, group=Segment, color=Segment)) + geom_line()
ggplot(data = impressions_data, aes(x = Week, y = Cost, group=Segment, color=Segment)) + geom_line()
