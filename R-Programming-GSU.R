#Vectors
a <- c(1, 2, 5, 3, 6, -2, 4)
b <- c("one", "two", "three")
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)

#Indexing data from vectors
a[3]
a[c(1, 3, 5)]
a[2:6]

#Creating a matrix
y <- matrix(1:20, nrow=5, ncol=4)

y[3,]
y[,3]
y[1, c(2,3)]

###DataFrame creation
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
class(patientdata)
patientdata

##Indexing a DataFrame
patientdata[2,3:4]
patientdata$age

patientdata[patientdata$age >= 30,] #Get list of patients with age greater than 30
patientdata[patientdata$age >= 30,c("diabetes","status")] # Find diabetes type and health status of patients older than 30 years

