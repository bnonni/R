#!/usr/bin/env R
library(kknn)
library(class)
library(cvTools)

set.seed(200)
folds <- c()
ks <- c()
fold1 <- c()
fold2 <- c()
fold3 <- c()
fold4 <- c()
fold5 <- c()
fold6 <- c()
fold7 <- c()
fold8 <- c()
fold9 <- c()
fold10 <- c()

ladata <- as.data.frame(read.table("credit_card_data-headers.txt",header = T))
head(ladata)

for (x in 1:10){
  elsample = sample(1:nrow(ladata), size = round(nrow(ladata)*.6), replace = FALSE)
  
  train = ladata[elsample,]
  test.vali.break = ladata[-elsample,]
  test.frame = sample(1:nrow(test.vali.break), size = round(nrow(test.vali.break)*.5), replace = FALSE)
  
  vali = test.vali.break[test.frame,]
  test = test.vali.break[-test.frame,]
  for(fold in 1:10){ 
    
    knn <- train.kknn(R1 ~ ., data = train, x, kernel = c("optimal","rectangular", "inv", "gaussian", "triangular"), scale = TRUE, fold)
    
    predic<-predict(knn, test)
    predic
    predbin <- round(predic)
    pred_accuracy<-table(predbin,test$R1)
    pred_accuracy
    
    if(fold == 1) fold1[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 2) fold2[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 3) fold3[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 4) fold4[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 5) fold5[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 6) fold6[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 7) fold7[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 8) fold8[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 9) fold9[x] = sum(predbin==test$R1)/length(test$R1)
    else if(fold == 10) fold10[x] = sum(predbin==test$R1)/length(test$R1)
    else print("error")
  }
  
  
}
fold1
fold2
fold3
fold4
fold5
fold6
fold7
fold8
fold9
fold10