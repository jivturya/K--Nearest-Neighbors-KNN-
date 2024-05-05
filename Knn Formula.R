  rm(list=ls())
  set.seed(100)
  #Read data
  data=read.csv("M:/OMSA/ISYE6501/HW1/credit_card_data.csv")
  
  #Select Library
  library(kknn)
  
  #defining predicted variable for storage
    predicted_value=rep(0,nrow(data))
    
    #Define a function to try multiple K Values
k_value_best_fit=function(X){
  
  #Define a loop to create training data and testing data for each datapoint
    for (i in 1:nrow(data))
    {
      model=kknn(R1~.,data[-i,],data[i,],k=X,distance=2,kernel="optimal",scale=TRUE)
      predicted_value[i]=round(fitted.values(model))
    }

#Accuracy of the model for k value X
    sum(predicted_value==data[,11])/nrow(data)
}

#define range of k values to test
range=rep(0,20)
for(X in 1:20)
  {
  range[X]=k_value_best_fit(X)
}

#Save Accuracy model as a matrix
knn_model_accuracy=as.matrix(range*100)
#knn_model_accuracy  #prints accuracy by k value as rownumber

#Find Max Accuracy Model
max(knn_model_accuracy)





  
