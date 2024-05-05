rm(list=ls())
set.seed(100)

#Read data
data=read.csv("M:/OMSA/ISYE6501/HW1/credit_card_data.csv")

#Select Library
library(kknn)

#Select random set of data points to create training,validation and testing dataset
random_rows=sample(1:nrow(data),as.integer(0.7*nrow(data)))
training_data=data[random_rows,]
rest_data=data[-random_rows,]
random_rows_2=sample(1:nrow(rest_data),as.integer(0.5*nrow(rest_data)))
validation_data=rest_data[random_rows_2,]
testing_data=rest_data[-random_rows_2,]

#finding best fit k using knn method

predicted_value_training=rep(0,nrow(training_data))

#Define a function to try multiple K Values
k_value_best_fit=function(X){
  
  #Define a loop to create training data and testing data for each datapoint
  for (i in 1:nrow(training_data))
  {
    training_model=kknn(R1~.,training_data[-i,],training_data[i,],k=X,distance=2,kernel="optimal",scale=TRUE)
    predicted_value_training[i]=round(fitted.values(training_model))
  }
  
  #Accuracy of then model for k value X
  sum(predicted_value_training==training_data[,11])/nrow(training_data)
}

#define range of k values to test
range=rep(0,20)
for(X in 1:20)
{
  range[X]=k_value_best_fit(X)
}

#Save Accuracy model as a matrix
knn_training_model_accuracy=as.matrix(range*100)
knn_training_model_accuracy  #prints accuracy by k value as rownumber

#Find Max Accuracy Model
max(knn_training_model_accuracy)




#Validate using validation_data
predicted_value_validation=rep(0,nrow(validation_data))

#Run model
  for (i in 1:nrow(validation_data))
  {
    validation_model=kknn(R1~.,validation_data[-i,],validation_data[i,],k=12,distance=2,kernel="optimal",scale=TRUE)
    predicted_value_validation[i]=round(fitted.values(validation_model))
  }
  
  #Accuracy of then model in validation
  validation_model_accuracy=sum(predicted_value_validation==validation_data[,11])/nrow(validation_data)

validation_model_accuracy  #prints accuracy for validation model


#Validate using test_data
predicted_value_test=rep(0,nrow(testing_data))

#Run model
for (i in 1:nrow(testing_data))
{
  test_model=kknn(R1~.,testing_data[-i,],testing_data[i,],k=12,distance=2,kernel="optimal",scale=TRUE)
  predicted_value_test[i]=round(fitted.values(test_model))
}

#Accuracy of then model in validation
test_model_accuracy=sum(predicted_value_test==testing_data[,11])/nrow(testing_data)

test_model_accuracy  #prints accuracy for validation model
validation_model_accuracy
max(knn_training_model_accuracy)

