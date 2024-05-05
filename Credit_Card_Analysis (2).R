rm(list=ls())
set.seed(100)

#Read data
data=read.csv("M:/OMSA/ISYE6501/HW1/credit_card_data.csv")

#Select Library
library(kknn)

#Checking if data is balanced to select appropriate sorting methodology
hist(data$R1,col="coral")
prop.table(table(data$R1))


#Select random set of data points to create training and validation dataset
random_rows=sample(1:nrow(data),as.integer(0.7*nrow(data)))
tv_data=data[random_rows,]
test_data=data[-random_rows,]

#train model
train.kknn(as.factor(R1)~.,tv_data,kmax=100,scale=TRUE)
 
#Checking model accuracy on train data using K=12

predicted_value_train=rep(0,nrow(tv_data))

for (i in 1:nrow(tv_data))
  {
  model_train=kknn(R1~.,tv_data[-i,],tv_data[i,],k=12,distance=2,kernel="optimal",scale=TRUE)
predicted_value_train[i]=round(fitted.values(model_train))
  }

#Accuracy of the model for k=12
train_accuracy=sum(predicted_value_train==tv_data[,11])/nrow(tv_data)


#Testing Data Accuracy on testing model 
predicted_value_test=rep(0,nrow(test_data))

for (i in 1:nrow(test_data))
{
  model_test=kknn(R1~.,test_data[-i,],test_data[i,],k=12,distance=2,kernel="optimal",scale=TRUE)
  predicted_value_test[i]=round(fitted.values(model_test))
}

#Accuracy of the model for k=12
test_accuracy=sum(predicted_value_test==test_data[,11])/nrow(test_data)


train_accuracy
test_accuracy
