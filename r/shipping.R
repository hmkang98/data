library(caret)
library(dplyr)
library(readr)
setwd("c:/data/r")
shippingdata <- read_csv("shipping_train.csv")
shippingdata <- data.frame(shippingdata)
shippingdata <- shippingdata[,-1]
head(shippingdata)
str(shippingdata)
shippingdata <- shippingdata %>% mutate(Warehouse_block = factor(Warehouse_block), Mode_of_Shipment = factor(Mode_of_Shipment),
                          Customer_care_calls = factor(Customer_care_calls), Customer_rating = factor(Customer_rating),
                          Prior_purchases = factor(Prior_purchases),Product_importance = factor(Product_importance),
                          Gender = factor(Gender), Reached.on.Time_Y.N = factor(Reached.on.Time_Y.N))
sample <- createDataPartition(shippingdata$Reached.on.Time_Y.N,p=0.7,list=F)
training <- shippingdata[sample,]
testing <- shippingdata[-sample,]
ctrl <- trainControl(method="adaptive_cv", repeats=5)
model <- train(Reached.on.Time_Y.N~.,data=training,method="knn",trControl=ctrl)
pred <- predict(model,testing)
head(pred)
prop.table(table(pred,testing$Reached.on.Time_Y.N))
test_label <- as.numeric(testing$Reached.on.Time_Y.N) - 1

pr <- prediction(pred,as.numeric(testing$Reached.on.Time_Y.N))
prof <- performance(pred,measure="tpr",measure.x="fpr")
head(testing$Reached.on.Time_Y.N)

head(pred)
confusionMatrix(pred,testing$Reached.on.Time_Y.N)
