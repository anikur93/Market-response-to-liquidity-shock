
## 50000 points are labelled into five clusters  using classification model 
## Features for classifer model:
##     -- Time 40-50 Mean and spread
##     -- Min,MAx,Stdev of Mean and Spread 
##     -- Events/Quotes rate
##     -- Trade volume and Trade amount before shock 
##     -- Buyer Initiated/Seller Intiatied 

## Out of 37500 points taken as training data, to train cluater model to lable
## 12750 points are taken as test data to check the accuracy of classifier model 
## We used raodm forest and SVM model classifiers 
## Plots are also attached. 


Mean = read.csv("mean50.csv")
colnames(Mean)= c("id",1:100, "cluster")
Spread = read.csv("speard50.csv")
colnames(Spread)= c("id",1:100, "cluster")
data_all = read.csv("data_subset_50k.csv")

head(Mean)
head(Spread)

Mean$min = apply(Mean[2:50],1,function(x)(min(x)))
Mean$max = apply(Mean[2:50],1,function(x)(max(x)))
Mean$diff = Mean$max-Mean$min
Mean$sd = apply(Mean[2:50],1,function(x)(sd(x)))
Mean$intiator = data_all$intiator
Mean$trade_vwap = data_all$trade_vwap
Mean$trade_volume = data_all$trade_volume 
#Mean$eventrate = apply(data_all,1, function(x)(50/(data_all$time50-data_all$time1)))

Spread$min = apply(Spread[2:50],1,function(x)(min(x)))
Spread$max = apply(Spread[2:50],1,function(x)(max(x)))
Spread$diff = Spread$max-Spread$min
Spread$sd = apply(Spread[2:50],1,function(x)(sd(x)))
Spread1=Spread
colnames(Spread1)= c("id",500:599, "cluster")

Model_data = cbind(Mean$min,Mean$max,Mean$diff,Mean$sd,Mean$intiator,
                   Mean$trade_vwap,Mean$trade_volume,Spread$min,Spread$max,Spread$diff,Spread$sd,Mean$cluster,
                   Mean$`40`,Mean$`41`,Mean$`42`,Mean$`43`,Mean$`44`,Mean$`45`,Mean$`46`,Mean$`47`,Mean$`48`,Mean$`49`,
                   Spread$`46`, Spread$`45`, Spread$`44`, Spread$`45`, Spread$`46`, Spread$`47`, Spread$`48`, Spread$`49`)

######### Train Data ###########
set.seed(100)
coin = sample(2, nrow(Model_data),replace =TRUE, prob = c(0.7,0.3))
Model_train=Model_data[coin==1,]
Model_test=Model_data[coin==2,]
head(Model_train,1)

require(randomForest)

#form <- as.formula ( paste( c( names(Model_train)[11] , paste(names(Model_train)[1:10],collapse = "+") ),
#                            collapse="~" ) )

#model<-randomForest(form,data=Model_train,mtry=4)

randomModel = randomForest(Model_train[,c(1:10,12:29)], Model_train[,11], mtry = 6, ntree =500 )

summary(randomModel)
model
randomModel$mse
plot(randomModel)


importance(randomModel)
Cluster<-(predict(randomModel,Model_test[,1:10]))

Cluster_predict = (as.matrix(Cluster))
dim(Model_test)
Model_test_X = cbind(Model_test, Cluster_predict)
plot(Model_test_X[,12], Model_test_X[,11])

########## SVM ##############
library("e1071")
colnames(Model_train) = c(1:29)
colnames(Model_test) = c(1:29)
dim(Model_test)
Model_train[,11]
x = subset(Model_train, select = c(1:10,12:29))
y = Model_train[,11]
svm_model <- svm(y ~ x, data=Model_train)
summary(svm_model)

x_test = subset(Model_test, select = c(1:10,12:29))
y_test = Model_test[,11]
pred =predict(svm_model,x_test)
length(pred)
table(pred,y_test)
plot(pred, y)
