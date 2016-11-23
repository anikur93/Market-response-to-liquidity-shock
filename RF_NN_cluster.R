data1k<-read.csv("data_subset_50k.csv")
################# feature
data3k<-data1k[, grepl("ask",names(data1k))]

for(i in 1:nrow(data1k))
{
  data1k$mean_ask[i]<-mean(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$sd_ask[i]<-sd(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$median_ask[i]<-median(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$min_ask[i]<-min(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$max_ask[i]<-max(as.numeric((data1k[i, grepl("ask",names(data1k))])))
}

for(i in 1:nrow(data1k))
{
  data1k$mean_bid[i]<-mean(as.numeric((data1k[i, grepl("bid",names(data1k))])))
}
#standard dev
for(i in 1:nrow(data1k))
{
  data1k$sd_ask[i]<-sd(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$sd_bid[i]<-sd(as.numeric((data1k[i, grepl("bid",names(data1k))])))
}
#median
for(i in 1:nrow(data1k))
{
  data1k$median_ask[i]<-median(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$median_bid[i]<-median(as.numeric((data1k[i, grepl("bid",names(data1k))])))
}
#min & max
for(i in 1:nrow(data1k))
{
  data1k$min_ask[i]<-min(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$min_bid[i]<-min(as.numeric((data1k[i, grepl("bid",names(data1k))])))
  data1k$max_ask[i]<-max(as.numeric((data1k[i, grepl("ask",names(data1k))])))
  data1k$max_bid[i]<-max(as.numeric((data1k[i, grepl("bid",names(data1k))])))
}
#################
#model check
######################

data3k<-data1k
data1k<-subset(data3k, cluster== 5)
data3k<-data1k[, grepl("ask",names(data1k))]
data3k$cluster<-data1k$cluster

set.seed(1101)
samp<-sample(dim(data3k)[1], round(dim(data3k)[1]/1.3))
dtrain1<-data3k[samp,]
dtest1<-data3k[-samp,]

require(randomForest)
require(nnet)

form <- as.formula ( paste( c( names(dtrain1)[52] , paste(names(dtrain1)[c(1:51,101:106)],collapse = "+") ),
                            collapse="~" ) )

model<-randomForest(form,data=dtrain1,mtry=5)

pred.ask52<-predict(model,(dtest1)[c(1:51,101:106)])

plot(dtest1$ask52,col='blue')
lines(pred.ask52,col='red',types='l')
rms52<-(mean((pred.ask52-dtest1$ask52)^2))^.5

#################################################
#NeuralNet
#################################################

require(nnet)
dim(dtrain1)
dim(dtest1)
model<-nnet(form,dtrain1,size=5,rang = 0.1,
            decay =.1, maxit = 2000,linout=1)
model
pred.ask52<-predict(model,(dtest1)[c(2:52,101:107)])
rms52<-(mean((pred.ask52-dtest1$ask52)^2))^.5
