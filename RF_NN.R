data1k<-read.csv("data1k.csv")
data_ask<-data1k[, grepl("ask",names(data1k))]
data_bid<-data1k[, grepl("bid",names(data1k))]
spread<-(data_ask[1:nrow(data_ask),]-data_bid[1:nrow(data_bid),])


#################
#feature creation
##################
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

for(i in 1:nrow(data1k))
{
  data1k$min_spread[i]<-min(as.numeric(spread[i,]))
  data1k$max_spread[i]<-max(as.numeric(spread[i,]))
  data1k$median_spread[i]<-median(as.numeric(spread[i,]))
  data1k$sd_spread[i] <- sd(as.numeric(spread[i,]))
}

#################
#model check
##################
data3k<-cbind(data3k,data1k[, 308:321])
names(data3k)
set.seed(1101)
samp<-sample(dim(data3k)[1], round(dim(data3k)[1]/1.3))
dtrain1<-data3k[samp,]
dtest1<-data3k[-samp,]

require(randomForest)
form <- as.formula ( paste( c( names(dtrain1)[52] , paste(names(dtrain1)[c(1:51,101:114)],collapse = "+") ),
                            collapse="~" ) )
model<-randomForest(form,data=dtrain1,mtry=5)
varImpPlot(model)
pred.ask52<-predict(model,(dtest1)[c(1:51,101:114)])

plot(dtest1$ask52,col='blue')
lines(pred.ask52,col='red',types='l')
rms52<-(mean((pred.ask52-dtest1$ask52)^2))^.5
rms52
#################################################
#NeuralNet
#####################################

require(nnet)
dim(dtrain1)
dim(dtest1)
model<-nnet(form,dtrain1,size=5,rang = 0.1,
                        decay =.1, maxit = 2000,linout=1)

pred.ask52<-predict(model,(dtest1)[c(1:52,101:114)])
nnrms52<-(mean((pred.ask52-dtest1$ask52)^2))^.5
nnrms52
