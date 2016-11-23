data1k<-read.csv("data1k.csv")
dtrain3<-as.data.frame(data1k[, grepl("ask",names(data1k))])
names(dtrain3)
dtrain4<-dtrain3[,1:50]
diff.val<-0
dim(dtrain4)
for (i in 1:nrow(dtrain4))
{
ar.mod<-tryCatch(ar(as.numeric(dtrain4[i,]), method = "burg"), error=function(e) NULL)
if(!is.null(ar.mod)){
pred.test<-predict(ar.mod,as.numeric(dtrain4[i,]), n.ahead = 50, se.fit = TRUE)
pred.val <-as.numeric(pred.test$pred[1:50])
#diff<- mean(as.numeric(dtrain3[i,51:100])-pred.val)
diff<- (mean((as.numeric(dtrain3[i,51:100])-pred.val)^2))^0.5
diff.val[i]<-diff
}
}
#############################################
pred1<-dtrain4[100,]
for (i in 1:50)
{
pred.test<-predict(ar.mod,as.numeric(pred1), n.ahead = 1, se.fit = TRUE)
pred1[i+50]<-pred.test$pred
}
plot.ts(as.numeric(dtrain4[100,51:100]),col='blue')

lines(as.numeric(pred1),col='red')
####################################################


sum(diff.val)/length(diff.val)

#.187+.9555+.6558+

length(diff.val)
pred.test$pred
plot.ts(as.numeric(dtrain3[i,51:100]),ylim=c(691,693),col='blue')

lines(as.numeric(pred.test$pred),col='red')

colsum(diff.val)

diff.val1<-as.numeric(na.omit(diff.val))
plot(diff.val1)

rowidd<-diff.val>10

rowidd()

sum(as.numeric(na.omit(diff.val)))/48685

dtrain3[1,51:100]
acf(as.numeric(dtrain3[301,]))

plot.ts(as.numeric(dtrain3[301,1:100]))

lines(as.numeric(dtrain3[301,1:100]),type='l')

summary(diff.val)
plot(diff.val)

summary(diff.val<2)


differ<-abs(data1k$ask53-data1k$ask54)

summary(differ)

###################s

pred_ask<-pred.test$pred
pred_bid<-pred.test$pred


plot(pred_ask)
plot(pred_bid)

spread<-(pred_ask-pred_bid)

mean.310<-(pred_ask+pred_bid)/2

plot(spread)
plot(mean.310)

dtrain4.bid<-dtrain3[301,51:100]
dtrain4.ask<-dtrain3[301,51:100]



par(new=T)
plot.ts(as.numeric(dtrain4.bid))

tryCatch(solve(x), error=function(e) NULL)
?ar()
dtrain4[42,]
dim(dtrain4)
names(dtrain3)[c(51:100)]

ar.modd<-ar(dtrain4[1,], method = "burg")
pred.testt<-predict(ar.modd,as.numeric(dtrain4[1,]), n.ahead = 50, se.fit = TRUE)

pred.val <-pred.test$pred[1:50]
diff<- mean(dtrain2[,51:100]-pred.val)

a<-c(1,2,3)
append(a,4)
