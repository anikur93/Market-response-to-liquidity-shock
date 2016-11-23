#reading data
data1k <- read.csv('data1k.csv')
data2k <- read.csv('data2k.csv')
data_subset_50k <- read.csv('data_subset_50k')
#normalise
normalize <- function(x){
   
   return ((x - min(x)) / (max(x) - min(x)))
   }
#ask and bid prices
data_ask<-data1k[, grepl("ask",names(data1k))]
data_bid<-data1k[, grepl("bid",names(data1k))]
data_com<-cbind(data_ask,data_bid)
#normalise data
data_ask_n = as.data.frame(lapply(data_ask, normalize))
data_bid_n = as.data.frame(lapply(data_bid, normalize))
head(data_ask_n)
data_n = cbind(data_ask_n, data_bid_n)
head(data_n)
dim(data_n)

#means 
data_average<-(data_ask[1:nrow(data_bid),]+data_bid[1:nrow(data_bid),])/2


#kmeans clust trail run
km = kmeans(data_n, 4, iter.max = 10, nstart = 20)
km
km$cluster
data_n$cluster = km$cluster
data_com$cluster = km$cluster
index = c(1:100)
plot(index,data_ask[5,], type = 'o')
data_1 = subset(data_com, cluster==1)
data_2 = subset(data_com, cluster==2)
data_3 = subset(data_com, cluster ==3)
data_4 = subset(data_com, cluster ==4)

#kmeans clust for data - timestamps 1:50
data_average2 = data_average[,1:50]
#data_average2_n = as.data.frame(apply(average2,1, normalize))
data_average2_n = as.data.frame(apply(data_average2, 1, function(x)(x-min(x))/(max(x)-min(x))))
data_average2_n = as.data.frame(t(data_average2_n))
data_trans_5 = as.data.frame(t(data_average2_n))
#k=5
km2 = kmeans(t(data_average2_n), 5, iter.max = 10, nstart = 20)
km2
dim(as.aaray(km2$cluster))
data_average2$cluster5 = (km2$cluster)
data_trans_5$cluster5 = km2$cluster
##subsetting each cluster
data5_1 = subset(data_average2, cluster5 == 1)
data5_2 = subset(data_average2, cluster5 == 2)
data5_3 = subset(data_average2, cluster5 == 3)
data5_4 = subset(data_average2, cluster5 == 4)
data5_5 = subset(data_average2, cluster5 == 5)
#plots
index2 = c(1:50)
plot(index2, data5_1[1,1:50], type = 'o')
plot(index2, data5_1[2,1:50], type = 'o')
plot(index2, data5_1[3,1:50], type = 'o')
plot(index2, data5_2[1,1:50], type = 'o')
plot(index2, data5_2[2,1:50], type = 'o')
plot(index2, data5_2[4,1:50], type = 'o')
plot(index2, data5_3[1,1:50], type = 'o')
plot(index2, data5_3[2,1:50], type = 'o')
plot(index2, data5_3[4,1:50], type = 'o')
plot(index2, data5_3[1,1:50], type = 'o')
plot(index2, data5_3[2,1:50], type = 'o')
plot(index2, data5_3[4,1:50], type = 'o')
plot(index2, data5_4[1,1:50], type = 'o')
plot(index2, data5_4[2,1:50], type = 'o')
plot(index2, data5_4[4,1:50], type = 'o')
plot(index2, data5_5[1,1:50], type = 'o')
plot(index2, data5_5[2,1:50], type = 'o')
plot(index2, data5_5[4,1:50], type = 'o')
##changing values of k
for (i in 1:15) {
  kms = kmeans(t(data_average2_n), i, iter.max = 10, nstart = 20)
  print (kms$betweenss/kms$totss)
}

####toy test for normalization###Rough
a1 = c(2,4,6)
a2 = c(3,8,1)
mat = cbind(a1,a2)
mat2 = as.data.frame(apply(mat,1, normalize))
average2_n[2,1:50]



#kmeans clust by 51:100,for dataset of 50k rows
data2k = data_subset_50k
data_ask2<-data2k[, grepl("ask",names(data2k))]
data_bid2<-data2k[, grepl("bid",names(data2k))]
data_average2<-(data_ask2[1:nrow(data_bid2),]+data_bid2[1:nrow(data_bid2),])/2
data_average3 = data_average2[,51:100]
#data_average2_n = as.data.frame(apply(average2,1, normalize))
data_average3_n = as.data.frame(apply(data_average3, 1, 
                                      function(x)(x-min(x))/(max(x)-min(x)+0.05)))
data_average3_n = as.data.frame(t(data_average3_n))
data_trans_6 = as.data.frame(t(data_average3_n))
#k=5
km3 = kmeans(data_average3_n, 5, iter.max = 10, nstart = 20)
km3
#dim(as.aaray(km2$cluster))
data_average2$cluster5 = (km3$cluster)
data_trans_5$cluster5 = km3$cluster
data6_1 = subset(data_average2, cluster5 == 1)
data6_2 = subset(data_average2, cluster5 == 2)
data6_3 = subset(data_average2, cluster5 == 3)
data6_4 = subset(data_average2, cluster5 == 4)
data6_5 = subset(data_average2, cluster5 == 5)
write.csv(data6_1, 'c:/data50_1.csv')
index2 = c(51:100)
#plots
plot(index2, data6_1[1,51:100], type = 'o')
plot(index2, data6_1[2,51:100], type = 'o')
plot(index2, data6_1[5,51:100], type = 'o')

plot(index2, data6_2[1,51:100], type = 'o')
plot(index2, data6_2[2,51:100], type = 'o')
plot(index2, data6_2[4,51:100], type = 'o')

plot(index2, data6_3[1,51:100], type = 'o')
plot(index2, data6_3[2,51:100], type = 'o')
plot(index2, data6_3[3,51:100], type = 'o')

plot(index2, data6_4[1,51:100], type = 'o')
plot(index2, data6_4[2,51:100], type = 'o')
plot(index2, data6_4[3,51:100], type = 'o')

plot(index2, data6_5[1,51:100], type = 'o')
plot(index2, data6_5[2,51:100], type = 'o')
plot(index2, data6_5[3,51:100], type = 'o')

plot(index2, data6_3[1,1:50], type = 'o')
plot(index2, data6_3[2,1:50], type = 'o')
plot(index2, data6_3[4,1:50], type = 'o')

##Kmeans clust by timestamps - 1:100
data_ask2<-data2k[, grepl("ask",names(data2k))]
data_bid2<-data2k[, grepl("bid",names(data2k))]
data_spread2<-abs(data_ask2[1:nrow(data_bid2),]-data_bid2[1:nrow(data_bid2),])
data_spread3 = data_spread2[,1:100]
#data_average2_n = as.data.frame(apply(average2,1, normalize))
data_average3_n = as.data.frame(apply(data_spread3, 1, 
                                      function(x)(x-min(x))/(max(x)-min(x)+0.05)))
data_average3_n = as.data.frame(t(data_average3_n))
data_trans_6 = as.data.frame(t(data_average3_n))
#k=5
km3 = kmeans(data_average3_n, 5, iter.max = 10, nstart = 20)
km3
#dim(as.aaray(km2$cluster))
data_average2$cluster5 = (km3$cluster)
data_spread3$cluster = km3$cluster


