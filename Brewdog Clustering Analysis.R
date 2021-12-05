getwd()
setwd("C:/R/BrewDogNew.csv")

library(dplyr)
data <-read.csv("C:/R/BrewDogNew.csv", header=TRUE, stringsAsFactors = T)
data
dim(data)
View(data)

data_missing <- data %>%
  filter(data$Name == "SPEEDBIRD")

View(data_missing)



#Simple Imputation,
#==================

mean(data$Price,na.rm = TRUE)
data_mean <-data
data_mean
data_mean$Price[is.na(data_mean$Price)] <-mean(data$Price,na.rm = TRUE)
data_mean$Price[is.na(data_mean$Price)]<-5.271429
mean(data_mean$Price, na.rm = TRUE )
median(data$Price,na.rm = TRUE)
data_median <-data
data_median$Price[is.na(data_median$Price)]<-5.45
data_median
data_mode <-data

# Create the function
#======================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <-getmode(data_mode$Price)
result
data_mode$Price[is.na(data_mode$Price)]<-5.95
data_mode


#Data Manipulation
#====================

library(dplyr)
dim(data)
names(data)
data


sum(complete.cases(data))
sum(!complete.cases(data))
!complete.cases(data)
which(!complete.cases(data))

install.packages("VIM")
library(VIM)

aggr(data, numbers=TRUE, prop=FALSE)
summary(data$Price, na.rm =TRUE)

install.packages("corrgram")
library(corrgram)
corrgram(data)


#Multiple Imputation
#===================

install.packages("mice")
library(mice)

data_Mul_Imputation <-data
data_Mul_Imputation

md.pattern(data_Mul_Imputation)
matrixplot(data_Mul_Imputation)

impute <-mice(data_Mul_Imputation, m =10)
impute
impute$imp
complete(impute)

data_mi <-complete(impute)
data_mi



summary(data_Mul_Imputation$Price)


#Simple Imputation vs  Multiple Imputation:
#==========================================

summary(data_mean$Price)
summary(data_mi$Price)

#Hierarchical Clustering:
#============================

install.packages("fastcluster")
install.packages("NbClust")
install.packages("cluster")

library(fastcluster)
library(NbClust)
library(cluster)

data_mean

HC1 <-hclust(dist(data_mean[3:6]), "ward.D2")
HC1


plot(HC1, labels=data_mean$Name)
rect.hclust(HC1, 5)

#The above method will not be the correct one , as we ignore the Type column too

dm <-daisy(data_mean[2:6])
HC2 <- agnes(dm, diss = TRUE, method = "ward")
HC2
plot(HC2, labels=data_mean$Name, which.plots=2)
rect.hclust(HC2,5)

Cluster <-data.frame(data_mean, cutree(HC2, k=5))
Cluster[c(1:7)][order(Cluster[5]),]

#K - Mean Clustering
#====================

NumCls <-NbClust(data_mean[3:6], min.nc = 2, max.nc = 10, method = "kmeans")
data1 <-data_mean
data1
data1$Type <-NULL
data1$Name <-NULL
NumCluster <-NbClust(data1, min.nc = 2, max.nc = 10, method = "kmeans")

kmeans_Brew <-kmeans(data1, 3)
kmeans_Brew

Clst <-data.frame(data_mean, kmeans_Brew$cluster)
Clst[order(Clst[7]),]

plot(data_mean[c("Price","ABV")],col=kmeans_Brew$cluster)
text(data_mean$Price, data_mean$ABV, data_mean$Name, cex = 0.6, pos = 2)