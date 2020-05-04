require("cluster")
require("fpc")
require("factoextra")
require("gridExtra")
library(cluster)
library(fpc)
library(factoextra)
library(gridExtra)
library(conjoint)

load("/Users/liutianyao/Desktop/Analytics design/project 3/GBA424 - Toy Horse Case Data.Rdata")

###Question A
#data cleaning: drop rows with NA
conjointData1 = conjointData[complete.cases(conjointData), ]
conjointDataNA <- conjointData[is.na(conjointData$ratings),]

#create a dataframe and fill each individual's part-utilities
coef <- data.frame(ID = rep(NA, 200), intercept = rep(NA,200), 
                   price_119.99 = rep(NA,200), size_26 = rep(NA,200), motion_rock = rep(NA,200),
                   style_glam = rep(NA,200))
for(i in 1:200) {
  a = lm(ratings~price+size+motion+style, data = subset(conjointData1, ID==i))
  coef[i,1] = i
  coef[i,2:6] = a$coefficient[1:5]
  coef
} 


#predict missing value
for (i in 1:200){
  for (j in c(3,6,10,16)){
    conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 3] = 
      coef[i,2] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 4] * coef[i,3] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 5] * coef[i,4] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 6] * coef[i,5] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 7] * coef[i,6]
  }
  
}

#merge and reorder the dataframe by ID and profile
conjointData_full <-rbind(conjointDataNA, conjointData1)
conjointData_full <-conjointData_full[order(conjointData_full$ID, conjointData_full$profile),]


###Question B###
##Evaluate number of clusters to use on data with visualizations
clustTest = function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(scale){ toClust = scale(toClust);}
  set.seed(seed);   # set random number seed before doing cluster analysis
  wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var))
  for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
  ##gpw essentially does the following plot using wss above. 
  #plot(1:maxClusts, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  gpw = fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) #alternative way to get wss elbow chart.
  pm1 = pamk(toClust,scaling=TRUE)
  ## pm1$nc indicates the optimal number of clusters based on 
  ## lowest average silhoutte score (a measure of quality of clustering)
  #alternative way that presents it visually as well.
  gps = fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
  if(print){
    grid.arrange(gpw,gps, nrow = 1)
  }
  list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}

##Runs a set of clusters as kmeans
runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
    
  }
  library(gridExtra)
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}

##Plots a kmeans cluster as three plot report
##  pie chart with membership percentages
##  ellipse plot that indicates cluster definitions against principle components
##  barplot of the cluster means
plotClust = function(km,toClust,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  clusplot(toClust, km$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}

partworth = coef[2:6]

checks = clustTest(partworth,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
clusts = runClusts(partworth,c(3,4,9,14),print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
plotClust(clusts[[1]][[1]],partworth)



###Question C Priori segmentation###
conjointDemo <- merge(conjointData1, respondentData, by = 'ID')

#Base on age and gender together
#age = 0: 2 years old
age0 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 0))
#age = 1: 3-4 years old
age1 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 1))
#gender = 0: boy
gender0 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, gender == 0))
#gender = 1: girl
gender1 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, gender == 1))

#age = 0, gender = 0, 2 year-old boy 
age0gender0 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 0 & gender == 0))
#age = 0, gender = 1, 2 year-old girl 
age0gender1 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 0 & gender == 1))
#age = 1, gender = 0, 3-4 year-old boy
age1gender0 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 1 & gender == 0))
#age = 1, gender = 1, 3-4 year-old girl
age1gender1 = lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 1 & gender == 1))

coefDemo <- data.frame(demo = rep(NA, 8), intercept = rep(NA,8), 
                   price_119.99 = rep(NA,8), size_26 = rep(NA,8), motion_rock = rep(NA,8),
                   style_glam = rep(NA,8))
rownames(coefDemo) = c('age0','age1','gender0','gender1','age0gender0','age0gender1','age1gender0','age1gender1')
coefDemo[1,2:6] = age0$coefficients
coefDemo[2,2:6] = age1$coefficients
coefDemo[3,2:6] = gender0$coefficients
coefDemo[4,2:6] = gender1$coefficients
coefDemo[5,2:6] = age0gender0$coefficients
coefDemo[6,2:6] = age0gender1$coefficients
coefDemo[7,2:6] = age1gender0$coefficients
coefDemo[8,2:6] = age1gender1$coefficients
coefDemo[,1] = rownames(coefDemo)



#Significance test
#base on gender
summary(lm(ratings~price+size+motion+style+gender:price+gender:size+gender:motion+gender:style, data = conjointDemo))
#base on age
summary(lm(ratings~price+size+motion+style+age:price+age:size+age:motion+age:style, data = conjointDemo))



###Question D###

#calculate market share
## Rate Chart
Rate = as.data.frame(c(1:200))

for (i in 1:200){
  ID_data = conjointData_full[conjointData_full$ID==i,]
  Rate[i,c(2:17)] = t(ID_data[,3])
}

colnames(Rate) <- c('ID','P1','P2','P3','P4',
                    'P5','P6','P7','P8','P9',
                    'P10','P11','P12','P13','P14',
                    'P15','P16')
RateData <- Rate[,-1]

#market share
simFCSharesA = function(scen,data,ascend=TRUE){ 
  inmkt = data[,scen] #construct the subsetted matrix of options
  scores = matrix(c(rep(0,length(scen))))
  if(ascend){ #if ranks 1 is best
    for (i in seq(1:nrow(inmkt))) {
      z = min(inmkt[i,])
      if (sum(inmkt[i,]==z) == 1) {
        scores[inmkt[i,]==z] = scores[inmkt[i,]==z] + 1
      }
      else {
        j = sum(inmkt[i,]==z)
        scores[inmkt[i,]==z] = scores[inmkt[i,]==z] + 1/j
      }
    }
  } else { #else the best rank is the largest number
    for (i in seq(1:nrow(inmkt))) {
      y = max(inmkt[i,])
      if (sum(inmkt[i,]==y) == 1) {
        scores[inmkt[i,]==y] = scores[inmkt[i,]==y] + 1
      }
      else {
        j = sum(inmkt[i,]==y)
        scores[inmkt[i,]==y] = scores[inmkt[i,]==y] + 1/j
      }
    }
  }
  marketShare = data.frame(t(scores/i))
  colnames(marketShare) = colnames(inmkt)
  marketShare
}
simFCSharesA(1:16, RateData[])


#create scenarios (add products with higher market shares and use different conbination plans)
scen0 = c(13, 5, 7) #current market
scen1 =c(13, 5, 4, 14, 16, 8)  #launch top 3 profiles
scen2 = c(4,5,13, 16, 8)  #delete 5 (0 market share), launch top 3 profiles
scen3 = c(4, 5,14,13, 8) #only launch top 3 profiles
scen4 = c(4,5, 8) #delete 4
scen5 = c(5, 8) #delete 14
scen6 = c(4, 14, 8) #delete 16

simFCSharesA(scen0, RateData[])
simFCSharesA(scen1, RateData[])
simFCSharesA(scen2, RateData[])
simFCSharesA(scen3, RateData[])
simFCSharesA(scen4, RateData[])
simFCSharesA(scen5, RateData[])
simFCSharesA(scen6, RateData[])

scen0 = simFCSharesA(c(13,5,7),RateData[,])
scen0
scen1 = simFCSharesA(c(4,14,16, 13, 5, 8),RateData[,])
scen1
scen2 = simFCSharesA(c(4,13, 5, 8),RateData[,])
scen2
scen3 = simFCSharesA(c(4,14,5,8),RateData[,])
scen3
scen4 = simFCSharesA(c(4,5,8),RateData[,])
scen4

#Costs = 20,000/year * #products + $20,000/3 *#products not in existing set
#shortrun - 1 year period
Profit0 = (0.385*4000)*(111.99-33) + (0.42*4000)*(111.99-33)-20000*2
Profit1 = 0.395*(111.99-33)*4000+0.42*(111.99-33)*4000+0.185*(95.99-29)*4000-20000*5-20000/3*3
Profit2 = 0.42*(111.99-33)*4000+0.185*(95.99-29)*4000+0.395*(111.99-33)*4000-20000*3-20000/3*1
profit3 = 0.78*4000*(111.99-33)+0.215*4000*(95.99-29)+0.005*4000*(95.99-33)-20000*3-20000/3*2
profit4 = 0.785*4000*(111.99-33)+0.215*4000*(95.99-29)-20000*2-20000/3*1
profit5 = 0.755*4000*(111.99-33)+0.095*4000*(95.99-29)+0.15*4000*(95.99-29)-20000*3-20000/3*2 # $230,866
profit6 = 0.45*4000*(95.99-29)+0.265*4000*(95.99-29)-20000*2-20000/3*2 # $138,258
profit7 = 0.79*4000*(111.99-33)+0.20*4000*(95.99-29)-20000*2-20000/3*1 # $256,533

#profile 4 has highest profit for shortrun

#longrun - 5 years period
Profit0 = 5*(0.385*4000)*(111.99-33) + 5*(0.42*4000)*(111.99-33)-5*20000*2
Profit1 = 0.395*(111.99-33)*4000+0.42*(111.99-33)*4000+0.185*(95.99-29)*4000-20000*5-20000/3*3
Profit2 = 0.42*(111.99-33)*4000+0.185*(95.99-29)*4000+0.395*(111.99-33)*4000-20000*3-20000/3*1
profit3 = 0.78*4000*(111.99-33)+0.215*4000*(95.99-29)+0.005*4000*(95.99-33)-20000*3-20000/3*2
profit4 = 5*0.785*4000*(111.99-33)+5*0.215*4000*(95.99-29)-5*20000*2-20000/3*1

#profile 3 has highest profit for both longrun and shortrun situation




