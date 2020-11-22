# HSI-Hang Seng Index data

db=file.choose()
hsi=read.csv(db)

hsi$Adj.Close[ hsi$Adj.Close == "null"] <- NA
price<-as.numeric(hsi$Adj.Close[ complete.cases(hsi$Adj.Close)])

  res<-c()
  for (i in 2:length(price)) {
    res[i]<-(log(price[i]/price[i-1]))
  }
  res<-res[-1]
  
#algorithm 
  #1.divide vector in subsets of equal length

  blade<-function(m) {
    d<<-length(res)%/%m
    vet=0
    for (i in 1:(m*d)) {
      vet[i]=res[i]
    }
    vet
    fin<-matrix(vet,ncol=d)
    subset<-as.data.frame(fin)
    subset<<- subset
    m<<-m
  }
  
blade(100)
  
  ##2.calculate the average absolute deviation between the subsets
  
  means<-c()
  for(j in 1:m){
    means[j]=mean(subset[,j])
  }

  means


mean(means)

meanabsv<-c()
for (i in 1:d) {
  meanabsv[i]<- sum(abs(means[i]-mean(means)))/d
}
meanabsv

#### 

x<-seq(1:d)

l<-lm(log(meanabsv)~ log(x))
summary(l)

hurst<-(l$coefficients[2])+1
hurst

