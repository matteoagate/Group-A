Y<-1:16
Y<-log(Y)
n<-c(1,2,4,8)
n<-c(16,8,4,2,1)
m<-c(8,4,2,1)
V<-c()

Y<-log(bynd$Adj.Close)
myfunction<-function(Y,n,m){
  for (i in 1:length(n) ){
    V[i]<- Y[(length(Y)/n[i])*m]-Y[((m-1)*(length(Y)/n[i]))+1]
  }
  V
}

ciccio_pasticcio<-myfunction2(Y,n)

myfunction2<-function(res,n){
  K<-c()
  for (m in 1:tail(n,1)){
    K[i]<-mean(myfunction(res,n,m))
  }
  K
}


lm(log(ciccio_pasticcio)~log(n))%>% summary()
plot(log(n),log(ciccio_pasticcio))
plot.ts(bynd$Adj.Close)



myfunction<-function(Y,n){
 Z<-list()
 m<-list()
   for (i in 1:length(n)){
    m[[i]]<-1:n[i]
     for (j in 1:length(m[[i]])){
    Z[[i]]<- Y[(length(Y)/n[i])*m[[i]][j]]-Y[(((m[[i]][j])-1)*(length(Y)/n[i]))+1]
     }
    }
  Z
}

myfunction(Y,n)

m[[1]]<-1:n[1]
m[[2]]<-1:n[2]
m[[3]]<-1:n[3]
m[[1]]
