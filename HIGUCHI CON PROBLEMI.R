#In order to find the HFD (Higuchi Fractal Dimension) the alghoritm is the following

#Given a one dimensional time series X: X(1),...,X(N)
PREZZI<-as.numeric(X_DJI$Close)
X<-na.omit(PREZZI)

N<-length(X)


#Created a function in order to calculate the length of the curve

curvelength<- function(ret,K,N,m){
  c<-c()
  for (i in 1:round(((N-m)/K))) {
    c[i]<- abs(ret[m+i*K]-ret[m+(i-1)*K])*((N-1)/((round(((N-m)/K)))*K))/K
  }
  c<-na.omit(c)
  return(c)}

# Try with Kmax=8


curvelengthvec1<-list()
curvelengthvec2<-list()
curvelengthvec3<-list()
curvelengthvec4<-list()
curvelengthvec5<-list()
curvelengthvec6<-list()
curvelengthvec7<-list()
curvelengthvec8<-list()

curvelengthvec1<-curvelength(X,1,N,1)
curvelengthvec2[[1]]<-curvelength(X,2,N,1)
curvelengthvec2[[2]]<-curvelength(X,2,N,2)
curvelengthvec3[[1]]<-curvelength(X,3,N,1)
curvelengthvec3[[2]]<-curvelength(X,3,N,2)
curvelengthvec3[[3]]<-curvelength(X,3,N,3)
curvelengthvec4[[1]]<-curvelength(X,4,N,1)
curvelengthvec4[[2]]<-curvelength(X,4,N,2)
curvelengthvec4[[3]]<-curvelength(X,4,N,3)
curvelengthvec4[[4]]<-curvelength(X,4,N,4)
curvelengthvec5[[1]]<-curvelength(X,5,N,1)
curvelengthvec5[[2]]<-curvelength(X,5,N,2)
curvelengthvec5[[3]]<-curvelength(X,5,N,3)
curvelengthvec5[[4]]<-curvelength(X,5,N,4)
curvelengthvec5[[5]]<-curvelength(X,5,N,5)
curvelengthvec6[[1]]<-curvelength(X,6,N,1)
curvelengthvec6[[2]]<-curvelength(X,6,N,2)
curvelengthvec6[[3]]<-curvelength(X,6,N,3)
curvelengthvec6[[4]]<-curvelength(X,6,N,4)
curvelengthvec6[[5]]<-curvelength(X,6,N,5)
curvelengthvec6[[6]]<-curvelength(X,6,N,6)
curvelengthvec7[[1]]<-curvelength(X,7,N,1)
curvelengthvec7[[2]]<-curvelength(X,7,N,2)
curvelengthvec7[[3]]<-curvelength(X,7,N,3)
curvelengthvec7[[4]]<-curvelength(X,7,N,4)
curvelengthvec7[[5]]<-curvelength(X,7,N,5)
curvelengthvec7[[6]]<-curvelength(X,7,N,6)
curvelengthvec7[[7]]<-curvelength(X,7,N,7)
curvelengthvec8[[1]]<-curvelength(X,8,N,1)
curvelengthvec8[[2]]<-curvelength(X,8,N,2)
curvelengthvec8[[3]]<-curvelength(X,8,N,3)
curvelengthvec8[[4]]<-curvelength(X,8,N,4)
curvelengthvec8[[5]]<-curvelength(X,8,N,5)
curvelengthvec8[[6]]<-curvelength(X,8,N,6)
curvelengthvec8[[7]]<-curvelength(X,8,N,7)
curvelengthvec8[[8]]<-curvelength(X,8,N,8)

#Now we calculate the average length for all the K, from 1 to 8

curvelength1<-sum(curvelengthvec1)

curvelength2<-(sum(curvelengthvec2[[1]])+sum(curvelengthvec2[[2]]))/2




curvelength3<-c()
for (i in 1:3) {
  curvelength3[i]<-sum(curvelengthvec3[[i]])
}
curvelength3<-sum(curvelength3)/3

curvelength4<-c()
for (i in 1:4) {
  curvelength4[i]<-sum(curvelengthvec4[[i]])
}
curvelength4<-(sum(curvelength4))/4

curvelength5<-c()
for (i in 1:5) {
  curvelength5[i]<-sum(curvelengthvec5[[i]])
}
curvelength5<-(sum(curvelength5))/5

curvelength6<-c()
for (i in 1:6) {
  curvelength6[i]<-sum(curvelengthvec6[[i]])
}
curvelength6<-(sum(curvelength6))/6

curvelength7<-c()
for (i in 1:7) {
  curvelength7[i]<-sum(curvelengthvec7[[i]])
}
curvelength7<-(sum(curvelength7))/7


curvelength8<-c()
for (i in 1:8) {
  curvelength8[i]<-sum(curvelengthvec8[[i]])
}
curvelength8<-(sum(curvelength8))/8


#Now we calculate the logarithm of the average lengths

loglk<-log(c(curvelength1,curvelength2,curvelength3,curvelength4,curvelength5,
                             curvelength6, curvelength7, curvelength8))

#Perform a regression beetween log(L(K)) and log(1/K)

plot(loglk,log(c(1,1/2,1/3,1/4,1/5,1/6,1/7,1/8)))
REGRESSION<-lm(loglk~log(c(1,1/2,1/3,1/4,1/5,1/6,1/7,1/8)))


#We calculate the HFD
HFD<-REGRESSION$coefficients[2]

HURSTEXPONENT<-2-HFD


HURSTEXPONENT
