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

# Try with Kmax=16

curvelengthprova1<-function(X,K,N)
{
  curvelength<-list()
  for (i in 1:K) {
    curvelength[[i]]<-curvelength(X,K,N,i)
  }
  curvelength
  
}
curva1<-curvelengthprova1(X,1,N)
curva2<-curvelengthprova1(X,2,N)
curva3<-curvelengthprova1(X,3,N)
curva4<-curvelengthprova1(X,4,N)
curva5<-curvelengthprova1(X,5,N)
curva6<-curvelengthprova1(X,6,N)
curva7<-curvelengthprova1(X,7,N)
curva8<-curvelengthprova1(X,8,N)
curva9<-curvelengthprova1(X,9,N)
curva10<-curvelengthprova1(X,10,N)
curva11<-curvelengthprova1(X,11,N)
curva12<-curvelengthprova1(X,12,N)
curva13<-curvelengthprova1(X,13,N)
curva14<-curvelengthprova1(X,14,N)
curva15<-curvelengthprova1(X,15,N)
curva16<-curvelengthprova1(X,16,N)


curvelength1<-sum(curva1[[1]])
curvelength2<-(sum(curva2[[1]])+sum(curva2[[2]]))/2
curvelength3<-c()
for (i in 1:3) {
  curvelength3[i]<-sum(curva3[[i]])
}
curvelength3<-sum(curvelength3)/3

curvelength4<-c()
for (i in 1:4) {
  curvelength4[i]<-sum(curva4[[i]])
}
curvelength4<-(sum(curvelength4))/4

curvelength5<-c()
for (i in 1:5) {
  curvelength5[i]<-sum(curva5[[i]])
}
curvelength5<-(sum(curvelength5))/5

curvelength6<-c()
for (i in 1:6) {
  curvelength6[i]<-sum(curva6[[i]])
}
curvelength6<-(sum(curvelength6))/6

curvelength7<-c()
for (i in 1:7) {
  curvelength7[i]<-sum(curva7[[i]])
}
curvelength7<-(sum(curvelength7))/7

curvelength8<-c()
for (i in 1:8) {
  curvelength8[i]<-sum(curva8[[i]])
}
curvelength8<-(sum(curvelength8))/8

curvelength9<-c()
for (i in 1:9) {
  curvelength9[i]<-sum(curva9[[i]])
}
curvelength9<-(sum(curvelength9))/9

curvelength10<-c()
for (i in 1:10) {
  curvelength10[i]<-sum(curva10[[i]])
}
curvelength10<-(sum(curvelength10))/10

curvelength11<-c()
for (i in 1:11) {
  curvelength11[i]<-sum(curva11[[i]])
}
curvelength11<-(sum(curvelength11))/11

curvelength12<-c()
for (i in 1:12) {
  curvelength12[i]<-sum(curva12[[i]])
}
curvelength12<-(sum(curvelength12))/12

curvelength13<-c()
for (i in 1:13) {
  curvelength13[i]<-sum(curva13[[i]])
}
curvelength13<-(sum(curvelength13))/13

curvelength14<-c()
for (i in 1:14) {
  curvelength14[i]<-sum(curva14[[i]])
}
curvelength14<-(sum(curvelength14))/14

curvelength15<-c()
for (i in 1:15) {
  curvelength15[i]<-sum(curva15[[i]])
}
curvelength15<-(sum(curvelength15))/15

curvelength16<-c()
for (i in 1:16) {
  curvelength16[i]<-sum(curva16[[i]])
}
curvelength16<-(sum(curvelength16))/16


#Now we calculate the logarithm of the average lengths

loglk<-log(c(curvelength1,curvelength2,curvelength3,curvelength4,curvelength5,
                      curvelength6, curvelength7, curvelength8,curvelength9,curvelength10,curvelength11,curvelength12,curvelength13,
                      curvelength14, curvelength15, curvelength16))


#Perform a regression beetween log(L(K)) and log(1/K)


plot(loglk,log(c(1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10,1/11,1/12,1/13,1/14,1/15,1/16)))
REGRESSION<-lm(loglk~log(c(1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10,1/11,1/12,1/13,1/14,1/15,1/16)))




#We calculate the HFD
HFD<-REGRESSION$coefficients[2]

HURSTEXPONENT<-2-HFD


HURSTEXPONENT
