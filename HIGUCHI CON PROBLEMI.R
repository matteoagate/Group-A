library(dplyr)
library(ggplot2)
library(xts)
X<-X_DJI$`Adj Close`

HIGUCHIKMAX32<-function(X){
N<-length(X)

curvelength<- function(ret,K,N,m){
  c<-c()
  for (i in 1:round(((N-m)/K))) {
    c[i]<- abs(ret[m+i*K]-ret[m+(i-1)*K])*((N-1)/((round(((N-m)/K)))*K))/K
  }
  c<-na.omit(c)
  return(c)}


curvelengthprova1<-function(X,K,N)
{
  curvelength<-list()
  for (i in 1:K) {
    curvelength[[i]]<-curvelength(X,K,N,i)
  }
  curvelength
  
}
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
curva17<-curvelengthprova1(X,17,N)
curva18<-curvelengthprova1(X,18,N)
curva19<-curvelengthprova1(X,19,N)
curva20<-curvelengthprova1(X,20,N)
curva21<-curvelengthprova1(X,21,N)
curva22<-curvelengthprova1(X,22,N)
curva23<-curvelengthprova1(X,23,N)
curva24<-curvelengthprova1(X,24,N)
curva25<-curvelengthprova1(X,25,N)
curva26<-curvelengthprova1(X,26,N)
curva27<-curvelengthprova1(X,27,N)
curva28<-curvelengthprova1(X,28,N)
curva29<-curvelengthprova1(X,29,N)
curva30<-curvelengthprova1(X,30,N)
curva31<-curvelengthprova1(X,31,N)
curva32<-curvelengthprova1(X,32,N)



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

curvelength17<-c()
for (i in 1:17) {
  curvelength17[i]<-sum(curva17[[i]])
}
curvelength17<-(sum(curvelength17))/17

curvelength18<-c()
for (i in 1:18) {
  curvelength18[i]<-sum(curva18[[i]])
}
curvelength18<-(sum(curvelength18))/18

curvelength19<-c()
for (i in 1:19) {
  curvelength19[i]<-sum(curva19[[i]])
}
curvelength19<-(sum(curvelength19))/19

curvelength20<-c()
for (i in 1:20) {
  curvelength20[i]<-sum(curva20[[i]])
}
curvelength20<-(sum(curvelength20))/20

curvelength21<-c()
for (i in 1:21) {
  curvelength21[i]<-sum(curva21[[i]])
}
curvelength21<-(sum(curvelength21))/21

curvelength22<-c()
for (i in 1:22) {
  curvelength22[i]<-sum(curva22[[i]])
}
curvelength22<-(sum(curvelength22))/22

curvelength23<-c()
for (i in 1:23) {
  curvelength23[i]<-sum(curva23[[i]])
}
curvelength23<-(sum(curvelength23))/23

curvelength24<-c()
for (i in 1:24) {
  curvelength24[i]<-sum(curva24[[i]])
}
curvelength24<-(sum(curvelength24))/24

curvelength25<-c()
for (i in 1:25) {
  curvelength25[i]<-sum(curva25[[i]])
}
curvelength25<-(sum(curvelength25))/25

curvelength26<-c()
for (i in 1:26) {
  curvelength26[i]<-sum(curva26[[i]])
}
curvelength26<-(sum(curvelength26))/26

curvelength27<-c()
for (i in 1:27) {
  curvelength27[i]<-sum(curva27[[i]])
}
curvelength27<-(sum(curvelength27))/27

curvelength28<-c()
for (i in 1:28) {
  curvelength28[i]<-sum(curva28[[i]])
}
curvelength28<-(sum(curvelength28))/28

curvelength29<-c()
for (i in 1:29) {
  curvelength29[i]<-sum(curva29[[i]])
}
curvelength29<-(sum(curvelength29))/29

curvelength30<-c()
for (i in 1:30) {
  curvelength30[i]<-sum(curva30[[i]])
}
curvelength30<-(sum(curvelength30))/30


curvelength31<-c()
for (i in 1:31) {
  curvelength31[i]<-sum(curva31[[i]])
}
curvelength31<-(sum(curvelength31))/31

curvelength32<-c()
for (i in 1:32) {
  curvelength32[i]<-sum(curva32[[i]])
}
curvelength32<-(sum(curvelength32))/32

#Now we calculate the logarithm of the average lengths

loglk<-log(c(curvelength2,curvelength3,curvelength4,curvelength5,
             curvelength6, curvelength7, curvelength8,curvelength9,curvelength10,curvelength11,curvelength12,curvelength13,
             curvelength14, curvelength15, curvelength16,curvelength17,curvelength18,curvelength19,curvelength20,curvelength21,
             curvelength22, curvelength23, curvelength24,curvelength25,curvelength26,curvelength27,curvelength28,curvelength29,
             curvelength30, curvelength31, curvelength32))
loglk

#Perform a regression beetween log(L(K)) and log(1/K)


plot(loglk,log(c(1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10,1/11,1/12,1/13,1/14,1/15,1/16,1/17,1/18,1/19,1/20,1/21,1/22,1/23,1/24,1/25,1/26,1/27,1/28,1/29,1/30,1/31,1/32)))
REGRESSION<-lm(loglk~log(c(1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10,1/11,1/12,1/13,1/14,1/15,1/16,1/17,1/18,1/19,1/20,1/21,1/22,1/23,1/24,1/25,1/26,1/27,1/28,1/29,1/30,1/31,1/32)))




#We calculate the HFD
HFD<-REGRESSION$coefficients[2]

HURSTEXPONENT<-2-HFD
HURSTEXPONENT
}

ROLLINGHIGUCHI<-function(X,windowsize){
X<-matrix(X[1:length(X)],ncol = windowsize, byrow = TRUE)

A<-apply(X, 1, HIGUCHIKMAX32)

myDate <- as.Date(X_DJI$Date[seq(windowsize,length(X), by=windowsize)])
b<-xts(A,order.by = myDate)
my_list <- list("plotH" = plot.xts(b), "H" = b)
return(my_list)
}

C<-ROLLINGHIGUCHI(X,210)


