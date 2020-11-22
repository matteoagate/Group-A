#In order to find the HFD (Higuchi Fractal Dimension) the alghoritm is the following

#Given a one dimensional time series X: X(1),...,X(N)
PREZZI<-as.numeric(AAPL_2_$`Adj Close`)
RETURNS<-diff(RETURNS)
X<-na.omit(PREZZI)
N<-length(X)
Kmax<-200


#Created a function in order to calculate the Higuchi's fractal dimension

curvelength<- function(ret,K,N,m){
  c<-c()
  for (i in 1:round(((N-m)/K))) {
    c[i]<- abs(ret[m+i*K]-ret[m+(i-1)*K])*((N-1)/((round(((N-m)/K)))*K))/K
  }
  c<-na.omit(c)
  return(c)}

curvelenghtvec<-list()

for (i in 1:Kmax) {
  for (m in 1:i){
    curvelenghtvec[[i]]<-curvelength(X,i,N,m)
    }
}

curvelengthfinal<-c()
for (i in 1:Kmax) {
  curvelengthfinal[i]<-sum(curvelenghtvec[[i]])/i
  
}

#Take the logarithms of the lengths and of 1/k
curvelenghtvec<-log(curvelengthfinal)
kappa<-log((seq(1:Kmax)))

#Made the plot and the regression
plot(kappa,curvelenghtvec)
LM<-lm(curvelenghtvec~kappa)

#HFD
HFD<-LM$coefficients[2]

