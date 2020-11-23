#In order to find the HFD (Higuchi Fractal Dimension) the alghoritm is the following

#Given a one dimensional time series X: X(1),...,X(N)
PREZZI<-as.numeric(AAPL_2_$`Adj Close`)
RETURNS<-diff(RETURNS)
X<-na.omit(PREZZI)
N<-length(X)
Kmax<-200


#Created a function in order to calculate the Higuchi's fractal dimension
curvelength1<.function(ret,K,N,m){
  c<-c()
  for (i in 1:round(((N-m)/K))) {
    c[i]<- abs(ret[m+i*K]-ret[m+(i-1)*K])*((N-1)/((round(((N-m)/K)))*K))/K
  }
  c<-na.omit(c)
  csum<-sum(c)/K  
  return(csum)}

curvelength2<-function(X,k,N){
  L<-c()
  for (m in 1:k) {
    L[m]<-curvelength(X,k,N,m)
  }
  L<-log(sum(L)/k)
  return(L)
}

#This funcion calculate the lenght of the vector for all possible k

LK<-c()
for (i in 1:Kmax) {
  LK[i]<-prova(X,i,N)
  
}
#Made the plot and the regression

KAPPA<-log(1/seq(1:Kmax))
plot(KAPPA,LK)
LM<-lm(LK~KAPPA)

#HFD
HFD<-LM$coefficients[2]
