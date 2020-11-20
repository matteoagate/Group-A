Aggregate_variance_method<- function(apple,m){
  apple$Adj.Close<-apple$Adj.Close %>%na.omit()
  res<-c()
  for (i in 2:length(apple$Adj.Close)) {
    res[i]<-(log(apple$Adj.Close[i]/apple$Adj.Close[i-1]))
  }
  res<-res[-1]
d<-length(res)/m
z<-c()
for (i in 1:(d-1))
  ## ATTENTION SI VOUS PLAIT d-1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ## d-1 PERCHE NON VENGONO VETTORI  DELLA STESSA LUNGHEZZA A CAUSA DELLA LUNGHEZZA DI RES
  
  {
  z[i]<-((1/m)*sum((res[(i-1)*m+1 ]):(res[(i*m)]))^2)   ## il +1 è forte come affermazione
  ## QUESTA E' L'AFFERMAZIONE FORTE!!!!!!!!
  ## CREO UN VETTORE CON LE VARIANZE E REGREDISCO IL TUTTO SU LA LOG DI QUESTO
  
  }


x<-seq(1:length(z))

l<-lm(log(z)~log(x))%>%  summary()

coeff<-l$coefficients[2]

outcome<-coeff/2+1

outcome
}
Aggregate_variance_method(apple = apple, m=32)
Aggregate_variance_method(apple = apple, m=64)
Aggregate_variance_method(apple = apple, m=128)
