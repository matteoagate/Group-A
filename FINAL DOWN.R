################## R/S

apple<-read.csv("C:\\Users\\matte\\OneDrive\\Desktop\\Econometrics for finance\\AAPL.csv")
bynd<-read.csv("C:\\Users\\matte\\OneDrive\\Desktop\\Econometrics for finance\\BYND.csv")
dji<-read.csv("C:\\Users\\matte\\OneDrive\\Desktop\\Econometrics for finance\\DJI-3.csv")
plot.ts(dji$Adj.Close)

hurst_range<-function(apple,date){
n<-c(5,10,15,20,40,50,80,100,200,400,800,1000,2000)

res<-c()
for (i in 2:length(apple$Adj.Close)) {
  res[i]<-(log(apple$Adj.Close[i]/apple$Adj.Close[i-1]))
}
res<-res[-1]
res<-res[1:date]
length(res)
Y<-res
d<-floor(length(Y)/n)
Z<-list()
for (i in 1:length(d)){
  Z[[i]]<-matrix(Y,n[i],length(Y)/n[i])
}
########## MEAN
## str(Z)
K<-list()
for(i in 1:length(n)){
  K[[i]]<-apply(Z[[i]],2,io <- function(x) {mean(x)})
}
## str(K)
SD<-list()
for(i in 1:length(n)){
  SD[[i]]<-apply(Z[[i]],2,io <- function(x) {sd(x)})
}

M<-list()
for (i in 1:length(n)){
  M[[i]]<-(Z[[i]]-K[[i]])
}

L<-list()
for ( i in 1:length(n)){
  L[[i]]<-apply(M[[i]],2,io<- function(x) cumsum(x))}

S<-list()
for( i in 1:length(n)){
  S[[i]]<-apply(L[[i]],2,io<-function(x){ max(x)-min(x)})
}
##  str(S)
B<-list()

for( i in 1:length(n)){
  B[[i]]<-S[[i]]/SD[[i]]
}
#### str(B)
O<-list()
for( i in 1:length(n)){
  O[[i]]<-lapply(B,io<-function(x){mean(x)})
}
##str(O)


plot(log10(n),log10(as.numeric(O[[1]])))

lm<-lm(log10(as.numeric(O[[1]]))~log10(n))%>% summary()
print(lm$coefficients[2])

}
hurst_range(dji,5000 )

########### AVE

AVE<-function(apple,date){
  n<-c(5,10,15,20,40,50,80,100,200,400,800,1000,2000)
  

res<-c()
for (i in 2:length(apple$Adj.Close)) {
  res[i]<-(log(apple$Adj.Close[i]/apple$Adj.Close[i-1]))
}
res<-res[-1]
res<-res[1:date]
Y<-res
d<-floor(length(Y)/n)
Z<-list()
for (i in 1:length(d)){
  Z[[i]]<-matrix(Y,n[i],length(Y)/n[i])
}


## str(Z)
K<-list()
for(i in 1:length(n)){
  K[[i]]<-apply(Z[[i]],2,io <- function(x) {mean(x)})
}
## str(K)

S<-list()
for(i in 1:length(n)){
  S[[i]]<-mean(abs(K[[i]]))
}
##str(S)

plot(log10(n),log10(as.numeric(S)))


l<-lm(log10(as.numeric(S))~log10(n))
l%>% summary()
l$coefficients[2]+1
}

AVE(dji,5000)


####### AVAREGE VARIANCE
AVE_VAR<-function(apple,date){
  n<-c(5,10,15,20,40,50,80,100,200,400,800,1000,2000)
  res<-c()
for (i in 2:length(apple$Adj.Close)) {
  res[i]<-(log(apple$Adj.Close[i]/apple$Adj.Close[i-1]))
}
res<-res[-1]
res<-res[1:date]

Z<-list()
for (i in 1:length(n)){
  Z[[i]]<-matrix(Y,n[i],length(Y)/n[i])
}


## str(Z)
K<-list()
for(i in 1:length(n)){
  K[[i]]<-apply(Z[[i]],2,io <- function(x) {var(x)})
}


M<-list()
for (i in 1:length(n)){
  M[[i]]<-var(K[[i]])
}

plot(log10(n),log10(as.numeric(M)))
coeff_model<-lm(log10(as.numeric(M))~log10(n))%>% summary()
M_ex<-coeff_model$coefficients[2]
1-(M_ex/2)
}

AVE_VAR(dji,5000)

#########  GEOMETRIC ONE EVVIVA

GEOM<-function(apple,date){
Y<-apple$Adj.Close%>% na.omit()%>% log()
Y<-Y[1:date]
n<-c(10,20,40,80,100,200,400,800,1000,2000)
Z<-list()
for (i in 1:length(n)){
  Z[[i]]<-matrix(Y,n[i],length(Y)/n[i])
}
## str(Z)
K<-list()
for(i in 1:length(n)){
  K[[i]]<-apply(Z[[i]],2,io <- function(x) {tail(x,1)-head(x,1)})
}

## str(K)

M<-list()
for (i in 1:length(n)){
  M[[i]]<-mean(K[[i]])
}

plot(log10(as.numeric(M)),log10(n))
model<-lm(log10(as.numeric(M))~log10(n))%>% summary()
model$coefficients[2]
}

GEOM(dji,5000)

###### GEOMETRIC TWO

GEOM_2<- function(apple,date){
Y<-apple$Adj.Close%>% na.omit()%>% log()
Y<-Y[1:date]
n<-c(10,20,40,80,100,200,400,800,1000,2000)

Z<-list()
for (i in 1:length(n)){
  Z[[i]]<-matrix(Y,n[i],length(Y)/n[i])
}
##str(Z)
K<-list()
for(i in 1:length(n)){
  K[[i]]<-apply(Z[[i]],2,io <- function(x) {max(x,1)-min(x,1)})
}



M<-list()
for (i in 1:length(n)){
  M[[i]]<-mean(K[[i]])
}

plot(log10(as.numeric(M)),log10(n))
model<-lm(log10(as.numeric(M))~log10(n))%>% summary()
model$coefficients[2]
}

GEOM_2(apple,5000)






