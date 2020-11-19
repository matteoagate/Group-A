Hurst_bellissimo<-function(apple){
apple$Adj.Close<-apple$Adj.Close %>%na.omit()
res<-c()
for (i in 2:length(apple$Adj.Close)) {
  res[i]<-(log(apple$Adj.Close[i]/apple$Adj.Close[i-1]))
}
res<-res[-1]
## STEP NUMBER ONE CALCULATE MEAN VALUE m
m<-mean(res)

## STEP NUMBER TWO CALCULATE MEAN ADJUSTED SERIES Y
new<-c()
for (i in 1:length(res)){
  new[i]<-res[i]-m
}

## Calculate cumulative deviate series Z

Z<-c()
for ( i in 1:length(new)){
  Z[i]<-sum(new[1:i])
}
all(cumsum(new)==Z)

## Calculate range series R

Range<- c()
for(i in 1:length(Z)){
  Range[i]<- max(cumsum(new[1:i]))-min(cumsum(new[1:i]))
}

## Calculate standard deviation series S

S<-c()

for ( i in 1:length(Range)){
  S[i]<-((1/i)*sum((res[1:i]-mean(res[1:i]))^2))^(1/2)
}

##  Calculate standard deviation series S
final<- Range/S
data<-data.frame(Range,S)
data<- data %>% mutate("R/S"= Range/S,
                       "t"=1:length(Range))

myplot<-ggplot(data)+geom_point(aes(x=log(t),y=log(Range/S)))

model<-lm(log(Range/S)~log(t),data)%>%summary()
coeff<-model$coefficients
print(myplot)
alessio<-coeff[2]
alessio
}

## Hurst bellissimo corretto ha bisogno di fix.

Hurst_bellissimo_corretto<-function(apple){
  apple$Adj.Close<-apple$Adj.Close %>%na.omit()
  res<-c()
  for (i in 2:length(apple$Adj.Close)) {
    res[i]<-(log(apple$Adj.Close[i]/apple$Adj.Close[i-1]))
  }
  res<-res[-1]
  ## STEP NUMBER ONE CALCULATE MEAN VALUE m
  m<-mean(res)
  
  ## STEP NUMBER TWO CALCULATE MEAN ADJUSTED SERIES Y
  new<-c()
  for (i in 1:length(res)){
    new[i]<-res[i]-m
  }
  
  ## Calculate cumulative deviate series Z
  
  Z<-c()
  for ( i in 1:length(new)){
    Z[i]<-sum(new[1:i])
  }
  all(cumsum(new)==Z)
  
  ## Calculate range series R
  
  Range<- c()
  for(i in 1:length(Z)){
    Range[i]<- max(cumsum(new[1:i]))-min(cumsum(new[1:i]))
  }
  
  ## Calculate standard deviation series S
  
  S<-c()
  
  for ( i in 1:length(Range)){
    S[i]<-((1/i)*sum((res[1:i]-mean(res[1:i]))^2))^(1/2)
  }
  
  ##  Calculate standard deviation series S
  final<- Range/S
  data<-data.frame(Range,S)
  t<-c()
  for (i in 1:length(Range)){
    t[i]<-2^i
  }
  data<- data %>% mutate("R/S"= Range/S,
                         "t"=t)
  
  
  myplot<-ggplot(data)+geom_point(aes(x=log(t),y=log(Range/S)))
  
  model<-lm(log(Range/S)~log(t),data)%>%summary()
  coeff<-model$coefficients
  print(myplot)
  alessio<-coeff[2]
  alessio
}

## Migliora comando Date

Selected_hurst_bellissimo<-function(apple,Date){
  matteo<-apple$Adj.Close[1:Date] %>%na.omit()
  
  res<-c()
  for (i in 2:length(matteo)) {
    res[i]<-(log(matteo[i]/matteo[i-1]))
  }
  res<-res[-1]
  ## STEP NUMBER ONE CALCULATE MEAN VALUE m
  m<-mean(res)
  
  ## STEP NUMBER TWO CALCULATE MEAN ADJUSTED SERIES Y
  new<-c()
  for (i in 1:length(res)){
    new[i]<-res[i]-m
  }
  
  ## Calculate cumulative deviate series Z
  
  Z<-c()
  for ( i in 1:length(new)){
    Z[i]<-sum(new[1:i])
  }
  all(cumsum(new)==Z)
  
  ## Calculate range series R
  
  Range<- c()
  for(i in 1:length(Z)){
    Range[i]<- max(cumsum(new[1:i]))-min(cumsum(new[1:i]))
  }
  
  ## Calculate standard deviation series S
  
  S<-c()
  
  for ( i in 1:length(Range)){
    S[i]<-((1/i)*sum((res[1:i]-mean(res[1:i]))^2))^(1/2)
  }
  
  ##  Calculate standard deviation series S
  final<- Range/S
  data<-data.frame(Range,S)
  data<- data %>% mutate("R/S"= Range/S,
                         "t"=1:length(Range))
  
  myplot<-ggplot(data)+geom_point(aes(x=log(t),y=log(Range/S)))
  
  model<-lm(log(Range/S)~log(t),data)%>%summary()
  coeff<-model$coefficients
  print(myplot)
  coeff<-coeff[2]
  print(coeff)
}
Selected_hurst_bellissimo(apple,Date = 2500)



## PLOT serve tanto miglioramento
str(X)
plot_moving_hurst_bellissimo<-function(apple,Date){
  andrea<-c()
  t<-1:length(apple)
  X<-c()
  for (i in 100:Date){
    X[i]<-Selected_hurst_bellissimo(apple,Date = i)
  }       
  silvia<-plot.ts(X)
  print(silvia)
}
plot_moving_hurst_bellissimo(apple,120)
Selected_hurst_bellissimo(apple,2000)

str(Selected_hurst_bellissimo())

