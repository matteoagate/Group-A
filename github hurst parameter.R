# choose a series

apple <- read.csv("AAPL-10.csv")
apple <- na.omit(apple)
appleadj <- as.numeric(apple$Adj.Close)
returns <- diff(log(appleadj))*100

# Divide the series of N length in d subseries of length n
# We establish the number of d, in our case it is 4, so we devide returns in 4. The length of n is
# 2517

returns <- returns[-10069]
date <- apple$Date
date <- date[-1]
date <- date[-10069]
dataframe <- data.frame(date, returns)
library(dplyr)

# it is important to put two elements into the functions below: n and the number of
# subseries that we want into the second for cycle. This number is the i-th element
# of the for cycle. For exaple, if we want 4 subseris it will be: for (i in 1:4)

ritorni <- dataframe$returns
n <- 2517
sottoserie <- function(x) {
  v <- c()
  for (i in 1:n) {
    v[i] <- ritorni[i+(2517*(x-1))]
  }
  return(v)
}


ritorni_tutti <- data.frame(x=rep(0, n))

for (y in 1:4) {
  ritorni_tutti[,y] <- sottoserie(y)
}

ritorni_tutti


# Creation of new cumulative time series

ritorni_tutti <- ritorni_tutti %>% mutate(Y1 = cumsum(x), Y2 = cumsum(V2), 
                                          Y3 = cumsum(V3), Y4 = cumsum(V4))


# We fit a least square line for every series
# we need a verctor that specifies the n

t <- c(1:2517)

b <- c()
for (i in 1:4) {
  b[i] <- summary(lm(ritorni_tutti[,i+4] ~ t))$coefficients[1]
}

a <- c()
for (i in 1:4) {
  a[i] <- summary(lm(ritorni_tutti[,i+4] ~ t))$coefficients[2]
}

# we compute the root root mean square fluctuation of the detrended series

Y1m <- (ritorni_tutti$Y1 - a[1] - b[1])
Y1m2 <- (ritorni_tutti$Y1 - a[1] - b[1])^(2)
sumY1m2 <- sum(Y1m2)
F1 <- (sumY1m2/2517)^(1/2)

Y2m <- (ritorni_tutti$Y2 - a[2] - b[2])
Y2m2 <- (ritorni_tutti$Y2 - a[2] - b[2])^(2)
sumY2m2 <- sum(Y2m2)
F2 <- (sumY2m2/2517)^(1/2)

Y3m <- (ritorni_tutti$Y3 - a[3] - b[3])
Y3m2 <- (ritorni_tutti$Y3 - a[3] - b[3])^(2)
sumY3m2 <- sum(Y3m2)
F3 <- (sumY3m2/2517)^(1/2)

Y4m <- (ritorni_tutti$Y4 - a[4] - b[4])
Y4m2 <- (ritorni_tutti$Y4 - a[4] - b[4])^(2)
sumY4m2 <- sum(Y4m2)
F4 <- (sumY4m2/2517)^(1/2)

# At this point we calculate the mean of the Fm and we find the Hurst parameter,
# looking at the coeffincient of the last regresson done below


Fn <- (F1 + F2 + F3 + F4)/4

prova<-data_frame(Y1m, Y2m, Y3m, Y4m)
final_prova<- prova %>% mutate(mean= (((Y1m+Y2m+Y3m+Y4m))^2/(2517))^(1/2))/4

lm(log(final_prova$mean)~log(t))%>% summary()
