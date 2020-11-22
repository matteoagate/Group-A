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
Y1hat <- lm(ritorni_tutti$Y1 ~ t)
summary(Y1hat)
b1 <- summary(Y1hat)$coefficients[1,1]
a1 <- summary(Y1hat)$coefficients[2,1]

Y2hat <- lm(ritorni_tutti$Y2 ~ t)
summary(Y2hat)
b2 <- summary(Y2hat)$coefficients[1,1]
a2 <- summary(Y2hat)$coefficients[2,1]

Y3hat <- lm(ritotni_tutti$Y3 ~ t)
summary(Y3hat)
b3 <- summary(Y3hat)$coefficients[1,1]
a3 <- summary(Y3hat)$coefficients[2,1]

Y4hat <- lm(ritorni_tutti$Y4 ~ t)
summary(Y4hat)
b4 <- summary(Y4hat)$coefficients[1,1]
a4 <- summary(Y4hat)$coefficients[2,1]

# we compute the root root mean square fluctuation of the detrended series

Y1m <- (ritorni_tutti$Y1 - a1 - b1)
Y1m2 <- (ritorni_tutti$Y1 - a1 - b1)^(2)
sumY1m2 <- sum(Y1m2)
F1 <- (sumY1m2/2517)^(1/2)

Y2m <- (ritorni_tutti$Y2 - a2 - b2)
Y2m2 <- (ritorni_tutti$Y2 - a2 - b2)^(2)
sumY2m2 <- sum(Y2m2)
F2 <- (sumY2m2/2517)^(1/2)

Y3m <- (ritorni_tutti$Y3 - a3 - b3)
Y3m2 <- (ritorni_tutti$Y3 - a3 - b3)^(2)
sumY3m2 <- sum(Y3m2)
F3 <- (sumY3m2/2517)^(1/2)

Y4m <- (ritorni_tutti$Y4 - a4 - b4)
Y4m2 <- (ritorni_tutti$Y4 - a4 - b4)^(2)
sumY4m2 <- sum(Y4m2)
F4 <- (sumY4m2/2517)^(1/2)

# At this point we calculate the mean of the Fm and we find the Hurst parameter,
# looking at the coeffincient of the last regresson done below


Fn <- (F1 + F2 + F3 + F4)/4

prova<-data_frame(Y1m, Y2m, Y3m, Y4m)
final_prova<- prova %>% mutate(mean= ((Y1m+Y2m+Y3m+Y4m))^2/(4*2517))

lm(log(final_prova$mean)~log(t))%>% summary()
