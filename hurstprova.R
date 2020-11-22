
n = length(res)
min.d=100 #min.number of subsets
minm=2 #levels of M 

increment = (log10(n/min.d))/minm
M = floor(10^((1:minm)*increment))


# Absolute values :
ABSVAL = NULL
for (m in  M ) {
  nCols = n %/% m 
  X = matrix(res[1:(m*nCols)], byrow = FALSE, ncol = nCols)
  Y = colMeans(X)
  MEAN = mean(Y)
  STATS = sum( abs(Y-MEAN) ) / (length(Y))
  ABSVAL = c( ABSVAL, STATS )
 X<<-X
}
ABSVAL

# regression:

fit = lsfit(log(M), log(ABSVAL))
b = fit$coef[[2]]
H = b + 1
H 

plot(log(M),log(ABSVAL))

 