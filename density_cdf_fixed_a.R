z.trans <- function(x) { 0.5 * log((1 + x)/ (1-x))}
z = cor_coeff  = val = NULL
p = seq((10^(-5)),  1- (10^(-5)) , by = (10^-5))
N = 500
var_x  = (1)
a = c(0.05, 0.05)
row = sqrt(1/(1 + (1/(a*sqrt(var_x)))^2))   
delta = 1/2*log((1+row)/(1-row))

for(i in 1:10000)
{
  x = rnorm(N, mean = 0, sd = sqrt(var_x))
  y = runif(1,a[1], a[2]) * x + rnorm(N, mean = 0, sd = sqrt(1))
  cor(x,y) 
  cor_coeff[i] <- cor(x,y)
  z[i] <- z.trans(cor_coeff[i])
  p_val <- gaussCItest(1, 2, S = NULL, suffStat = list(C = cor(data <- cbind(x, y )), n = nrow(data)))
  val[i]<- p_val
}

i = 1
p_H1 = 1/2*exp(  log((dnorm(qnorm(1-p/2) - (sqrt(N-3)*delta[i])))  +  (dnorm(qnorm(1-p/2) + (sqrt(N-3)*delta[i])) )) - log(dnorm(qnorm(1-p/2)))   )

cdf <-   1 - pnorm(qnorm(1-p/2) - sqrt(N-3)*delta[i])+  1 - pnorm(qnorm(1-p/2) + sqrt(N-3)*delta[i])

jpeg("Density for row = 0-05.jpg", width = 5, height = 5, units = 'in', res = 300)
hist(val, breaks =200, xlim = c(0,01), probability = 1, xlab = "p-value", ylab = "Density",border = "darkblue",col = "lightblue", main = "")
legend(0.6, 8, c("Simulations", "Analysis"), fill=c("lightblue", "red"))
lines(p, p_H1, type = 'l', col = 'red', lwd = 2, pch = 0.2)
dev.off()

index  = seq(0, 1, by = .050)
temp <- index
jpeg("cdf_for_row = 0-05.jpg", width = 5, height = 5, units = 'in', res = 300)
plot(index,my_ecdf_function(val),type="p",xlim = c(0,1),lwd = 3,pch = 23, ylim=c(0,1), col = "blue", main = "",xlab = "p-value", ylab = "CDF" )
lines(p, cdf , col = "red", lty = "dashed", lwd = 2)
legend(0.6, 0.6, c("Empirical", "Analytical"), lty = c(NA, 'solid'),pch = c(23,NA ),col=c("blue", "red"))
dev.off()




