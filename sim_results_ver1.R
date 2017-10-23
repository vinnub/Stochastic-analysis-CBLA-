#Simulation Results Version 1 
#Probability of Type 2 error given Delta for various sample sizes and significance levels

par(lwd = 2)
alpha = 0.05
samples = c(200,500,1000,2000)
pe_de_matrix = matrix(ncol = length(de), nrow = length(samples))
for ( j in 1:4)
{N = samples[j]
p = seq((10^(-5)),  1- (10^(-5)) , by = (10^-5))
de = seq(0.001,0.4, by = 0.001 )
pe_de <- rep(0, length(de))
for ( i in 1:length(de))
{
  delta = de[i]
  cdf <-   1 - pnorm(qnorm(1-p/2) - sqrt(N-3)*delta)+  1 - pnorm(qnorm(1-p/2) + sqrt(N-3)*delta)
  pe_de[i] = 1 - cdf[alpha*10^5]
}
pe_de_matrix[j,] = pe_de

}

jpeg("P_E_vs_delta_various_N.jpg", width = 5, height = 5, units = 'in', res = 300)
plot(de, pe_de_matrix[1,], lty = 'solid', type = "l", col = "blue", xlab = TeX('$\\delta$'), ylab =  TeX('$\\Pr(E_2|\\delta)$'))
lines(de, pe_de_matrix[2,], col = "red", lty = "dashed")
lines(de, pe_de_matrix[3,], col = "magenta", lty  = 'dotted')
lines(de, pe_de_matrix[4,], col = "Black", lty = "longdash")
legend(0.25, 0.7,c(TeX('$N_s = 200$'), TeX('$N_s = 500$'), TeX('$N_s = 1000$'), TeX('$N_s = 2000$') ),lty = c('solid', 'dashed', 'dotted', 'longdash'),col=c("Blue", "Red", "magenta","black"))
dev.off()


par(lwd = 2)
alphas = c(0.001,0.01,0.05, 0.1)
pe_de_matrix = matrix(ncol = length(de), nrow = length(alpha))
N = 500
for ( j in 1:length(alpha))
{
alpha = alphas[j]
p = seq((10^(-5)),  1- (10^(-5)) , by = (10^-5))
de = seq(0.001,0.4, by = 0.001 )
pe_de <- rep(0, length(de))
for ( i in 1:length(de))
{
  delta = de[i]
  cdf <-   1 - pnorm(qnorm(1-p/2) - sqrt(N-3)*delta)+  1 - pnorm(qnorm(1-p/2) + sqrt(N-3)*delta)
  pe_de[i] = 1 - cdf[alpha*10^5]
}
pe_de_matrix[j,] = pe_de

}

jpeg("P_E_vs_delta_various_Alpha.jpg", width = 5, height = 5, units = 'in', res = 300)
plot(de, pe_de_matrix[1,], lty = 'solid', type = "l", col = "blue", xlab = TeX('$\\delta$'), ylab =  TeX('$\\Pr(E_2|\\delta)$'))
lines(de, pe_de_matrix[2,], col = "red", lty = "dashed")
lines(de, pe_de_matrix[3,], col = "magenta", lty  = 'dotted')
lines(de, pe_de_matrix[4,], col = "black", lty  = 'dotted')
legend(0.25, 0.7,c(TeX('$\\alpha = 0.001$'), TeX('$\\alpha = 0.01$'), TeX('$\\alpha = 0.05$'), TeX('$\\alpha = 0.1$') ),lty = c('solid', 'dashed', 'dotted', 'longdash'),col=c("Blue", "Red", "magenta","black"))
dev.off()


