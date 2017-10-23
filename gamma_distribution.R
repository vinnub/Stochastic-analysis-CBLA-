#CALCULATING LAMBDA FOR different value of P and Q

q = 1
NN = 10000

mins <- lambda <- matrix(nrow =6, ncol = 6)
for ( q in 2:2)
{
gamma <- matrix( nrow = NN, ncol = 6 )
p = 1
a = rmydistb(NN)
B <- matrix (nrow = NN, ncol = 0)
C <-  matrix (nrow = NN, ncol = q)
for ( i in 1:q)
  C[,i] <- rmydistb(NN)

gamma[,1] <- (1 )/((1 + apply(C^2, 1, sum)) * a^2  )

for( p in 2:6)
{
a = rmydistb(NN)
B <- matrix (nrow = NN, ncol = p-1)
C <- matrix (nrow = NN, ncol = q)
for ( i in 1:(p-1))
  B[,i] <- rmydistb(NN)
for ( i in 1:q)
  C[,i] <- rmydistb(NN)
 gamma[, p] = (1 + apply(B^2, 1, sum))/((1 + apply(C^2, 1, sum)) * a^2  )
 
}

row <-  sqrt(1 /(1 + gamma))
delta = 0.5 * log((1 +row)/(1-row))
means <- apply(delta^2, 2, mean)
mins[,q] <- apply(delta^2, 2, min) 
lambda[, q]   <- 1/(means - mins[,q])
}



#FOR Q =0
gamma0 <- matrix( nrow = NN, ncol = 5 )
for( p in 2:6)
{
  a = rmydistb(NN)
  B <- matrix (nrow = NN, ncol = p-1)
  for ( i in 1:(p-1))
    B[,i] <- rmydistb(NN)
gamma0[, p-1] = (1 + apply(B^2, 1, sum))/(a^2  )
}
row0 <-  sqrt(1 /(1 + gamma0))
delta0 = 0.5 * log((1 +row0)/(1-row0))

means0 <- apply(delta0^2, 2, mean)
mins0 <- apply(delta0^2, 2, min)
new_lambda <- cbind(c(0,1/(means0 - mins0)), lambda)
new_mins <-  cbind(c(0,mins0), mins)

#P = 1 Q = 0
temp = 1/(a^2)
temp_row = sqrt(1/(1+temp))
temp_delta =  0.5 * log((1 +temp_row)/(1-temp_row))

new_mins[1,1]<-  min(temp_delta^2)
new_lambda[1,1] <- 1/(mean(temp_delta^2) -min(temp_delta^2) )

save(new_mins, file = "L_for_exponential_distbr.rda")
save(new_lambda, file = "lambda_values_for_p_q.rda")






exp_ecdf <- 1 - exp(- lambda*(p - L))
exp_ecdf[p <= L] <- 0
lines(p, exp_ecdf)

temp = seq(0.000,1.4, by = 0.001)
my_ecdf_function <- function (x)
{ my_ecdf <- NULL
  
  for(i in 1:length(temp))
  my_ecdf[i] = length(which(x <= temp[i]))/length(x)
  return(my_ecdf)
  }



