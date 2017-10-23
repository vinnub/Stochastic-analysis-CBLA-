

de = seq(0.001,0.4, by = 0.001 )
emp_p_de <- NULL
for ( i in 1:length(de))
{
  delta = de[i]
  cdf <-   1 - pnorm(qnorm(1-p/2) - sqrt(N-3)*delta)+  1 - pnorm(qnorm(1-p/2) + sqrt(N-3)*delta)
  pe_de[i] = 1 - cdf[5000]
  # emp_p_de[i] = ((length(which(delta2 > de[i] & delta2 <= de[i+1])) )/50000) 
}


# THe probability distribution of In-degree in closed for \
p_id <- NULL
for(i in 0:5)
{temp <- 0 
for( j in i:num_nodes)
  temp <- temp + dbinom(j, num_nodes,  2*num_nodes/choose(num_nodes, 2))/(j+1)
p_id[i +1 ] <- temp

}
p_id
sum(p_id)
p_id[length(p_id) + 1] <- 1 - sum(p_id)
p_id
p_id_matrix <- p_id %*% t(p_id)




cal_auc <- function(de, lambda, L, pe_de)
{
  p_de  <- 2 * de* lambda * exp(- lambda * (de^2 ))
   
x <-de
y <- p_de*pe_de
id <- order(x)

AUC <- sum(diff(x[id])*rollmean(y[id],2))
AUC
}

area <- matrix(ncol = 7, nrow = 6)
for (j in 1:6)
for( i in 1:7)
  area[j,i] <-  cal_auc(de, new_lambda[j,i], 0, pe_de)
  
sum(area*p_id_matrix[2:7,])
