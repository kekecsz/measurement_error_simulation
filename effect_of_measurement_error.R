# number of simulations per block
N_sim = 1000
# number of observations to try
obs = c(20, 100, 1000)
# effect size (beta) to try
beta = c(0.25, 0.5, 1.5)
# measurement errors to try proportional to beta (for example 3 means the measurement error is three times as high as the effect size) 
error_coef = exp(seq(-2, 2, 0.5))

# this list will contain the mean bias for each block
mean_bias_list = list(NA)

# simulation

for(l in 1:length(obs)){
  #create a matrix to hold output
  mean_bias = matrix(rep(NA, length(error_coef)*length(beta)), ncol = length(beta))
  
  for(k in 1:length(beta)){
    # size of error is calculated proportional to beta
    error_size = beta[k]*error_coef
    
    for(j in 1:length(error_size)){
      # print status to show progress
      print(paste("simulating obs =", obs[l], "beta =", beta[k], "error size = ", round(error_coef[j], 3), "* beta"))
      # vector to hold error data (predicted standardized estimate minus real std beta)
      bias = rep(NA, N_sim)
      
      for(i in 1:N_sim){
        # generate predictor variable for each subject
        x = rnorm(obs[l],0,1)
        # generate random noise for each subject
        noise = rnorm(obs[l],0,1)
        # generate measurement error for each subject
        m_error = rnorm(obs[l],0,error_size[j])
        
        # simulate predicted variable for each subject
        sm = noise+m_error+beta[k]*x
        
        # predicting the predicted variable from the observed data (real underlying predictor variable + measurement error)
        # this gives a standardized beta estimate
        summsm = summary(lm(scale(sm)~scale(x+m_error)))
        # standardized real beta, so it is comparable to predicted standardized beta
        Betasm = beta[k]/sd(sm)
        
        # calculate bias score, which is the difference of predicted strd beta and real strd beta.
        # only focusing on significant results (p < 0.05)
        if(summsm$coefficients[2,4] < 0.05){bias[i] = (summsm$coefficients[2,1]-Betasm)}
      }
      # calculating mean bias across all simulation within the block
      mean_bias[j, k] = mean(na.omit(bias))
    }
  }
  # entering mean bias data into the final list
  mean_bias_list[[l]] = mean_bias
}

# visualization
par(mfrow = c(length(obs),length(beta)))
for(j in 1:length(obs)){
  for(i in 1:length(beta)){
    plot(cbind(round(error_coef, 2), mean_bias_list[[j]][,i]), log = "x", type = "l", ylim = c(min(mean_bias),max(mean_bias)), xlab = "Error size (proportion of beta)", ylab="mean bias", main = paste("obs =", obs[j], "beta =", beta[i]))
  }
}
