N=10000
obs=50
beta=0.15

bias=c()

for(i in 1:N){
  x=rnorm(obs,0,1)
  b=rnorm(obs,0,1)
  meas_error_s=rnorm(obs,0,1)
  
  s=b+beta*x #without measurement error
  sm=s+meas_error_s #with measurement error
  
  cor_s = cor(s, x) #correlation of the true dv and iv
  cor_sm = cor(sm, x) #correlation of the dv with measurement error and iv
  p_sm = cor.test(sm, x)$p.value
  
  #select significant cases of cor(sm, x)
  if(p_sm<.05) bias=c(bias,cor_sm-cor_s)
}

#average difference between correlation of the dv with measurement 
#error and iv and correlation of the true dv and iv, positive 
#values mean higher effect size in the measurement error case
mean(bias)
plot(density(bias), main = "Bias caused by measurement error")
abline(v = 0)
abline(v = mean(bias), lty = 2)
