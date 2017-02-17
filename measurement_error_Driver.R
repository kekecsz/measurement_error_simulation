N=10000
obs=50
beta=exp(rnorm(N,-2,1))

cs=c()
csm=c()

for(i in 1:N){
  true_x=rnorm(obs,0,1)
  meas_error_x=rnorm(obs,0,1)
  observed_x=true_x+meas_error_x
  error_s=rnorm(obs,0,1)
  
  s=error_s+beta[i]*true_x # only simulated once!
  
  summs=summary(lm(scale(s)~scale(true_x)))
  summsm=summary(lm(scale(s)~scale(observed_x)))
  
  Betas=beta[i]/sd(s)
  Betasm=beta[i]/sd(sm)
  
  #select significant...
  if(summs$coefficients[2,4]<.05) cs=c(cs,summs$coefficients[2,1]-Betas) #bias calc
  if(summsm$coefficients[2,4]<.05) csm=c(csm,summsm$coefficients[2,1]-Betasm)
}


mean(cs) #average bias of sig results without error
mean(csm)#average bias of sig results with error
par(mfrow=c(1,2))
plot(density(beta))
plot(density(cs),main='bias in Beta')
points(density(csm),type='l',col='red')
legend(x='topleft',legend=c('no error', 'error'),text.col=c(1,2))
