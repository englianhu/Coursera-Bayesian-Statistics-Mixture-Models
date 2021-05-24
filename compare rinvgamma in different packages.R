## Load package
if(!suppressPackageStartupMessages(require('invgamma'))) {
  ##install.packages('invgamma', dependencies = TRUE, INSTALL_opts = '--no-lock')
  devtools::install_github('dkahle/invgamma', force=TRUE)
}

## these few packages have `rinvgamma` function.
library(EDISON, extraDistr, MCMCpack)

## function in 'invgamma' will be more accurate than 'MCMCpack'
suppressPackageStartupMessages(require('invgamma'))
#lib('MCMCpack')

### Get a "Bayesian" kernel density estimator based on the same location mixture of 6 normals
## Priors set up using an "empirical Bayes" approach
aa      = rep(1,KK)  
eta     = mean(x)    
tau     = sqrt(var(x))
dd      = 2
qq      = var(x)/KK
mu_0    = rnorm(KK, eta, tau)
sigma_0 = extraDistr::rinvgamma(KK, dd, qq)

## Initialize the parameters
w     = rep(1,KK)/KK
mu    = rnorm(KK, mean(x), sd(x))
sigma = sd(x)/KK
cc    = sample(1:KK, n, replace=T, prob=w)

## Number of iterations of the sampler
rrr   = 12000
burn  = 2000

## Storing the samples
cc.out    = array(0, dim=c(rrr, n))
w.out     = array(0, dim=c(rrr, KK))
mu.out    = array(0, dim=c(rrr, KK))
sigma.out = rep(0, rrr)
logpost   = rep(0, rrr)


for(s in 1:rrr){
  
  # Sample the indicators
  for(i in 1:n){
    v = rep(0, KK)
    
    for(k in 1:KK){
      v[k] = log(w[k]) + dnorm(x[i], mu[k], sigma, log = TRUE)  #Compute the log of the weights
    }
    v      = exp(v - max(v))/sum(exp(v - max(v)))
    cc[i]  = sample(1:KK, 1, replace = TRUE, prob = v)
  }
  
  # Sample the weights
  w = as.vector(rdirichlet(1, aa + tabulate(cc, nbins=KK)))
  
  # Sample the means
  for(k in 1:KK){
    nk       = sum(cc==k)
    xsumk    = sum(x[cc==k])
    tau2.hat = 1/(nk/sigma^2 + 1/sigma_0[k]^2)
    mu.hat   = tau2.hat*(xsumk/sigma^2 + mu_0[k]/sigma_0[k]^2)
    mu[k]    = rnorm(1, mu.hat, sqrt(tau2.hat))
  }
  
  # Sample the variances
  dd.star = dd + n/2
  qq.star = qq + sum((x - mu[cc])^2)/2
  sigma   = sqrt(1/rgamma(1, dd.star, qq.star))
  
  # Store samples
  cc.out[s,]   = cc
  w.out[s,]    = w
  mu.out[s,]   = mu
  sigma.out[s] = sigma
  
  for(i in 1:n){
    logpost[s] = logpost[s] + log(w[cc[i]]) + dnorm(x[i], mu[cc[i]], sigma, log = TRUE)
  }
  logpost[s] = logpost[s] + log(ddirichlet(w, aa))
  for(k in 1:KK){
    logpost[s] = logpost[s] + dnorm(mu[k], eta, tau, log = TRUE)
  }
  logpost[s] = logpost[s] + dgamma(1/sigma^2, dd, qq, log = TRUE) - 4*log(sigma)
  
  if(s/500==floor(s/500)){
    print(paste("s =",s))
  }
}


xx  = seq(0,7,length=150)
nxx = length(xx)

## Compute the samples of the density over a dense grid
density.mcmc = array(0, dim = c(rrr-burn, length(xx)))
for(s in 1:(rrr-burn)){
  for(k in 1:KK){
    density.mcmc[s,] = density.mcmc[s,] + w.out[s+burn,k]*dnorm(xx,mu.out[s+burn,k],sigma.out[s+burn])
  }
}
density.mcmc.m = apply(density.mcmc , 2, mean)



## Plot Bayesian estimate with pointwise credible bands along with kernel density estimate and frequentist point estimate
colscale = c("black", "blue", "red")
yy = density(x)
density.mcmc.lq = apply(density.mcmc, 2, quantile, 0.025)
density.mcmc.uq = apply(density.mcmc, 2, quantile, 0.975)

plot(xx, density.mcmc.m, type="n",ylim=c(0,max(density.mcmc.uq)),xlab="Eruption Duration", ylab="Density")

polygon(c(xx,rev(xx)), c(density.mcmc.lq, rev(density.mcmc.uq)), col="grey", border="grey")
lines(xx, density.mcmc.m, col=colscale[1], lwd=2)
#lines(xx, density.EM, col=colscale[2], lty=2, lwd=2)
lines(yy, col=colscale[3], lty=3, lwd=2)
points(x, rep(0,n))
legend(5, 0.45, c("KDE","EM","MCMC"), col=colscale[c(3,2,1)], lty=c(3,2,1), lwd=2, bty="n")




