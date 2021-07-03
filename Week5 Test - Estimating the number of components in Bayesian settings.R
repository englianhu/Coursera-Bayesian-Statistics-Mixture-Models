# Estimating the number of components in Bayesian settings

合計点数 4


1. 質問 1

Let K^{*} be the prior expected number of occupied components in a mixture model with $K$ components where the weights are given a Dirichlet prior (w1,…,wK)∼Dir(2K,…,2K).  If you have $n=400$ observations, what is the expected number of occupied components, E(K^{*}) according to the exact formula we discussed in the lecture? Round your answer to one decimal place. (1点)

11.1
正解

The following code computes the expected number of occupied components using the exact formula:

```r
n      = 400
alpha  = 2

EKstar = 0
for(i in 1:n){
  EKstar = EKstar + alpha/(alpha + i -1)
}
print(EKstar)
```


2. 質問 2

Consider the same setup as the previous question, what is the expected number of occupied components, E(K^{*}) according to the exact formula we discussed in the lecture if $n=100$ instead? Round your answer to one decimal place. (1点)

8.4
正解

The following code carries out the caluclation

```r
n      = 100
alpha  = 2

EKstar = 0
for(i in 1:n){
  EKstar = EKstar + alpha/(alpha + i -1)
}
print(EKstar)
```


3. 質問 3

What would be the answer to the previous question if you used the approximate formula instead of the exact formula? Remember to round your answer to one decimal place. (1点)


7.8
正解

The following code will perform the calculation using the approximate formula

```r
n      = 100
alpha  = 2
alpha*log((n+alpha-1)/alpha)
```


4. 質問 4

If you have $n=200$ observations and a priori expect the mixture will have about 2 occupied components $(i.e., E(K^{*}) \approx 2E(K)≈2$ a priori), what value of \alphaα should you use for the prior $(w1,…,wK)∼Dir(αK,…,αK)$.  Use the approximation $E(K^{*}) \approx \alpha \log\left( \frac{n+\alpha-1}{\alpha} \right)E(K)≈αlog((n+α−1)/2) to provide an answer, which should be rounded to two decimal places. (1点)

```r
library('MCMCpack')  x <- exp(rnorm(200)) w <- mean(x) KK      <- 2  # As asked n       <- length(x) aa      <- rep(1,KK)   eta     <- mean(x)     tau     <- sqrt(var(x)) dd      <- 2 qq      <- var(x)/KK mu_0    <- rnorm(KK, eta, tau) sigma_0 <- invgamma::rinvgamma(KK, dd, qq)  ## Initialize the parameters w       <- rep(1,KK)/KK mu      <- rnorm(KK, mean(x), sd(x)) sigma   <- sd(x)/KK cc      <- sample(1:KK, n, replace=TRUE, prob=w)  ## Number of iterations of the sampler rrr     <- 12000 burn    <- 2000  ## Storing the samples cc.out    <- array(0, dim=c(rrr, n)) w.out     <- array(0, dim=c(rrr, KK)) mu.out    <- array(0, dim=c(rrr, KK)) sigma.out <- rep(0, rrr) logpost   <- rep(0, rrr)  for(s in 1:rrr){     # Sample the indicators     for(i in 1:n){         v <- rep(0,KK)         for(k in 1:KK){             v[k] <- log(w[k]) + dnorm(x[i], mu[k], sigma, log=TRUE)  #Compute the log of the weights         }         v     <- exp(v - max(v))/sum(exp(v - max(v)))         cc[i] <- sample(1:KK, 1, replace=TRUE, prob=v)     }          # Sample the weights     ## https://cran.r-project.org/web/packages/rBeta2009/rBeta2009.pdf     ## gtools, extraDistr, MCMCpack also have rbeta and rdirichlet functions.     w <- as.vector(rdirichlet(1, aa + tabulate(cc, nbins=KK)))          # Sample the means     for(k in 1:KK){         nk       <- sum(cc==k)         xsumk    <- sum(x[cc==k])         tau2.hat <- 1/(nk/sigma^2 + 1/sigma_0[k]^2)         mu.hat   <- tau2.hat*(xsumk/sigma^2 + mu_0[k]/sigma_0[k]^2)         mu[k]    <- rnorm(1, mu.hat, sqrt(tau2.hat))     }          # Sample the variances     dd.star    <- dd + n/2     qq.star    <- qq + sum((x - mu[cc])^2)/2     sigma      <- sqrt(1/rgamma(1, dd.star, qq.star))          # Store samples     cc.out[s,]   <- cc     w.out[s,]    <- w     mu.out[s,]   <- mu     sigma.out[s] <- sigma     for(i in 1:n){         logpost[s] <- logpost[s] + log(w[cc[i]]) + dnorm(x[i], mu[cc[i]], sigma, log=TRUE)     }     logpost[s]   <- logpost[s] + log(ddirichlet(w, aa))     for(k in 1:KK){         logpost[s] <- logpost[s] + dnorm(mu[k], eta, tau, log=TRUE)     }     logpost[s]   <- logpost[s] + dgamma(1/sigma^2, dd, qq, log=TRUE) - 4*log(sigma)     if(s/500==floor(s/500)){         print(paste("s =",s))     } }  xx  <- seq(0,7,length.out=150) nxx <- length(xx)  ## Plot EM Density Estimate density.EM = rep(0, nxx) for(s in 1:nxx){     for(k in 1:KK){         density.EM[s] = density.EM[s] + w[k]*dnorm(xx[s], mu[k], sigma)     } }  round(mean(density.EM), 1) #0.1
```

# [A Short Course on Bayesian Nonparametrics Lecture 2 - Introduction to Dirichlet process mixture models]()

