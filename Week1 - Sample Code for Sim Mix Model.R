# Sample code for simulating from a Mixture Model
# Generate n observations from a mixture of two Gaussian 
# distributions
n     = 50           # Size of the sample to be generated
w     = c(0.6, 0.4)  # Weights
mu    = c(0, 5)      # Means
sigma = c(1, 2)      # Standard deviations
cc    = sample(1:2, n, replace=T, prob=w)
x     = rnorm(n, mu[cc], sigma[cc])

# Plot f(x) along with the observations 
# just sampled
xx = seq(-5, 12, length=200)
yy = w[1]*dnorm(xx, mu[1], sigma[1]) + 
  w[2]*dnorm(xx, mu[2], sigma[2])
par(mar=c(4,4,1,1)+0.1)
plot(xx, yy, type="l", ylab="Density", xlab="x", las=1, lwd=2)
points(x, y=rep(0,n), pch=1)
