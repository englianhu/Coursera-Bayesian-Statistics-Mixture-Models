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

```

# [A Short Course on Bayesian Nonparametrics Lecture 2 - Introduction to Dirichlet process mixture models]()

