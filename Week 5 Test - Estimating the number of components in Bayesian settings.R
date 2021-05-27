# Estimating the number of components in Bayesian settings

合計点数 4

1. 質問 1

Let K^{*} be the prior expected number of occupied components in a mixture model with $K$ components where the weights are given a Dirichlet prior (w1,…,wK)∼Dir(2K,…,2K).  If you have $n=400$ observations, what is the expected number of occupied components, E(K^{*}) according to the exact formula we discussed in the lecture? Round your answer to one decimal place. (1点)

```r

```

2. 質問 2

Consider the same setup as the previous question, what is the expected number of occupied components, E(K^{*}) according to the exact formula we discussed in the lecture if $n=100$ instead? Round your answer to one decimal place. (1点)

```r

```

3. 質問 3

What would be the answer to the previous question if you used the approximate formula instead of the exact formula? Remember to round your answer to one decimal place. (1点)

```r

```

4. 質問 4

If you have $n=200$ observations and a priori expect the mixture will have about 2 occupied components $(i.e., E(K^{*}) \approx 2E(K)≈2$ a priori), what value of \alphaα should you use for the prior $(w1,…,wK)∼Dir(αK,…,αK)$.  Use the approximation $E(K^{*}) \approx \alpha \log\left( \frac{n+\alpha-1}{\alpha} \right)E(K)≈αlog((n+α−1)/2) to provide an answer, which should be rounded to two decimal places. (1点)

```r

```
