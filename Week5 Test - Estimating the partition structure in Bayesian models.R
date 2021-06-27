# Estimating the partition structure in Bayesian models

合計点数 4

1. 質問 1

Binder's loss function is invariant to label switching (1点)

- Yes 正解
- No

```r
正解
Binder's loss function is a function of indicators that check whether $c_i = c_j$ or \hat{c}_i = \hat{c}_j$. Since it only depends on whether the labels for two observations are the same and not on the exact value of the labels themselves, Binder's loss function is invariant to label switching.
```

2. 質問 2

Use the implementation of the MCMC algorithm for fitting a mixture model to the galaxies dataset contained in the lesson "Sample code for estimating the number of components and the partition structure in Bayesian models" to estimate the number of component associated with the optimal partition obtained using Binder's loss function with $\gamma_1 = 3$ and $\gamma_2 = 1$. Make sure to set keep the seed of the random number generator set to 781209. (1点)

```r

```

3. 質問 3

Rerun the algorithm contained in "Sample code for estimating the number of components and the partition structure in Bayesian models" using a prior for the weights (w1,…,wK)∼Dir(0.2K,…,0.2K).  What is the mode for the posterior distribution on $K^*$, the number of occupied clusters? (1点)

```r

```

4. 質問 4

Under the new prior $(w1,…,wK)∼Dir(0.2K,…,0.2K)$‵, what is the number of components in the optimal partitions according to Binder's loss function with $\gamma_1 = \gamma_2$?

```r

```
