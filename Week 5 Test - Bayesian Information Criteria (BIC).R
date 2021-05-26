# Bayesian Information Criteria (BIC)

合計点数 1

1. 質問 1

Consider a K-component mixture of D-dimensional Multinomial distributions,

$$f(\mathbf{x}) =\sum_{k=1}^{K} w_k {x_1+x_2 + \cdots + x_D \choose x_1 \; x_2 \; \cdots \; x_D} \prod_{d=1}^{D} \theta_{d,k}^{x_d}$$

where $\mathbf{x} = (x_1 , \ldots, x_D)$ and $\sum_{d=1}^{D} \theta_{d,k}=1$ for all $k = 1, \ldots, K$. For the purpose of computing the BIC, what is the effective number of parameters in the model? (1点)

- (K-1) + K x D(K−1)
- K+K x (D-1)
- (K-1) + K x (D-1) 正解
- (K-1) x (D-1)

正解
There are $K-1$ independent weights, and each of the $K$ components has $D-1$ independent parameters (remember that $\sum_{d=1}^{D} \theta_{d,k}=1$).
