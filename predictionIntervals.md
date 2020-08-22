# Prediction intervals (PIs)



## The definition:

In general, a prediction interval $$\Gamma_{\alpha}(X)=\Gamma_{D, \alpha}(X)$$ is an interval-valued function of $$X$$, the data $$D$$, and the confidence level $$\alpha$$, such that, loosely speaking, a new observation falls within the interval with probability at least $$1 - \alpha$$  .

It is comprised of upper  and  lower  bounds  that  bracket  a  future  unknown value  with  a  prescribed  probability, accounting for both model uncertainty and data noise variance.

Definition $1 .$ The prediction interval $\Gamma_{\alpha}(X)$ controls average coverage if
$$
\mathbb{P}\left(Y \in \Gamma_{\alpha}(X)\right) \geq 1-\alpha
$$




## Relationship to Confidence intervals(CIs):

For general learning problem of learning $$\hat f(x)$$ from observation $$
y=f(\mathbf{x})+\epsilon
$$ , the uncertainty comes from two sources: 

	1. the model uncertainty or epistemic uncertainty $$\sigma_{\text {model}}^{2}$$
 	2. irreducible variance or data noise variance  $$\sigma_{\text {noise}}^{2}$$

Ideally, if these two part are independent to each other, the total variance of the observations is given by:
$$
\sigma_{y}^{2}=\sigma_{\text {model}}^{2}+\sigma_{\text {noise}}^{2}
$$
For confidence intervals, it only consider the distribution $$
\operatorname{Pr}(f(\mathbf{x}) \mid \hat{f}(\mathbf{x}))
$$ , and hence only require estimation of $$\sigma_{\text {model}}^{2}$$

Whilst prediction intervals consider $$
\operatorname{Pr}(y \mid \hat{f}(\mathbf{x}))
$$ and must also include $$\sigma_{\text {noise}}^{2}$$



## Ways to estimate PIs



### Delta method



### Mean Variance Estimation(MVE) 



### The Bootstrap 

