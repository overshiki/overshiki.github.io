---
layout: default
---

## Generative model point of view

### The definition
Given the joint distribution between observe $$x$$ and latent variable $$z$$, provided by a generative model:
$$
p(x, z) = p_{\theta}(x|z) p_{\theta}(z)
$$

We start with the marginal distribution of $$x$$ for the model:
$$
p(x) = \int p(x, z) dz = \int p_{\theta}(x|z) p_{\theta}(z) dz
$$

A good generative model should maximize this marginal distribution, i.e. place most of the mass on observed data.
However, searching for $$\theta$$ which maximize this marginal distribution require integral over all the possible values of $$z$$, thus is inefficient.

A possible solution of this problem is to use an inference model $$q_{\phi}(z|x)$$ to narrow the search region thus resulting more effecient sampling


$$
\begin{align*}
logp(x) & = log \int p_{\theta}(x|z) p_{\theta}(z) dz \\
		& = log \int \frac{p_{\theta}(x|z) p_{\theta}(z)}{q_{\phi}(z|x)} q_{\phi}(z|x) dz \\
		&>= \int log \{\frac{p_{\theta}(x|z) p_{\theta}(z)}{q_{\phi}(z|x)} \} q_{\phi}(z|x) dz \\
		& = \mathbb{ELBO}
\end{align*}
$$


This yeilds the ELBO, i.e. evidence lower bound. The inequality is due to the Jensen's inequality for convex function(log function in this case). The name of the evidence lower bound comes from another name of marginal distribution of obsevation $$x \sim p(x)$$: the evidence, thus its lower bound is the so called evidence lower bound.

### The intuitive decomposion of ELBO

#### 1. Minimizing the KL divergence between posteriors
The first intuitive way to decompose ELBO is through analyzing how tight the lower bound is, i.e. how large the term $$logp(x) - \mathbb{ELBO}$$ is[1]:
it turns out


$$
\begin{align*}
logp(x) - \mathbb{ELBO} &= logp(x) - \mathbb{E}_{q_{\phi}(z|x)} log(p_{\theta}(x|z) p_{\theta}(z)) + \mathbb{E}_{q_{\phi}(z|x)} log(q_{\phi}(z|x)) \\
&= logp(x) - \mathbb{E}_{q_{\phi}(z|x)} log(p_{\theta}(x,z)) + \mathbb{E}_{q_{\phi}(z|x)} log(q_{\phi}(z|x)) \\
&= \mathbb{E}_{q_{\phi}(z|x)}(logp(x) -  log(p_{\theta}(x,z)) +  log(q_{\phi}(z|x)) ) \\
&= \mathbb{E}_{q_{\phi}(z|x)}(-log\frac{p_{\theta}(x,z)}{p(x)} + log(q_{\phi}(z|x))) \\
&= \mathbb{KL}(q_{\phi}(z|x)|p_{\theta}(z|x))
\end{align*}
$$
the intuition here is that the difference between the log evidence and the evidence lower bound is the KL divergence between the variational posterior and the true posterior of the hidden variable $$z$$. Thus maximizing the evidence lower bound will bring the KL divergence as small as possible, which means bringing the variational posterior as close as possible to the true posterior distribution.
Note that since:


$$
\mathbb{KL}(q_{\phi}(z|x)|p_{\theta}(z|x)) \ge 0
$$
we have:


$$
logp(x) \ge \mathbb{ELBO}
$$
where equality holds if 


$$
q_{\phi}(z|x) \,{\buildrel d \over =}\, p_{\theta}(z|x)
$$
This is another point of view of how ELBO is related to the log evidence.


#### 2. Maximizing the expect complete likelihood with entropy regularization
The second intuitive way to decompose ELBO is[1]:


$$
\begin{align*}
\mathbb{ELBO} &= \int log (p_{\theta}(x|z) p_{\theta}(z)) q_{\phi}(z|x) dz - \int log (q_{\phi}(z|x)) q_{\phi}(z|x) dz \\
&= \mathbb{E}_{q_{\phi}(z|x)} log(p_{\theta}(x|z) p_{\theta}(z)) - \mathbb{E}_{q_{\phi}(z|x)} log(q_{\phi}(z|x)) \\
&= \mathbb{E}_{q_{\phi}(z|x)} log(p_{\theta}(x|z) p_{\theta}(z)) + \mathbb{Entropy}(q_{\phi}(z|x)) \\
&= \mathbb{E}_{q_{\phi}(z|x)} log(p_{\theta}(x,z)) + \mathbb{Entropy}(q_{\phi}(z|x))
\end{align*}
$$
The first term on the right is the expect complete likelihood in the literature of EM algorithm. The idea is that to find the maximum likelihood estimates in models with latent variables $$z$$, a computing heuristic alternates between a E-step and a M-step could be used. The E-step is to compute the likelihood under the expectation of auxiliary distribution $$q_{\phi}(z|x)$$, while the M-step is to optimize the expectation with respect to model parameters $${\theta, \phi}$$. And through time, the auxiliary distribution would become closer and closer to true posterior distribution, given the fact that ELBO is equal to the log evidence when $$q_{\phi}(z|x) \,{\buildrel d \over =}\, p_{\theta}(z|x)$$ in the above section.

The second term on the right is the entropy of the auxiliary distribution. Maxmizing this term means that we do not want the auxiliary distribution to denerate too much, so that a broad enough search space is avaliable.
#### 3. Maximizing the reweighted likelihood with KL divergence regularization
The third intuitive way to decompose ELBO is:


$$
\begin{align*}
\mathbb{ELBO} & = \int log(p_{\theta}(x|z)) q_{\phi}(z|x) dz + \int log \{ \frac{p_{\theta}(z)}{q_{\phi}(z|x)} \} q_{\phi}(z|x) dz \\
			  & = \mathbb{E}_{q_{\phi}(z|x)}log(p_{\theta}(x|z)) - \mathbb{KL}(q_{\phi}(z|x)||p_{\theta}(z))
\end{align*}
$$
which means maximizing ELBO is equivently to maximizing the likelihood of the generative model given observation when sampling from the inference model, and to minimize the KL divergence between the variational posterial distribution of latent variable given by the inference model and the known prior distribution.

This decomposition is most often used in variational autoencoder literatures[2], where the first term on the right is a Monte Carlo expectation of the model likelihood with repect to the output of auxiliary inference network, and the second term on the right is a close-form as long as the variational posterior and prior are conjugate distributions, with the help of reparemterize trick.

A critism on ELBO from this point of view is that when the variational posterior is too flexiable thus could fit into any arbitrary distribution, the optimization on model parameters will stop. That is to say, if the KL divergence could easily reach to $$0$$ within the first few step of training, then the model likelihood may not be able to capture anything from the obsevation!

A possible solution is to impose a limit on the capacity of the latent information channel($$q_{\phi}(z|x)$$) and emphsize on learning statistically indepedent latent variable $$z$$. The simplised way to do so is to introduce a single hyperparameter $$\beta$$ which controls relative weights of KL divergence regularizer $$\mathbb{KL}(q_{\phi}(z|x)||p_{\theta}(z))$$, i.e.:


$$
\begin{align*}
\mathbb{ELBO}_{\beta VAE}
			  & = \mathbb{E}_{q_{\phi}(z|x)}log(p_{\theta}(x|z)) - \beta \mathbb{KL}(q_{\phi}(z|x)||p_{\theta}(z))
\end{align*}
$$
Initially $$\beta$$ is high, thus introducing strong constraints on the variational posterior around a simple factorial form of true prior, then a simulated annealing technique is used to gradually decrease $$\beta$$ to 1, thus return to the original ELBO[3].


## Reference
[1] Blei, David M., Alp Kucukelbir, and Jon D. McAuliffe. 2017. “Variational Inference: A Review for Statisticians.” *Journal of the American Statistical Association* 112 (518): 859–77. https://doi.org/10.1080/01621459.2017.1285773.

[2] Kingma, Diederik P., and Max Welling. 2014. “Auto-Encoding Variational Bayes.” *ArXiv:1312.6114 [Cs, Stat]*, May. http://arxiv.org/abs/1312.6114.

[3] Higgins, Irina, Loic Matthey, Arka Pal, Christopher Burgess, Xavier Glorot, Matthew Botvinick, Shakir Mohamed, and Alexander Lerchner. 2016. “Beta-VAE: Learning Basic Visual Concepts with a Constrained Variational Framework,” November. https://openreview.net/forum?id=Sy2fzU9gl.

[back](./)
