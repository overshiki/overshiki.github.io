---
layout: default
---

## Generative model point of view

Given the joint distribution between observe $$x$$ and latent variable $$z$$, provided by a generative model:
$$
p(x, z) = p_{\theta}(x|z) p_{\theta}(z)
$$

We start with the marginal distribution of $$x$$ for the model:
$$
p(x) = \int p(x, z) dz = \int p_{\theta}(x|z) p_{\theta}(z) dz
$$

A good generative model should maximize this marginal distribution, i.e. place most of the mass on observed data.
However, searching for $\theta$ which maximize this marginal distribution require integral over all the possible values of $$z$$, thus is inefficient.

A possible solution of this problem is to use an inference model $$q_{\phi}(z|x)$$ to narrow the search region thus resulting more effecient sampling
$$
\begin{align*}
logP(x) & = log \int p_{\theta}(x|z) p_{\theta}(z) dz \\
		& = log \int \frac{p_{\theta}(x|z) p_{\theta}(z)}{q_{\phi}(z|x)} q_{\phi}(z|x) dz \\
		&>= \int log \{\frac{p_{\theta}(x|z) p_{\theta}(z)}{q_{\phi}(z|x)} \} q_{\phi}(z|x) dz \\
		& = \mathbb{ELBO}
\end{align*}
$$


This yeilds the ELBO, i.e. evidence lower bound. The inequality is due to the Jensen's inequality for convex function(log function in this case).


The first intuitive way to decompose ELBO is:
$$
\begin{align*}
\mathbb{ELBO} &= \int log (p_{\theta}(x|z) p_{\theta}(z)) q_{\phi}(z|x) dz - \int log (q_{\phi}(z|x)) q_{\phi}(z|x) dz \\
&= \mathbb{E}_{q_{\phi}(z|x)} log(p_{\theta}(x|z) p_{\theta}(z)) - \mathbb{E}_{q_{\phi}(z|x)} log(q_{\phi}(z|x)) \\
&= \mathbb{E}_{q_{\phi}(z|x)} log(p_{\theta}(x|z) p_{\theta}(z)) + \mathbb{Entropy}(q_{\phi}(z|x))
\end{align*}
$$


The second intuitive way to decompose ELBO is:
$$
\begin{align*}
\mathbb{ELBO} & = \int log(p_{\theta}(x|z)) q_{\phi}(z|x) dz + \int log \{ \frac{p_{\theta}(z)}{q_{\phi}(z|x)} \} q_{\phi}(z|x) dz \\
			  & = \mathbb{E}_{q_{\phi}(z|x)}log(p_{\theta}(x|z)) - \mathbb{KL}(q_{\phi}(z|x)||p_{\theta}(z))
\end{align*}
$$
which means maximize ELBO is equivently to maximize the likelihood of the generative model given observation when sampling from the inference model, and to minimize the KL divergence between the variational posterial distribution of latent variable given by the inference model and the known prior distribution. 



[back](./)
