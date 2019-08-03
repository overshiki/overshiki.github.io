---
layout: default
---

## Still under development

Given the joint distribution between observe $$x$$ and latent variable $$z$$, provided by a generative model:
$$
p(x, z) = p_{\theta}(x|z) p_{\theta}(z)
$$

We start with the marginal distribution of $$x$$ for the model:
$$
p(x) = \int p(x, z) dz = \int p_{\theta}(x|z) p_{\theta}(z) dz
$$

A good generative model should maximize this marginal distribution, i.e. place most of the mass on observed data.
However, searching for $\theta$ which maximize this marginal distribution require integral over all the possible values of $z$, thus is inefficient.

A possible solution of this problem is to use an inference model $$q_{\phi}(z|x)$$ to narrow the search region thus resulting more effecient sampling
$$
\begin{align*}
logP(x) & = log \int p_{\theta}(x|z) p_{\theta}(z) dz \\
		& = log \int \frac{p_{\theta}(x|z) p_{\theta}(z)}{q_{\phi}(z|x)} q_{\phi}(z|x) dz \\
		&>= \int log \{ \frac{p_{\theta}(x|z) p_{\theta}(z)}{q_{\phi}(z|x)} \} q_{\phi}(z|x) dz \\
		& = \int log(p_{\theta}(x|z)) q_{\phi}(z|x) dz + \int log \{ \frac{p_{\theta}(z)}{q_{\phi}(z|x)} \} q_{\phi}(z|x) dz \\
\end{align*}
$$

$$\mathbb{E}$$

[back](./)
