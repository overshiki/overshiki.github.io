<!-- +++
title = "Code blocks 2"
hascode = true
date = Date(2019, 3, 22)
rss = "A short description of the page which would serve as **blurb** in a `RSS` feed; you can use basic markdown here but the whole description string must be a single line (not a multiline string). Like this one for instance. Keep in mind that styling is minimal in RSS so for instance don't expect maths or fancy styling to work; images should be ok though: ![](https://upload.wikimedia.org/wikipedia/en/3/32/Rick_and_Morty_opening_credits.jpeg)"
+++ -->
@def title = "Decomposing ELBO"
<!-- @def tags = ["syntax", "code"] -->


# Decomposing ELBO: Generative model point of view

## The definition

Given the joint distribution between observe $x$  and latent variable $z$, provided by a generative model:

$$p(x, z)=p_{\theta}(x \mid z) p_{\theta}(z)$$

We start with the marginal distribution of $x$  for the model:

$$p(x)=\int p(x, z) d z=\int p_{\theta}(x \mid z) p_{\theta}(z) d z$$

A good generative model should maximize this marginal distribution, i.e. place most of the mass on observed data.

However, searching for $\theta$ which maximize this marginal distribution require integral over all the possible values of $z$, thus is inefficient.

A possible solution of this problem is to use an inference model to narrow the search region thus resulting in more efficient sampling

$$\begin{aligned} \log p(x) &=\log \int p_{\theta}(x \mid z) p_{\theta}(z) d z \\ &=\log \int \frac{p_{\theta}(x \mid z) p_{\theta}(z)}{q_{\phi}(z \mid x)} q_{\phi}(z \mid x) d z \\ &>=\int \log \left\{\frac{p_{\theta}(x \mid z) p_{\theta}(z)}{q_{\phi}(z \mid x)}\right\} q_{\phi}(z \mid x) d z \\ &=\mathbb{E} \mathbb{L} {\mathbb{B}} \mathbb{O} \end{aligned}$$

This yields the ELBO, i.e. evidence lower bound. The inequality is due to the Jensen's inequality for convex function(log function in this case), and only when $q_{\phi}(z \mid x) \stackrel{d}{=} p_{\theta}(z \mid x)$ the equality holds. The name of the evidence lower bound comes from another name of marginal distribution of observation $x \sim p(x)$: the evidence, thus its lower bound is the so called evidence lower bound.

## The intuitive decomposition of ELBO

### 1. Minimizing the KL divergence between posteriors

The first intuitive way to decompose ELBO is through analyzing how tight the lower bound is, i.e. how large the term $\log p(x)-\mathbb{E} \mathbb{L} \mathbb{B} \mathbb{O}$ is[1]:

it turns out

$$\begin{aligned} \log p(x)-\mathbb{E} \mathbb{L} \mathbb{B} \mathbb{O} &=\operatorname{logp}(x)-\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x \mid z) p_{\theta}(z)\right)+\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(q_{\phi}(z \mid x)\right) \\ &=\log p(x)-\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x, z)\right)+\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(q_{\phi}(z \mid x)\right) \\ &=\mathbb{E}_{q_{\phi}(z \mid x)}\left(\log p(x)-\log \left(p_{\theta}(x, z)\right)+\log \left(q_{\phi}(z \mid x)\right)\right) \\ &=\mathbb{E}_{q_{\phi}(z \mid x)}\left(-\log \frac{p_{\theta}(x, z)}{p(x)}+\log \left(q_{\phi}(z \mid x)\right)\right) \\ &=\mathbb{K} \mathbb{L}\left(q_{\phi}(z \mid x)\left|p_{\theta}(z \mid x)\right)\right.\end{aligned}$$

the intuition here is that the difference between the log evidence and the evidence lower bound is the KL divergence between the variational posterior and the true posterior of the hidden variable $z$. Thus maximizing the evidence lower bound will bring the KL divergence as small as possible, which means bringing the variational posterior as close as possible to the true posterior distribution. Note that since:

$$\mathbb{K} \mathbb{L}\left(q_{\phi}(z \mid x)\left|p_{\theta}(z \mid x)\right) \geq 0\right.$$

we have:

$$\log p(x) \geq \mathbb{E} \mathbb{L} \mathbb{B} \mathbb{O}$$

where equality holds if 

$q_{\phi}(z \mid x) \stackrel{d}{=} p_{\theta}(z \mid x)$

This is another point of view of how ELBO is related to the log evidence.

### 2. Maximizing the expected complete likelihood with entropy regularization

The second intuitive way to decompose ELBO is[1]:

$$\begin{aligned} \mathbb{E} \mathbb{L} \mathbb{B} \mathbb{O} &=\int \log \left(p_{\theta}(x \mid z) p_{\theta}(z)\right) q_{\phi}(z \mid x) d z-\int \log \left(q_{\phi}(z \mid x)\right) q_{\phi}(z \mid x) d z \\ &=\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x \mid z) p_{\theta}(z)\right)-\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(q_{\phi}(z \mid x)\right) \\ &=\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x \mid z) p_{\theta}(z)\right)+\mathbb{E} \operatorname{ntropy}\left(q_{\phi}(z \mid x)\right) \\ &=\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x, z)\right)+\mathbb{E} \operatorname{ntropy}\left(q_{\phi}(z \mid x)\right) \end{aligned}$$

The first term on the right is the expect complete likelihood in the literature of EM algorithm. The idea is that to find the maximum likelihood estimates in models with latent variables $z$, a computing heuristic alternates between a E-step and a M-step could be used. The E-step is to compute the likelihood under the expectation of auxiliary distribution $q_{\phi}(z \mid x)$, while the M-step is to optimize the expectation with respect to model parameters $\theta$, $\phi$. And through time, the auxiliary distribution would become closer and closer to true posterior distribution, given the fact that ELBO is equal to the log evidence when $q_{\phi}(z \mid x) \stackrel{d}{=} p_{\theta}(z \mid x)$ in the above section.

The second term on the right is the entropy of the auxiliary distribution. Maximizing this term means that we do not want the auxiliary distribution to degenerate too much, so that a broad enough search space is available.

### 3. Maximizing the reweighted likelihood with KL divergence regularization

The third intuitive way to decompose ELBO is:

$$\begin{aligned} \mathbb{E} \mathbb{L} \mathbb{B} \mathbb{O} &=\int \log \left(p_{\theta}(x \mid z)\right) q_{\phi}(z \mid x) d z+\int \log \left\{\frac{p_{\theta}(z)}{q_{\phi}(z \mid x)}\right\} q_{\phi}(z \mid x) d z \\ &=\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x \mid z)\right)-\mathbb{K} \mathbb{L}\left(q_{\phi}(z \mid x) \| p_{\theta}(z)\right) \end{aligned}$$

which means maximizing ELBO is equivalently to maximizing the likelihood of the generative model given observation when sampling from the inference model, and to minimize the KL divergence between the variational posterior distribution of latent variable given by the inference model and the known prior distribution.

This decomposition is most often used in variational autoencoder literatures[2], where the first term on the right is a Monte Carlo expectation of the model likelihood with respect to the output of auxiliary inference network, and the second term on the right is a close-form as long as the variational posterior and prior are conjugate distributions, with the help of reparemeterize trick.

A criticism on ELBO from this point of view is that when the variational posterior is too flexible thus could fit into any arbitrary distribution, the optimization on model parameters will stop. That is to say, if the KL divergence could easily reach to $0$ within the first few step of training, then the model likelihood may not be able to capture anything from the observation!

A possible solution is to impose a limit on the capacity of the latent information channel($q_{\phi}(z \mid x)$) and emphasize on learning statistically independent latent variable $z$. The simplest way to do so is to introduce a single hyperparameter $\beta$ which controls relative weights of KL divergence regularizer $\mathbb{K} \mathbb{L}\left(q_{\phi}(z \mid x) \| p_{\theta}(z)\right)$, i.e.:

$$\mathbb{E} \mathbb{L} \mathbb{B} \mathbb{O}_{\beta V A E}=\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x \mid z)\right)-\beta \mathbb{K} \mathbb{L}\left(q_{\phi}(z \mid x) \| p_{\theta}(z)\right)$$

Initially $\beta$ is high, thus introducing strong constraints on the variational posterior around a simple factorial form of true prior, then a simulated annealing technique is used to gradually decrease $\beta$ to 1, thus return to the original ELBO[3].

### 4. Minimizing Gibbs free energy to approach Helmholz free energy -
### -second law of thermodynamics

recall in section 1, maximizing ELBO is in equivalent to minimizing KL divergence between true posteriors and variational posteriors

$$\begin{aligned} \log p(x)-\mathbb{E} \mathbb{L} \mathbb{B} \mathbb{O} &=\operatorname{logp}(x)-\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x \mid z) p_{\theta}(z)\right)+\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(q_{\phi}(z \mid x)\right) \\ &=\log p(x)-\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(p_{\theta}(x, z)\right)+\mathbb{E}_{q_{\phi}(z \mid x)} \log \left(q_{\phi}(z \mid x)\right) \\ &=\mathbb{E}_{q_{\phi}(z \mid x)}\left(\log p(x)-\log \left(p_{\theta}(x, z)\right)+\log \left(q_{\phi}(z \mid x)\right)\right) \\ &=\mathbb{E}_{q_{\phi}(z \mid x)}\left(-\log \frac{p_{\theta}(x, z)}{p(x)}+\log \left(q_{\phi}(z \mid x)\right)\right) \\ &=\mathbb{K} \mathbb{L}\left(q_{\phi}(z \mid x)\left|p_{\theta}(z \mid x)\right)\right.\end{aligned}$$

consider a general case of KL divergence between $b\{x\}$ and $p\{x\}$:

$$\mathbb{KL}(b(\{x\}) \| p(\{x\})=\sum_{\{x\}} b(\{x\}) \ln \frac{b(\{x\})}{p(\{x\})}.$$



since probability distribution $$p\{x\} \in (0, 1)$$, an expression of  $p\{x\}$ into Bolzmann distribution is available:

$$p(\{x\})=\frac{1}{Z} e^{-E_p(\{x\}) / T}$$



where $Z = \int e^{-E_p(\{x\})/T} dx$ is the partition function

and  $E(\{x\}) = - T \cdot log(Z \cdot p_p(\{x\}))$ is easily holds for general cases.

In statistical physics literature, $E(\{x\})$ stands for potential energy the system obtained, and $T$ stands for temperature. Since temperature is just a parameter that defines a scale of units for the energy, for simplicity, we can choose our units so that $T=1$ 

Substitute Bolzmann distribution into the KL divergence, the KL divergence thus can be transformed into an energy based term:

$$\mathbb{KL}(b\{x\}|| p(\{x\})=\sum_{\{x\}} b(\{x\}) E_p(\{x\})+\sum_{\{x\}} b(\{x\}) \ln b(\{x\})+\ln Z.$$

The right hand side term consists an energy term, an entropy term, and a partition function term, and not surprisedly, The first two term is just equivalent to the Gibbs free energy in statistical physics literature:

$$G_p(b(\{x\})) \equiv \sum_{\{x\}} b(\{x\}) E_p(\{x\})+\sum_{\{x\}} b(\{x\}) \ln b(\{x\})=U(b\{x\})-S(b\{x\})$$

and the third term is equivalent to Helmholz free energy:

$F \equiv-\ln Z$

thus we have 

$$\mathbb{KL}(b(\{x\}) \| p(\{x\})= G_p(b(\{x\})) - F.$$

minimizing KL divergence is in equilivent to minimizing Gibbs free energy, and KL divergence reaches zero only if Gibbs free energy is minimized to be equal to Helmholz free energy.

This is the second law of thermodynamics!

and the ELBO could be decomposed into the following expression:

$$\mathbb{ELBO} = logp_{\theta}(x) +F_{q_{\phi}}(x) - G_{q_{\phi}, p_{\theta}}(x)$$

# Reference

[1] Blei, David M., Alp Kucukelbir, and Jon D. McAuliffe. 2017. “Variational Inference: A Review for Statisticians.” *Journal of the American Statistical Association* 112 (518): 859–77. https://doi.org/10.1080/01621459.2017.1285773.

[2] Kingma, Diederik P., and Max Welling. 2014. “Auto-Encoding Variational Bayes.” *ArXiv:1312.6114 [Cs, Stat]*, May. http://arxiv.org/abs/1312.6114.

[3] Higgins, Irina, Loic Matthey, Arka Pal, Christopher Burgess, Xavier Glorot, Matthew Botvinick, Shakir Mohamed, and Alexander Lerchner. 2016. “Beta-VAE: Learning Basic Visual Concepts with a Constrained Variational Framework,” November. https://openreview.net/forum?id=Sy2fzU9gl.