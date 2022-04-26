### Brief Introduction: 
Variational ansatz-based imaginary time evolution(VITE), or imaginary-time Ansatz optimization(as refered in [Ref](https://www.nature.com/articles/s41567-019-0704-4)) is a method to project the trajectory of imaginary time evolution onto the ansatz manifold. The basic idea is to solve the imaginary time evolution equation(McLachlan’s variational principle): $$\frac{\partial |\psi(\tau)\rangle}{\partial \tau} = -(H - E_{\tau})|\psi(\tau)\rangle$$

Where $$E_{\tau} = \langle \psi(\tau)|H|\psi(\tau) \rangle$$ 

What makes it different from conventional imaginary time evolution method is that it uses the function space of ansatz $\theta \mapsto |\psi_{\theta}(\tau)\rangle$ as the solution space. By doing so, the evolution of state vector $|\psi \rangle$ is transformed into evolution of parameters $\theta$. For more details, please refer to the Appendix of the [paper](https://www.nature.com/articles/s41534-019-0187-2).

In short, the VITE is to obey the following update rule:
$$\theta(\tau + \delta \tau) = \theta(\tau) + A^{-1}(\tau) \cdot C(\tau)\delta \tau$$

Where 

$$A_{ij}(\tau) = \mathrm{Real}\left(\frac{\partial \langle \phi(\tau)|}{\partial \theta_i} \frac{\partial |\phi(\tau) \rangle}{\partial \theta_j}\right)$$
$$C_{i}(\tau) = -\mathrm{Real} \left(\frac{\partial \langle \phi(\tau)|}{\partial \theta_i} H  |\phi(\tau) \rangle \right)$$

This is actually equivalent to applying a preconditioner $A^{-1}$ to the gradient descent method:
$$\theta(\tau + \delta \tau) = \theta(\tau) - A^{-1}(\tau) \cdot \mathrm{grad}(\theta(\tau))\delta \tau$$

Recent [propose](https://arxiv.org/pdf/1909.02108.pdf) of quantum natural gradient descent(QNGD) also connects to the precontitioner $A$ of VITE. In QNGD, the update rule is as follows:

$$\theta(\tau + \delta \tau) = \theta(\tau) - F^{-1}(\tau) \cdot \mathrm{grad}(\theta(\tau))\delta \tau$$

Where $$F_{ij}(\tau) = 4 \mathrm{Real}\left(\frac{\partial \langle \phi(\tau)|}{\partial \theta_i} \frac{\partial |\phi(\tau) \rangle}{\partial \theta_j} - \frac{\partial \langle \phi(\tau)|}{\partial \theta_i} |\phi(\tau)\rangle \langle \phi(\tau)|  \frac{\partial |\phi(\tau)\rangle}{\partial \theta_j}\right)$$

We see that both VITE and QNGD can be seen as cases for applying Hessian-like preconditioner to the gradient descent method.

### Calculation of Hessian-like matrix
The calculation of the form $\mathrm{Real}\left(\frac{\partial \langle \phi(\tau)|}{\partial \theta_i} \frac{\partial |\phi(\tau) \rangle}{\partial \theta_j}\right)$, from the viewpoint of the physical implementation of quantum computation, requires doing hadamard test at both side of the circuit, i.e. the left circuit with parameter $\theta_i$ and the right circuit with parameter $\theta_j$. From the viewpoint of the numerical simulation, it becomes a little bit interesting, since forward mode differetiation(including forward mode numerical differetiation method and hadamard test method) has a complexity of $O(N^2)$, which is not efficient enough. Since most numerical simulator use a more efficient(complexity of $O(N)$) differetiation method, i.e. reverse mode or backpropogation, here is the question that, if reverse mode could be used in the calculation of the Hessian-like matrix defined above.

It is actually possible, just do the hybrid mode!

In a forward-reverse hybrid mode, we firstly do forward-mode symbolic rewriting of the left circuit, such that the new circuit has the $\langle A|B\rangle$ form:
$$ \langle A|B\rangle = \mathrm{Real}\left( \frac{\partial \langle \phi(\tau)| \phi(\tau)  \rangle}{\partial \theta_i} \right)$$
and then using backpropogation to calculate the gradient with respect to each $\theta_j$ in the right circuit. That's it!

The only thing left is to proof that this is mathematically solid, that is, we need to prove:

$$
\mathrm{Real}\left(\frac{\partial \langle \phi(\tau)|}{\partial \theta_i} \frac{\partial |\phi(\tau) \rangle}{\partial \theta_j}\right) = \partial \mathrm{Real}\left( \frac{\partial \langle \phi(\tau)| \phi(\tau)  \rangle}{\partial \theta_i} \right) / \partial \theta_j
$$

It is actually straightforward, let's firstly define 
$$f(\theta_j) = \frac{\partial \langle \phi(\tau)| \phi(\tau)  \rangle}{\partial \theta_i}$$
without of loss of generality, we can also say:
$$
f(\theta_j) = u(\theta_j) + i v(\theta_j)
$$
where $u$ and $v$ are real function, and $\theta_j$ is a real variable. Thus we easily have:
$$
\frac{\partial f(\theta_j)}{\partial \theta_j} = \frac{\partial u(\theta_j)}{\partial \theta_j} + i \frac{\partial v(\theta_j)}{\partial \theta_j}
$$
thus
$$
\mathrm{Real}\left( \frac{\partial f(\theta_j)}{\partial \theta_j}\right) = \frac{\partial u(\theta_j)}{\partial \theta_j} = \frac{\partial \mathrm{Real}\left( f(\theta_j) \right) }{\partial \theta_j}
$$
expanding $f$ into $\frac{\partial \langle \phi(\tau)| \phi(\tau)  \rangle}{\partial \theta_i}$ we imediately have 
$$
\mathrm{Real}\left(\frac{\partial \langle \phi(\tau)|}{\partial \theta_i} \frac{\partial |\phi(\tau) \rangle}{\partial \theta_j}\right) = \partial \mathrm{Real}\left( \frac{\partial \langle \phi(\tau)| \phi(\tau)  \rangle}{\partial \theta_i} \right) / \partial \theta_j
$$