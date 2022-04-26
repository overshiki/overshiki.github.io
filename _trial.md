


\begin{eqnarray}
    \begin{aligned}
      o = \begin{pmatrix}
        2 & 1 & 1
      \end{pmatrix} & \quad &  
      T_a = \begin{pmatrix}
        1 & \frac{1}{3} & 1 \\ 
        0 & 0 & 0 \\
        0 & 0 & 0 \\ 
      \end{pmatrix} & \quad &
      T_b = \begin{pmatrix}
        1 & 0 & 0 \\ 
        0 & 0 & 3 \\
        0 & \frac{1}{e^{14i\pi}} & 0 \\ 
      \end{pmatrix} & \quad &
    \end{aligned}
\end{eqnarray}

@def hasplotly = true


```julia:pyplot1
using PyPlot
figure(figsize=(8, 6))
x = range(-2, 2, length=500)
for α in 1:5
    plot(x, sinc.(α .* x))
end
savefig(joinpath(@OUTPUT, "sinc.svg")) # hide
```

\fig{sinc}


Where `is_commute` and `is_cancel` are functions provided in `src/gate.jl` to determine if two gates are commute and if they could be cancelled out. 
Currently, `is_commute` considers two cases:
  - gate `a` and gate `b` do not have common `Loc` or `cLoc`
  - gate `a/b` is a `Z|S|T` gate, and gate `b/a` is a `CNOT` gate, where `cLoc` of `b/a` has the same index with `Loc` of `a/b`
`is_cancel` considers two cases:
  - gate `a` and gate `b` are identical and they belong to unitary & Hermitian gate
  - gate `a/b` is the dagger version of gate `b/a`