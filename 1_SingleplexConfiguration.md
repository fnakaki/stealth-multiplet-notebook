1. Singleplex configuration
================

## A hypothetical singleplex experiment

In the imaginary singleplex configuration, the probability of the
homogeneous stealth multiplet $p_{HS}$ and the partial stealth multiplet
$p_{PS}$ can be expressed as follows:

$$
\begin{aligned}
p_{HS} &= e^{-\lambda}(e^{a\lambda} - a{\lambda}-1) \\
p_{PS} &= 1-e^{-\lambda}(e^{a\lambda}+e^{(1-a)\lambda}-1)  \\
\end{aligned}
$$

To see their behaviour, defining the functions and plotting against
labelling efficiency.

``` r
sinplex_HS <- function(a, lambda){
  as.numeric(
    exp(-lambda)*(exp(a*lambda) - a*lambda - 1)
  )
}

sinplex_PS <- function(a, lambda){
  as.numeric(
    1-exp(-lambda)*(exp(a*lambda)+exp((1-a)*lambda)-1)
  )
}
```

``` r
par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))
curve(sinplex_HS(0.99, lambda), xname=expression(lambda), ylab="Frequency", lty=1, xlim = c(0,0.5), ylim = c(0.00,0.1), cex.lab=0.9, cex.axis=0.8, col="#F8766D", add=F)
curve(sinplex_HS(0.95, lambda), xname=expression(lambda), lty=5, col="#F8766D", add=T)
curve(sinplex_HS(0.90, lambda), xname=expression(lambda), lty=2, col="#F8766D", add=T)
curve(sinplex_HS(0.80, lambda), xname=expression(lambda), lty=4, col="#F8766D", add=T)
curve(sinplex_HS(0.70, lambda), xname=expression(lambda), lty=3, col="#F8766D", add=T)

curve(sinplex_PS(0.99, lambda), xname=expression(lambda), col="#00BFC4", add=T)
curve(sinplex_PS(0.95, lambda), xname=expression(lambda), lty=5, col="#00BFC4", add=T)
curve(sinplex_PS(0.90, lambda), xname=expression(lambda), lty=2, col="#00BFC4", add=T)
curve(sinplex_PS(0.80, lambda), xname=expression(lambda), lty=4, col="#00BFC4", add=T)
curve(sinplex_PS(0.70, lambda), xname=expression(lambda), lty=3, col="#00BFC4", add=T)

legend("topleft", inset = 0.02, legend=c("Type HS","Type PS"), lty=c(1,1), seg.len=3, ncol=1, box.lty=0, cex=0.7, col = c("#F8766D","#00BFC4"))
legend("topleft", inset = c(0.02,0.15), title = "Labelling efficiency", legend=c("0.99","0.95","0.90","0.80","0.70"), lty=c(1,5,2,4,3), seg.len=3, ncol=1, box.lty=0, cex=0.7)
```

![](1_SingleplexConfiguration_files/figure-gfm/Stealth%20Multiplet%20frequency%20and%20lambda-1.png)<!-- -->

Also exploring the ratio of these two types of the stealth multiplets.

``` r
par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))
curve(sinplex_PS(0.99, lambda)/sinplex_HS(0.99, lambda), xname=expression(lambda), ylab=expression(italic("P"[PS]~"/P"[HS])), lty=1, xlim = c(0,0.5), ylim = c(0.00,1), cex.lab=0.9, cex.axis=0.8, add=F)
curve(sinplex_PS(0.95, lambda)/sinplex_HS(0.95, lambda), xname=expression(lambda), lty=5, add=T)
curve(sinplex_PS(0.90, lambda)/sinplex_HS(0.90, lambda), xname=expression(lambda), lty=2, add=T)
curve(sinplex_PS(0.80, lambda)/sinplex_HS(0.80, lambda), xname=expression(lambda), lty=4, add=T)
curve(sinplex_PS(0.70, lambda)/sinplex_HS(0.70, lambda), xname=expression(lambda), lty=3, add=T)
legend("topleft", inset = c(0.02,0.2), title = "Labelling efficiency", legend=c("0.99","0.95","0.90","0.80","0.70"), lty=c(1,5,2,4,3), seg.len=3, ncol=1, box.lty=0, cex=0.7)
```

![](1_SingleplexConfiguration_files/figure-gfm/Ratio%20of%20Type%20HS%20and%20Type%20PS-1.png)<!-- -->

Next, the true singlet ratio (*TSR*) among apparently monolabelled
cell-droplets are

$$
TSR =\frac{aP(1)}{aP(1) + p_{HS} + p_{PS}}=\frac{a{\lambda}e^{-\lambda}}{1 - e^{-a\lambda}}\\
$$

Plotting *TSR* against $\lambda$ and labelling efficiency.

``` r
sinplex_TSR <- function(a, lambda){
  as.numeric(
    (a*lambda*exp(-lambda))/(1-exp(-a*lambda))
  )
}
```

``` r
par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))
curve(sinplex_TSR(0.99, lambda), xname=expression(lambda), ylab=expression(italic("TSR")), lty=1, xlim = c(0,0.5), ylim = c(0.7,1.0), cex.lab=0.9, cex.axis=0.8, add=F)
curve(sinplex_TSR(0.95, lambda), xname=expression(lambda), lty=5, add=T)
curve(sinplex_TSR(0.90, lambda), xname=expression(lambda), lty=2, add=T)
curve(sinplex_TSR(0.80, lambda), xname=expression(lambda), lty=4, add=T)
curve(sinplex_TSR(0.70, lambda), xname=expression(lambda), lty=3, add=T)
legend("bottomleft", title = "Labelling efficiency", legend=c("0.99","0.95","0.90","0.80","0.70"), lty=c(1,5,2,4,3), seg.len=3, ncol=1, box.lty=0, cex=0.7)
```

![](1_SingleplexConfiguration_files/figure-gfm/TSR%20and%20lambda-1.png)<!-- -->

``` r
par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))
curve(sinplex_TSR(a, 0.1), xname="a", xlab="Labelling efficiency", ylab=expression(italic("TSR")), lty=1, xlim = c(0,1.0), ylim = c(0.6,1.00), cex.lab=0.9, cex.axis=0.8, add=F)
curve(sinplex_TSR(a, 0.2), xname="a", lty=5, add=T)
curve(sinplex_TSR(a, 0.3), xname="a", lty=2, add=T)
curve(sinplex_TSR(a, 0.4), xname="a", lty=4, add=T)
curve(sinplex_TSR(a, 0.5), xname="a", lty=3, add=T)
legend("bottomright", inset = 0.02, title = expression(lambda), legend=c("0.1","0.2","0.3","0.4","0.5"), lty=c(1,5,2,4,3), seg.len=3, ncol=1, box.lty=0, cex=0.7)
```

![](1_SingleplexConfiguration_files/figure-gfm/TSR%20and%20labelling%20efficiency-1.png)<!-- -->
