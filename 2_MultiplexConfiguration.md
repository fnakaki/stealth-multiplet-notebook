2. Multiplex configuration
================

``` r
library(reshape2)
library(ggplot2)
```

## A multiple sample pooling configuration

When multiple $s$ samples are individually labelled and pooled, the
probability of the four categories of the multiplets can be expressed as
follows:

$$
\begin{aligned}
p_{\textrm{HS}} &=e^{-\lambda}\Big(\sum_{i=1}^{s}e^{r_ia_i\lambda}-\bar{a}{\lambda}-s\Big) \\
p_{\textrm{PS}} &=(e^{-\bar{a}\lambda}- e^{-\lambda})\Big(\sum_{i=1}^{s}e^{r_ia_i\lambda}-s\Big)\\
p_{\textrm{Mu}} &=1-e^{-\bar{a}\lambda}\Big(\sum_{i=1}^{s}e^{r_ia_i\lambda}  - s + 1 \Big)\\
p_{\textrm{Un}} &=e^{-\lambda} (e^{(1-\bar{a})\lambda} - (1-\bar{a}){\lambda} -1) \\
\end{aligned}
$$

For simplicity, we assume that the all samples are labelled with the
equal labelling efficiency and pooled with the equal proportion. In this
case, $a_ir_i=\bar{a}/s$, and the probability of the four cateories
above are

$$
\begin{aligned}
p_{\textrm{HS}} &=se^{-\lambda}(e^{\frac{\bar{a}\lambda}{s}}-1)-\bar{a}{\lambda}e^{-\lambda}\\
p_{\textrm{PS}} &=s(e^{-\bar{a}\lambda}-e^{-\lambda})(e^{\frac{\bar{a}\lambda}{s}}-1) \\
p_{\textrm{Mu}} &=1-e^{-\bar{a}\lambda} \big(s(e^{\frac{\bar a\lambda}{s}} - 1) +1 \big) \\
p_{\textrm{Un}} &=e^{-\lambda} (e^{(1-\bar{a})\lambda} - (1-\bar{a}){\lambda} -1) \\
\end{aligned}
$$

To examine each proportion, first defining the functions for each
category of multiplets.

``` r
Poisson <- function(k, lambda){
  as.numeric(
    ((lambda^k)*exp(-lambda))/factorial(k)
  )
}

multiplet <- function(lambda){
  as.numeric(
    1-Poisson(0, lambda)-Poisson(1, lambda)
  )
}

HS <- function(a, lambda, s){
  as.numeric(
    s*exp(-lambda)*(exp((a*lambda)/s) - 1) - a*lambda*exp(-lambda)
  )
}

PS <- function(a, lambda, s){
  as.numeric(
    s*(exp(-a*lambda) - exp(-lambda))*(exp((a*lambda)/s) - 1)
  )
}

Mu <- function(a, lambda, s){
  as.numeric(
    1 - exp(-lambda*a)*(s*exp((a*lambda)/s) - s + 1 )
  )
}

Un <- function(a, lambda){
  as.numeric(
    exp(-a*lambda) - (1-a)*lambda*exp(-lambda) - exp(-lambda)
  )
}
```

Then, visualising with area charts by fixing two of the parameters.

``` r
par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))
multiplet_palette <- c("#FDE725", "#35B779", "#31688E", "#440154")

s <- 5
lambda <- 0.3
a <- seq(0,1,by=0.01)

HS_frac <- HS(a, lambda, s)/multiplet(lambda)
PS_frac <- PS(a, lambda, s)/multiplet(lambda)
Mu_frac <- Mu(a, lambda, s)/multiplet(lambda)
Un_frac <- Un(a, lambda)/multiplet(lambda)
#HS_frac + PS_frac + Mu_frac + Un_frac

df <- as.data.frame(cbind(a, HS_frac,PS_frac,Mu_frac,Un_frac))
colnames(df) <- c("a", "Homogeneous stealth", "Partial stealth", "Multilabelled", "Unlabelled")

gg <- melt(df, id.vars = c("a"))
p <- ggplot(gg, aes(x=a, y=value, group=variable, fill=variable)) + 
  geom_area(position="fill", alpha=0.9) +
  theme_bw() +
  theme(aspect.ratio = 1,
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()
        ) +
  labs(x="Labelling efficiency", y="Fraction", title="s=5, lambda=0.3") +
  scale_fill_manual(values = multiplet_palette) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

p
#######################################
s <- seq(1,100,by=1)
lambda <- 0.3
a <- 0.9

HS_frac <- HS(a, lambda, s)/multiplet(lambda)
PS_frac <- PS(a, lambda, s)/multiplet(lambda)
Mu_frac <- Mu(a, lambda, s)/multiplet(lambda)
Un_frac <- Un(a, lambda)/multiplet(lambda)
#HS_frac + PS_frac + Mu_frac + Un_frac

df <- as.data.frame(cbind(s, HS_frac,PS_frac,Mu_frac,Un_frac))
colnames(df) <- c("s", "Homogeneous stealth", "Partial stealth", "Multilabelled", "Unlabelled")

gg <- melt(df, id.vars = c("s"))
p <- ggplot(gg, aes(x=s, y=value, group=variable, fill=variable)) + 
  geom_area(position="fill", alpha=0.9) +
  theme_bw() +
  theme(aspect.ratio = 1,
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()
  ) +
  labs(x="Number of samples", y="Fraction", title="a=0.9, lambda=0.3") +
  scale_fill_manual(values = multiplet_palette) +
  scale_x_continuous(limits = c(1, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

p
#######################################
s <- 5
lambda <- seq(0.01,1,by=0.01)
a <- 0.9

HS_frac <- HS(a, lambda, s)/multiplet(lambda)
PS_frac <- PS(a, lambda, s)/multiplet(lambda)
Mu_frac <- Mu(a, lambda, s)/multiplet(lambda)
Un_frac <- Un(a, lambda)/multiplet(lambda)
#HS_frac + PS_frac + Mu_frac + Un_frac

df <- as.data.frame(cbind(lambda, HS_frac,PS_frac,Mu_frac,Un_frac))
colnames(df) <- c("lambda", "Homogeneous stealth", "Partial stealth", "Multilabelled", "Unlabelled")

gg <- melt(df, id.vars = c("lambda"))
p <- ggplot(gg, aes(x=lambda, y=value, group=variable, fill=variable)) + 
  geom_area(position="fill", alpha=0.9) +
  theme_bw() +
  theme(aspect.ratio = 1,
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()
  ) +
  labs(x=expression(lambda), y="Fraction", title="a=0.9, s=5") +
  scale_fill_manual(values = multiplet_palette) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

p
```

<img src="2_MultiplexConfiguration_files/figure-gfm/Stealth Multiplet fraction-1.png" width="33%" /><img src="2_MultiplexConfiguration_files/figure-gfm/Stealth Multiplet fraction-2.png" width="33%" /><img src="2_MultiplexConfiguration_files/figure-gfm/Stealth Multiplet fraction-3.png" width="33%" />

Next, exploring *TSR*. under the condition of $a_ir_i=\bar{a}/s$, *TSR*
is expressed as

$$
TSR =\frac{\bar a{\lambda}e^{-\lambda(1-\bar a)}}{\sum_{i=1}^{s}e^{r_ia_i\lambda}-s}
=\frac{\bar a{\lambda}e^{-\lambda(1-\bar a)}}{s (e^{\frac{\bar a\lambda}{s}}-1)}
$$

*TSR* plotted against labelling efficiency under the different sample
numbers multiplexed.

``` r
TSR_eq <- function(a, lambda, s){
  as.numeric(
    (a*lambda*exp(-lambda*(1-a)))/
      (s*(exp((a*lambda)/s)-1))
  )
}
```

``` r
par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))
lambda <- 0.3
curve(TSR_eq(a, lambda, 1), xname="a", xlab="Labelling efficiency", ylab=expression(italic("TSR")), lty=1, xlim = c(0.01,1.0), ylim = c(0.6,1.00), cex.lab=0.9, cex.axis=0.8, add=F, col="#F8766D")
curve(TSR_eq(a, lambda, 2), xname="a", add=T, lty=5)
curve(TSR_eq(a, lambda, 5), xname="a", add=T, lty=6)
curve(TSR_eq(a, lambda, 10), xname="a", add=T, lty=2)
curve(TSR_eq(a, lambda, 50), xname="a", add=T, lty=3)
curve(TSR_eq(a, lambda, 1000), xname="a", add=T, lty=1, col="#00BFC4")
legend("topleft", inset = 0.02, title=expression(lambda~"= 0.3"), legend=NA, box.lty=0)
legend("bottomright", inset = 0.02, title="Number of samples", legend=c("1", "2", "5","10","50","1000"), lty = c(1,5,6,2,3,1), ncol=1, box.lty=0, col=c("#F8766D","black","black","black","black","#00BFC4"), cex=1)
```

![](2_MultiplexConfiguration_files/figure-gfm/TSR%20and%20lambda-1.png)<!-- -->

The frequencies of homogeneous and partial stealth multiplets among the
cell-droplets under various conditions. Note that they are the
frequencies not in the monolabelled but in all cell-droplets.

``` r
par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))
sn <- c(1,2,3,4,5,10,20,50,100)

plot(sn, PS(0.7, 0.5, sn), log="x", ylim=c(0,0.1),xlab="Number of samples", ylab="Ratio to monolabled droplets", pch=16)
points(sn, HS(0.7, 0.5, sn), pch=1)
points(sn, PS(0.9, 0.5, sn), pch=15)
points(sn, HS(0.9, 0.5, sn), pch=0)
points(sn, PS(0.9, 0.3, sn), pch=18)
points(sn, HS(0.9, 0.3, sn), pch=5)
points(sn, PS(0.99, 0.3, sn), pch=17)
points(sn, HS(0.99, 0.3, sn), pch=2)
legend("topright", inset = 0.02, title="Type II multiplet", legend=c(expression("a = 0.70, "~lambda~" = 0.5"), expression("a = 0.90, "~lambda~" = 0.5"), expression("a = 0.90, "~lambda~" = 0.3"),expression("a = 0.99, "~lambda~" = 0.3")), pch = c(16,15,18,17), ncol=1, box.lty=0, cex=0.7)
legend("topright", inset = c(0.02, 0.25), title="Type I multiplet", legend=c(expression("a = 0.70, "~lambda~" = 0.5"), expression("a = 0.90, "~lambda~" = 0.5"), expression("a = 0.90, "~lambda~" = 0.3"),expression("a = 0.99, "~lambda~" = 0.3")), pch = c(1,0,5,2), ncol=1, box.lty=0, cex=0.7)
```

![](2_MultiplexConfiguration_files/figure-gfm/HS%20and%20PS%20under%20different%20conditions-1.png)<!-- -->

Lastly, we examined a case of multiplexing samples with diffierent
labelling efficiency, particularly, when one sample was poorly labelled.
*TSR* is plotted against a labelling efficiency of one sample under
various sample numbers. The labelling efficiencies of the rest are 0.95.

``` r
TSRbscurve <- function(bs, a, lambda, s){
  ave <- (bs + a*(s-1))/s
  as.numeric(
    (ave*lambda*exp(-lambda*(1-ave)))/
      ( (s-1)*(exp((a/s)*lambda)-1) + exp((bs/s)*lambda)-1 )
  )
}

par(mar=c(3.1,3.1,1.1,1.1), mgp=c(2,0.8,0))

curve(TSRbscurve(bs, 0.95, 0.3, 2), xname="bs", xlab="Labelling efficiency of one sample", ylab=expression(italic("TSR")), lty=5, xlim = c(0,1), ylim = c(0.6,1), cex.lab=0.9, cex.axis=0.8, add=F, col="black", main=expression(lambda~"= 0.3, Labelling effcienciencies of the others=0.95"))
curve(TSRbscurve(bs, 0.95, 0.3, 5), xname="bs", add=T, lty=6)
curve(TSRbscurve(bs, 0.95, 0.3, 10), xname="bs", add=T, lty=2)
curve(TSRbscurve(bs, 0.95, 0.3, 20), xname="bs", add=T, lty=4)
curve(TSRbscurve(bs, 0.95, 0.3, 50), xname="bs", add=T, lty=3)
legend("bottomright", inset = 0.02, title = "Number of samples", legend=c("2","5","10","20","50"), lty=c(5,6,2,4,3), seg.len=3, ncol=1, box.lty=0, cex=0.7)
```

![](2_MultiplexConfiguration_files/figure-gfm/Effect%20of%20one%20bad%20sample-1.png)<!-- -->
