---
title: "Rethinking_week10_2019-07-17.Rmd"
author: "Stacey Harmer"
date: "7/10/2019"
output: 
  html_document: 
    keep_md: yes
---


## Video: 08-Jan 18: Model Comparison 
https://www.youtube.com/watch?v=gjrsYDJbRh0&feature=youtu.be

*Information Theory*
rigorous theory for reduction in uncertainty. 
information entropy - uncertainty is H, and is average log-prob of an event.  
potential of surprise is the flip side of entropy.  

Distance, divergence, is not symmetric

function DKL for homework.  minute 10.  Can think of divergence as potential for surprise (that is, difference between model and reality)
estimating divergence:  lppd, log-pointwise-predictive-density.  this is average across posterior. 
smaller is better on 'deviance' scale.  negative values are fine. 

magnitude of overfitting is difference between in and out samples. 

regularize - don't use flat priors!  this helps avoid overfitting.  (priors are regularizing)
in sample, more stringent prior does worse than flatter prior.  out of sample, the opposite. "flat priors are always bad!"

but with a large sample, priors hardly matter at all.

cross-validation and information criteria. can predict amount of overfitting, even when you don't have out of sample test data.  can score models on overfitting risk:
cross-validation
  leave out data, train on retained data, and then predict the omitted data.  a useful approximation is "importance sampling".  more useful is PSIS; use LOO function in rethinking.  only need  single posterior distribution.  
  
information criteria
  Akaike.  AIC.  estimate of K-L distance.  need gaussian posterior distribution. now, WAIC is used instead, as it doesn't assume gaussian posterior.  varince in posterior distirbution is key. 
  
avoid model selection.  instead, compare models to better understand them.  
model not necessarily useful for causal influence.


## Book: 7.2 (re-read if necessary), 

p 206 - log prob score. DEviance is like lppd, but multiplied by -2.
code 7.15 is on page 207.  lppd explained

## 7.3  Regularization
recall that standardization of predictors means SD = 1, and mean = 0.  

## 7.4 Predicting predictive accuracy
'for ordinary linear regressions with flat priors, the expected overfitting penalty is about twice the number of parameters'
AIC is reliable only when priors are flat, posterior is Gaussian, and n >> k 

WAIC - guesses out of sample K-L Divergence.  

code 7.20


```r
data(cars)
library(rethinking)
```

```
## Loading required package: rstan
```

```
## Loading required package: ggplot2
```

```
## Loading required package: StanHeaders
```

```
## rstan (Version 2.18.2, GitRev: 2e1f913d3ca3)
```

```
## For execution on a local, multicore CPU with excess RAM we recommend calling
## options(mc.cores = parallel::detectCores()).
## To avoid recompilation of unchanged Stan programs, we recommend calling
## rstan_options(auto_write = TRUE)
```

```
## Loading required package: parallel
```

```
## rethinking (Version 1.88)
```

```r
m <- quap(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b*speed,
    a <- dnorm(0,100),
    b ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data = cars
)

set.seed(94)
post <- extract.samples(m, n = 1000)
```
now get log-likelihood of each observation 

coe 7.21

```r
n_samples <- 1000

logprob <- sapply(1:n_samples,
                  function(s) {
                    mu <- post$a[s] + post$b[s]* cars$speed
                    dnorm(cars$dist, mu, post$sigma[s], log = TRUE)
                  })
cars
```

```
##    speed dist
## 1      4    2
## 2      4   10
## 3      7    4
## 4      7   22
## 5      8   16
## 6      9   10
## 7     10   18
## 8     10   26
## 9     10   34
## 10    11   17
## 11    11   28
## 12    12   14
## 13    12   20
## 14    12   24
## 15    12   28
## 16    13   26
## 17    13   34
## 18    13   34
## 19    13   46
## 20    14   26
## 21    14   36
## 22    14   60
## 23    14   80
## 24    15   20
## 25    15   26
## 26    15   54
## 27    16   32
## 28    16   40
## 29    17   32
## 30    17   40
## 31    17   50
## 32    18   42
## 33    18   56
## 34    18   76
## 35    18   84
## 36    19   36
## 37    19   46
## 38    19   68
## 39    20   32
## 40    20   48
## 41    20   52
## 42    20   56
## 43    20   64
## 44    22   66
## 45    23   54
## 46    24   70
## 47    24   92
## 48    24   93
## 49    24  120
## 50    25   85
```

code 7.22, p 218

```r
n_cases <- nrow(cars)
lppd <- sapply(1:n_cases, function(i) log_sum_exp(logprob[i,]) - log(n_samples))

# compare to main function
```


```r
lppd(m, n = 1e4) # yep, looks very similar.
```

```
##  [1] -3.597382 -3.913660 -3.639968 -3.922776 -3.546829 -3.702177 -3.567199
##  [8] -3.574774 -3.934979 -3.736337 -3.537134 -4.203795 -3.782244 -3.610596
## [15] -3.528091 -3.680691 -3.518579 -3.518579 -3.949442 -3.890438 -3.523348
## [22] -4.917930 -8.301403 -4.795210 -4.185533 -3.959921 -4.018894 -3.597947
## [29] -4.352384 -3.760569 -3.518752 -3.871578 -3.540859 -4.951456 -6.093510
## [36] -4.752065 -3.867513 -3.850480 -5.803230 -3.996562 -3.752586 -3.595929
## [43] -3.548641 -3.556262 -4.486016 -3.666146 -4.160135 -4.242840 -8.265733
## [50] -3.599986
```
code 7.23, 7.24, 7.25

```r
pWAIC <- sapply(1:n_cases, function(i) var(logprob[i ,]))
-2*(sum(lppd) - sum(pWAIC))  #423.3
```

```
## [1] 423.2977
```

```r
waic_vec <- -2*(lppd - pWAIC)
sqrt(n_cases*var(waic_vec))  #17.82;  this is SE of WAIC
```

```
## [1] 17.79755
```

```r
WAIC(m, n = 1000)
```

```
## [1] 423.4488
## attr(,"lppd")
## [1] -206.9389
## attr(,"pWAIC")
## [1] 4.785459
## attr(,"se")
## [1] 17.65518
```

```r
?WAIC
```

Huh, that was somewhat opaque.  finished on p 221

## Problems , p 234

### '6'M1

compare definitions of AIC, DIC, and WAIC.  which is most general? what are assumptions?

##### AIC = Dtrain + 2p = -2lppd + 2p
that is, it equals divergence of model and real target plus 2 * number of parameters

AIC is special case of WAIC; it requires 
* flat priors 
* it uses MAP estimates instead of the entire posterior (but lppd is the Baeysian version, as it uses entire posterior dist); he says posterior distribution is apporximately multivariate Gaussian
* sample size N >> number of parameters k

##### DIC - not defined formally

DIC assumes
* posterior is multivariate Gaussian
* N >> k

##### WAIC is the most general

WAIC is similar to AIC in that it = -2 ( lppd - pWAIC)

where pWAIC is a penalty score that proportional to variance in posterior predictions.  see p 215

* makes no assumptions about shape of posterior
* doesnt try to estimate cross-validation score, but instead out-of-sample K-L Divergence

### '6'M2

explain difference between model selection and model averaging.  what information is lost in each?

selection: described on p 221 (section 7.5).  this is choosing model with lowest criterion value (such as WAIC) and discarding the others. This process discards information about relative model accuracy, which can be useful as it tellls us how confident we are in the different models.  Also, the 'best' model may not tell us about causation.

averaging:  see p 225.  not covered in book, but a family of methods to combine predictions of multilple models

### '6'M3

When comparing models with IC, why must all models be fit to exactly same observations?  What if differnet # of observations were used?

This seemms intuitively obvious.  both the number of observations and the actual values themselves will affect model fitting.  to use differnent # of observations would be an apples and oranges type situation

test this:

OK, I'll use his examples


```r
sppnames <- c( "afarensis","africanus","habilis","boisei", 
"rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

# first, fit simple model to a dataset with 30 measurements
set.seed(15)
x1 <- runif(30)
set.seed(2)
x2 <- runif(30)

data.30 <- data.frame(y = x1, pred = x2)

m6M3.30 <- quap(
  alist(
    y ~ dnorm(mu, 1),
    mu <- a + b*pred, 
    a ~ dnorm(0.5, 1), 
    b ~ dnorm(0,10)
  ), data = data.30
)

#now a more complex model, with a smaller dataset

set.seed(15)
z1 <- runif(20)
set.seed(2)
z2 <- runif(20)

data.20 <- data.frame(y = z1, pred = z2)

m6M3.20 <- quap(
  alist(
    y ~ dnorm(mu, 1),
    mu <- a + b[1]*pred + b[2]*pred^2, 
    a ~ dnorm(0.5, 1), 
    b ~ dnorm(0,10)
  ), data = data.20, start = list(b=rep(0,2))
)

# now compare
#  set.seed(113)
#  compare(m6M3.30, m6M3.20)

# just throws an error

WAIC(m6M3.30)
```

```
## [1] 59.79486
## attr(,"lppd")
## [1] -29.66895
## attr(,"pWAIC")
## [1] 0.2284773
## attr(,"se")
## [1] 0.432553
```


```r
WAIC(m6M3.20)
```

```
## [1] 41.05372
## attr(,"lppd")
## [1] -20.16525
## attr(,"pWAIC")
## [1] 0.3616053
## attr(,"se")
## [1] 0.5001137
```

### '6'M4
What happens to the effective number of parameters, as measured by DIC or WAIC, as a prior
becomes more concentrated? Why? Perform some experiments, if you are not sure.

I think that more concentrated means more specific (less flat).  the effective number of parameters will decrease, because the strong prior will automatically constrain the values of the 'unnecessary' parameters.

### '6'M5

Why do informative priors reduce overfitting?

Kind of explained above.  If we have a strong priors, the data will have less effect on the posterior distribution. 

### '6'M6

Why to informative priors result in underfitting?

Also already touched on.  If priors are very strong, the data just can't sway the outcome, the posterior, very much.  This can lead to information being discarded.


"6"M1 - "6"M6
