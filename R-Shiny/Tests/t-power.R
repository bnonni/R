"***********************

      T-Power Test 

************************

___________________________________________________________________________________________________

pwr.t.test() -> Power calculations for t-tests of means (one sample, two samples and paired samples)
____________________________________________________________________________________________________

Description
    Compute power of tests or determine parameters to obtain target power (similar to power.t.test).

Usage
    pwr.t.test(n = NULL, d = NULL, sig.level = 0.05, power = NULL,
        type = c('two.sample', 'one.sample', 'paired'),
        alternative = c('two.sided', 'less', 'greater'))

Arguments
    n -> Number of observations (per sample)
    d -> Effect size (Cohen's d) - difference between the means divided by the pooled standard deviation
    sig.level -> Significance level (Type I error probability)
    power -> Power of test (1 minus Type II error probability)
    type -> Type of t test : one- two- or paired-samples
    alternative -> a character string specifying the alternative hypothesis, must be one of 'two.sided' (default), 'greater' or 'less'

Details
    Exactly one of the parameters 'd','n','power' and 'sig.level' must be passed as NULL, and that
    parameter is determined from the others. Notice that the last one has non-NULL default so NULL
    must be explicitly passed if you want to compute it.

"
#import pwr library
library(pwr)

#One Sample (power)
pwr.t.test(d=0.2,n=60,sig.level=0.10,type="one.sample",alternative="two.sided")

#Paired samples (power)
d<-8/(16*sqrt(2*(1-0.6)))
pwr.t.test(d=d,n=40,sig.level=0.05,type="paired",alternative="two.sided")

## Two independent samples (power)
d<-2/2.8
pwr.t.test(d=d,n=30,sig.level=0.05,type="two.sample",alternative="two.sided")

## Two independent samples (sample size)
pwr.t.test(d=0.3,power=0.75,sig.level=0.05,type="two.sample",alternative="greater")

