"***********************

Chi-Square Test 

************************

___________________________________________________________________________________________________

chisq.test() -> performs chi-squared contingency table tests and goodness-of-fit tests.
____________________________________________________________________________________________________

Description
Compute power of tests or determine parameters to obtain target power (similar to power.t.test).

Usage
chisq.test(x, y = NULL, correct = TRUE,
           p = rep(1/length(x), length(x)), rescale.p = FALSE,
           simulate.p.value = FALSE, B = 2000)

Arguments
x	-> a numeric vector or matrix. x and y can also both be factors.

y	-> a numeric vector; ignored if x is a matrix. If x is a factor, y should be a factor of the same length.

correct	-> a logical indicating whether to apply continuity correction when computing the test statistic for 2 by 2 tables: one half is subtracted from all |O - E| 
            differences; however, the correction will not be bigger than the differences themselves. No correction is done if simulate.p.value = TRUE.

p	-> a vector of probabilities of the same length of x. An error is given if any entry of p is negative.

rescale.p	-> a logical scalar; if TRUE then p is rescaled (if necessary) to sum to 1. If rescale.p is FALSE, and p does not sum to 1, an error is given.

simulate.p.value	-> a logical indicating whether to compute p-values by Monte Carlo simulation.

B	-> an integer specifying the number of replicates used in the Monte Carlo test.

Details
If x is a matrix with one row or column, or if x is a vector and y is not given, then a goodness-of-fit test is performed (x is treated as a one-dimensional
    contingency table). The entries of x must be non-negative integers. In this case, the hypothesis tested is whether the population probabilities equal those in p, 
    or are all equal if p is not given.

If x is a matrix with at least two rows and columns, it is taken as a two-dimensional contingency table: the entries of x must be non-negative integers.
    Otherwise, x and y must be vectors or factors of the same length; cases with missing values are removed, the objects are coerced to factors, 
    and the contingency table is computed from these. Then Pearson's chi-squared test is performed of the null hypothesis that the joint distribution of the 
    cell counts in a 2-dimensional contingency table is the product of the row and column marginals.

If simulate.p.value is FALSE, the p-value is computed from the asymptotic chi-squared distribution of the test statistic; 
    continuity correction is only used in the 2-by-2 case (if correct is TRUE, the default). Otherwise the p-value is computed for a Monte Carlo test (Hope, 1968)
    with B replicates.

In the contingency table case simulation is done by random sampling from the set of all contingency tables with given marginals, 
    and works only if the marginals are strictly positive. Continuity correction is never used, and the statistic is quoted without it. 
    Note that this is not the usual sampling situation assumed for the chi-squared test but rather that for Fisher's exact test.

In the goodness-of-fit case simulation is done by random sampling from the discrete distribution specified by p, each sample being of size n = sum(x). 
    This simulation is done in R and may be slow.

" 
"Examples"

## From Agresti(2007) p.39
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals


## Effect of simulating p-values
x <- matrix(c(12, 5, 7, 7), ncol = 2)
chisq.test(x)$p.value           # 0.4233
chisq.test(x, simulate.p.value = TRUE, B = 10000)$p.value
# around 0.29!


## Testing for population probabilities
## Case A. Tabulated data
x <- c(A = 20, B = 15, C = 25)
chisq.test(x)
chisq.test(as.table(x))             # the same
x <- c(89,37,30,28,2)
p <- c(40,20,20,15,5)
try(
  chisq.test(x, p = p)                # gives an error
)
chisq.test(x, p = p, rescale.p = TRUE)
# works
p <- c(0.40,0.20,0.20,0.19,0.01)
# Expected count in category 5
# is 1.86 < 5 ==> chi square approx.
chisq.test(x, p = p)            #               maybe doubtful, but is ok!
chisq.test(x, p = p, simulate.p.value = TRUE)


## Case B. Raw data
x <- trunc(5 * runif(100))
chisq.test(table(x))            # NOT 'chisq.test(x)'!
