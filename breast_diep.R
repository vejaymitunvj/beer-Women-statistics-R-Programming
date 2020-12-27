bd1 <- read.csv("http://www.stat.ufl.edu/~winner/data/breast_diep.csv/")
attach(bd1); names(bd1)

### Obtain sample sizes, sample means, and observed Test Statistic
(n1 <- length(totvol[trt==1]))
(n2 <- length(totvol[trt==2]))
(ybar1.obs <- mean(totvol[trt==1]))
(ybar2.obs <- mean(totvol[trt==2]))
(TS.obs <- ybar1.obs-ybar2.obs)
(n.tot <- n1+n2)

### Choose number of permutations and initialize TS vector to save Test Statistics
### set seed to be able to reproduce permutation samples
N <- 9999
TS <- rep(0,N)
set.seed(97531)

### Loop through N samples, generating Test Stat each time
for (i in 1:N) {
perm <- sample(1:n.tot,size=n.tot,replace=F)
if (i == 1) print(perm)
ybar1 <- mean(totvol[perm[1:n1]])             ### mean totvol of first n1 elements of perm
ybar2 <- mean(totvol[perm[(n1+1):(n1+n2)]])   ### mean totvol of next n2 elements of perm
TS[i] <- ybar1-ybar2
}

### Count # of cases where abs(TS) >= abs(TS.obs) for 2-sided test and obtain p-value
(num.exceed <- sum(TS>=TS.obs))
(p.val.2sided <- (num.exceed+1)/(N+1))

### Draw histogram of distribution of TS, with vertical line at TS.obs
hist(TS,xlab="Mean1 - Mean2",breaks=seq(-320,320,20),
main="Randomization Distribution for totvol")
abline(v=TS.obs)
