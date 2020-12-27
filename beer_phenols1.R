beerph <- read.csv("http://www.stat.ufl.edu/~winner/sta6166/beer_phenols.csv",
    header=T)
attach(beerph); names(beerph)

plot(brand, polyphenols)

brand.f <- factor(brand)

##### Assign y.var (response variable) and trt.var (factor variable)

y.var <- 
trt.var <- 

bp.mod1 <- aov(y.var ~ trt.var)
anova(bp.mod1)

install.packages("lawstat")
library(lawstat)
levene.test(y.var, trt.var)

### If you have problems downloading lawstat package, use this

install.packages("car")
library(car)
leveneTest(y.var, trt.var, "median")



oneway.test(y.var ~ trt.var, var.equal=F)

kruskal.test(y.var ~ trt.var)

### Contrasts

a.contrast <- c(-1,-1,1,1,-1,-1,1,1)

(brand.mean <- as.vector(tapply(polyphenols, brand.f, mean)))
(brand.n <- as.vector(tapply(polyphenols, brand.f, length)))
(brand.var <- as.vector(tapply(polyphenols, brand.f, var)))

(l.hi_lo <- sum(a.contrast*brand.mean))

(SSC.hi_lo <- (l.hi_lo^2)/sum(a.contrast^2/brand.n))

(SSW <- sum((brand.n-1)*brand.var))
(dfW <- sum(brand.n) - length(brand.n))
(MSW <- SSW/dfW)

(F.hi_lo <- SSC.hi_lo / MSW)
(F.hi_lo_pval <- 1-pf(F.hi_lo,1,dfW))

### All pairwise comparisons

TukeyHSD(bp.mod1, "trt.var")


### Bonferroni method
n.trt <- as.vector(tapply(y.var,trt.var,length))
ybar.trt <- as.vector(tapply(y.var,trt.var,mean))
sd.trt <- as.vector(tapply(y.var,trt.var,sd))
k.trt <- length(n.trt)
SSE <- sum((n.trt-1)*sd.trt^2)
dfE <- sum(n.trt)-k.trt
MSE <- SSE/dfE
bon.t <- qt(1-.05/(2*k.trt*(k.trt-1)/2),dfE)
bon.out <- matrix(rep(0,5*k.trt*(k.trt-1)/2),ncol=5)
bon.row <- 0

for(i1 in 1:(k.trt-1)) {
  for (i2 in (i1+1):k.trt) { 
    bon.row <- bon.row + 1
    bon.out[bon.row,1] <- i1
    bon.out[bon.row,2] <- i2
    bon.out[bon.row,3] <- ybar.trt[i1] - ybar.trt[i2]
    bon.out[bon.row,4] <- 
        (ybar.trt[i1] - ybar.trt[i2]) - 
         bon.t*sqrt(MSE*(1/n.trt[i1] + 1/n.trt[i2]))
    bon.out[bon.row,5] <-
        (ybar.trt[i1] - ybar.trt[i2]) + 
         bon.t*sqrt(MSE*(1/n.trt[i1] + 1/n.trt[i2]))
}}

colnames(bon.out) <- c("Trt i","Trt j","Diff","Lower Bound","Upper Bound")
round(bon.out,3)

