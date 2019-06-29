setwd("~/Documents/BIOMETRY/week8")
> r1 <-rnorm(runif(1, 70, 110), runif(1, 75, 105), runif(1, 0.6, 1))
> r2 <-rnorm(runif(1, 50, 71), runif(1, 49.6, 60.87), runif(1, 0.6, 1.2))
> dat3 <-cbind.data.frame(c(rep("treatment",
                                +                               length(r1)), rep("control", length(r2))), c(r1, r2))
> data.Q3 <-dat3
> colnames(data.Q3)<-c("treatment","mass")
> head(data.Q3)
treatment     mass
1 treatment 79.
2 treatment 78.79105
3 treatment 79.37421
4 treatment 79.40932
5 treatment 79.16352
6 treatment 79.64475
> tp <- t.test(data.Q3$mass)
> ts = replicate(1000,tp(rnorm(79),rnorm(70))$statistic)
> range(ts)
[1] -3.859086  3.554139
> tsp=seq(-4,4, length=149)
> abline(0,1)
> abline(0,1, col='red')

> t <- subset(data.Q3, treatment == "treatment")
> mean(t$mass)
[1] 75.14684
> c <- subset(data.Q3, treatment == "control")
> mean(c$mass)
[1] 55.8005
> ctsa = table (data.Q1$species, data.Q1$ants)
> ctsa

No Yes
Acacia_sphaerocephala 21  21
Vachellia_hindsii      8  11
> ctsa = as.table(ctsa)
> ctsa

No Yes
Acacia_sphaerocephala 21  21
Vachellia_hindsii      8  11
> fisher.test(ctsa, alternative = 'two.sided')

Fisher's Exact Test for Count Data

data:  ctsa
p-value = 0.5933
alternative hypothesis: true odds ratio is not equal to 1
95 percent confidence interval:
 0.4047304 4.7917792
sample estimates:
odds ratio 
  1.367832 
>mosaicplot(qtally, main = "Species and Ants distribution",color = 2:3, margin = TRUE)

> species <-c(rep("Vachellia_hindsii", sample(10:70, 1, replace=T)),
+ rep("Acacia_sphaerocephala", sample(14:80, 1, replace=T)))
> ants <-sample(c(rep("Yes", 100), rep("No", 100)), length(species), replace=T)
> data.Q1 <-data.frame(species, ants)
> head(data.Q1)
species ants
1 Vachellia_hindsii No
2 Vachellia_hindsii Yes
3 Vachellia_hindsii No
4 Vachellia_hindsii No
5 Vachellia_hindsii Yes
> fsher.test(ctsa, alternative = 'two.sided')
Fisher's Exact Test for Count Data
data: ctsa
p-value = 0.5933
alternative hypothesis: true odds ratio is not equal to 1
95 percent confdence interval:
  0.4047304 4.7917792
sample estimates:
  odds ratio
1.367832
> chisq_test(ctsa)
Asymptotic Pearson Chi-Squared Test
data: Var2 by Var1 (Acacia_sphaerocephala, Vachellia_hindsii)
chi-squared = 0.32693, df = 1, p-value = 0.5675
> mosaicplot(qtally, main = "Species and Ants distribution",color = 2:3, margin = TRUE)
> library(MASS)
> dat2 <-data.frame(mvrnorm(125, c(16.99, 22.56),
                            + matrix(runif(1, 0.6, 0.9),
                                     + nrow = 2, ncol=2) + diag(2)*runif(1, 0.6, 0.9)))
> data.Q2 <-dat2
> colnames(data.Q2) <-c("Before", "After")
> head(data.Q2)
Before After
1 17.13071 23.24762
2 19.81407 26.04698
3 16.56372 22.12478
4 16.77479 22.54210
5 16.38683 23.81268
6 15.95823 20.92772
> plot(data.Q2)
> hist(data.Q2$Before)
> hist(data.Q2$After)
> summary(aovlmjax)
Df Sum Sq Mean Sq F value Pr(>F)
Before 1 55.75 55.75 59.39 3.72e-12 ***
  Residuals 123 115.46 0.94
---
  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> aovlmjax1 = aov(Before ~ After, data.Q2 )
> summary(aovlmjax1)
Df Sum Sq Mean Sq F value Pr(>F)
After 1 56.87 56.87 59.39 3.72e-12 ***
  Residuals 123 117.79 0.96
---
  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
> summary(lmjax)
Call:
  lm(formula = data.Q2$After ~ data.Q2$Before)
Residuals:
  Min 1Q Median 3Q Max
-2.33137 -0.64973 -0.04055 0.70248 2.47925
Coefficients:
  Estimate Std. Error t value Pr(>|t|)
(Intercept) 12.93388 1.23867 10.442 < 2e-16 ***
  data.Q2$Before 0.56495 0.07331 7.706 3.72e-12 ***
  ---
  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Residual standard error: 0.9689 on 123 degrees of freedom
Multiple R-squared: 0.3256, Adjusted R-squared: 0.3201
F-statistic: 59.39 on 1 and 123 DF, p-value: 3.722e-12
> summary(lmjax2)
Call:
  lm(formula = data.Q2$Before ~ data.Q2$After)
Residuals:
  Min 1Q Median 3Q Max
-2.50389 -0.64467 -0.07569 0.76901 3.13867
Coefficients:
  Estimate Std. Error t value Pr(>|t|)
(Intercept) 3.91231 1.68175 2.326 0.0216 *
  data.Q2$After 0.57635 0.07479 7.706 3.72e-12 ***
  ---
  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
Residual standard error: 0.9786 on 123 degrees of freedom
Multiple R-squared: 0.3256, Adjusted R-squared: 0.3201
F-statistic: 59.39 on 1 and 123 DF, p-value: 3.722e-12
> summary(lmjax2)$r.squared
[1] 0.325611
> summary(lmjax)$r.squared
[1] 0.325611
> r1 <-rnorm(runif(1, 70, 110), runif(1, 75, 105), runif(1, 0.6, 1))
> r2 <-rnorm(runif(1, 50, 71), runif(1, 49.6, 60.87), runif(1, 0.6, 1.2))
> dat3 <-cbind.data.frame(c(rep("treatment",
                                + length(r1)), rep("control", length(r2))), c(r1, r2))
> data.Q3 <-dat3
> colnames(data.Q3)<-c("treatment","mass")
> head(data.Q3)
treatment mass
1 treatment 79.60622
2 treatment 78.79105
3 treatment 79.37421
4 treatment 79.40932
5 treatment 79.16352
6 treatment 79.64475
> t.test(data.Q3$mass,data.Q3$treatment)
Welch Two Sample t-test
data: data.Q3$mass and data.Q3$treatment
t = 68.734, df = 126.49, p-value < 2.2e-16
alternative hypothesis: true diference in means is not equal to 0
95 percent confdence interval:
  67.03145 71.00562
sample estimates:
  mean of x mean of y
69.5933387 0.5748031
> r1 <-rnorm(runif(1, 70, 110), runif(1, 75, 105), runif(1, 0.6, 1))
> r2 <-rnorm(runif(1, 50, 71), runif(1, 49.6, 60.87), runif(1, 0.6, 1.2))
> dat3 <-cbind.data.frame(c(rep("treatment",
                                + length(r1)), rep("control", length(r2))), c(r1, r2))
> data.Q3 <-dat3
> colnames(data.Q3)<-c("treatment","mass")
> head(data.Q3)
treatment mass
1 treatment 75.21925
2 treatment 74.02980
3 treatment 75.49750
4 treatment 75.59183
5 treatment 74.92286
6 treatment 75.79495
> t.test(data.Q3$mass)
One Sample t-test
data: data.Q3$mass
t = 82.918, df = 148, p-value < 2.2e-16
alternative hypothesis: true mean is not equal to 0
95 percent confdence interval:
  64.48364 67.63227
sample estimates:
  mean of x
66.05796
> tp <- t.test(data.Q3$mass)
#create data points for the plot.
> ts = replicate(1000,tp(rnorm(79),rnorm(70))$statistic)
> range(ts)
[1] -3.859086 3.554139
> tsp=seq(-4,4, length=149)
> abline(0,1)
> abline(0,1, col='red')
> t <- subset(data.Q3, treatment == "treatment")
> mean(t$mass)
[1] 75.14684
> c <- subset(data.Q3, treatment == "control")
> mean(c$mass)
[1] 55.8005
> r.1 <-rnorm(runif(1, 148, 153), runif(1, 45, 55),0.2)
> data.Q4 <-r.1
> data.Q4
[1] 50.51028 50.68105 50.59237 50.63758 50.29384 50.28741 50.45658 50.22474 50.28447 50.22563 50.14189 50.32751 50.45289 50.36383
[15] 50.52399 50.06155 50.60207 50.79568 50.50849 50.39793 50.41766 50.43530 50.64168 50.62633 50.49192 50.51070 50.61443 50.60023
[29] 50.59420 50.44725 50.48304 50.10564 50.46383 50.59521 50.47256 50.46867 50.41536 50.43245 50.52875 50.21889 50.15152 50.31395
[43] 50.24547 50.17802 50.14220 50.50694 50.55420 50.35158 50.38340 50.36851 50.53125 50.67124 50.79379 50.76360 50.66317 50.28012
[57] 50.34349 50.21669 50.68869 50.50581 50.22737 50.77462 50.43423 50.24404 50.52994 50.17528 50.16774 50.69655 50.73512 50.58108
[71] 50.39445 50.18275 50.75769 50.50307 50.46118 50.29330 50.51819 50.60525 50.41208 50.35402 50.25829 50.60029 50.31602 50.65924
[85] 50.45647 50.21143 50.36696 50.31317 50.26424 50.42363 50.13589 50.57215 50.23321 50.73427 50.67698 50.08253 50.46318 50.75476
[99] 50.68254 50.46307 50.24528 50.27241 50.55811 50.25616 50.46033 50.44913 50.50063 50.68253 50.15559 50.25280 50.40284 50.47796
[113] 50.42891 50.23942 50.55385 50.44447 50.52293 50.10272 50.52515 50.45305 50.29033 50.45182 50.60215 50.08391 50.11102 50.52752
[127] 50.16055 50.00569 50.23557 50.60471 50.38561 50.70029 50.64885 50.77425 50.30964 50.52760 50.58117 50.44297 50.55798 50.67027
[141] 50.47994 50.62282 50.65618 50.48259 50.43653 50.38291 50.36351 50.59051 50.32327
> head(data.Q4)
[1] 50.51028 50.68105 50.59237 50.63758 50.29384 50.28741
> pt(148, 3342.2)
[1] 1 (# this signifes that the mean of the population will be used as the mean of the sample in our calculation)
  > mean = mean(data.Q4)
  > mean
  [1] 50.43824
  > var = var(data.Q4)
  > var
  [1] 0.03393438
  > l = length(data.Q4)
  [1] 149
  > b = sqrt((var)/(l))
  > t.stat = (mean)/(b)
  > t.stat
  [1] 3342.205
  > t.test(data.Q4)
  One Sample t-test
  data: data.Q4
  t = 3342.2, df = 148, p-value < 2.2e-16
  alternative hypothesis: true mean is not equal to 0
  95 percent confdence interval:
    50.40842 50.46807
  sample estimates:
    mean of x
  50.43824
