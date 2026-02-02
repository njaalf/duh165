library(ggplot2)
library("scatterplot3d")
library(tidyverse)
library(lavaan)
# Generate simulated data from BRM study with 15 indicators
popmodel <- "F1=~start(0.8)*x1+start(0.9)*x2+start(1.1)*x3\n\nF2=~start(0.7)*x4+start(1.2)*x5+start(1.1)*x6\n\nF3=~start(0.7)*x7+start(1.3)*x8+start(0.9)*x9\n\nF4=~start(1.1)*x10+start(0.5)*x11+start(0.8)*x12\n\nF5=~start(0.7)*x13+start(1.2)*x14+start(1.4)*x15\n \n F1~~start(0.2)*F2+start(0.3)*F3+start(0.1)*F4+start(0)*F5 \n F2~~start(0.1)*F3+start(0.3)*F4+start(-0.3)*F5 \n F3~~start(0.1)*F4+start(-0.3)*F5 \n F4~~start(-0.2)*F5"
popmodel <- paste(popmodel, ";", "F1~~1*F1;F2~~1*F2;F3~~1*F3;F4~~1*F4;F5~~1*F5; ")
model <- "
F1 =~ x1 + x2 + x3
F2 =~ x4 + x5 + x6
F3 =~ x7 + x8 + x9
F4 =~ x10 + x11 + x12
F5 =~ x13 + x14 + x15"


set.seed(12)
dat <- simulateData(popmodel, sample.nobs=500, std.lv=T)

# 2. Introduce 5% missingness (MCAR/MAR approach)
set.seed(1) # For reproducibility
dat_missing <- as.data.frame(lapply(dat, function(x) {
  x[sample(c(TRUE, FALSE), length(x), replace = TRUE, prob = c(0.01, .99))] <- NA
  return(x)
}))


## did some gender tricks to get x1 slightly higer for females. 
dat_missing$gender <- ifelse(rowMeans(dat_missing[, 1:3], na.rm=T)>0, "F", "M")

write.csv(dat_missing, "data/bigfive.csv", row.names=F)
mydata <- dat_missing

set.seed(123)
dat <- simulateData(popmodel, sample.nobs=500, skewness=3, kurtosis=21, std.lv=T)
f <- cfa(model, dat, estimator="MLM")
write.csv(dat_missing, "data/bigfive_nonnormal.csv", row.names=F)
mydata <- dat_missing

