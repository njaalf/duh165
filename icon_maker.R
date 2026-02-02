library(ggplot2)
library(hexSticker)
library("scatterplot3d")
library(tidyverse)
library(lavaan)
# Generate simulated data
model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 ; s ~ 1*1'

set.seed(1)
dat <- simulateData(model, sample.nobs=30, model.type = "growth")
dat$id <- seq(nrow(dat))
ll <- pivot_longer(dat, cols=1:4)
theme_set(theme_minimal())
p <- ggplot(ll, aes(name, value, group=id, color=factor(id)))+
  geom_line(show.legend=F)+ylab("Skill")+xlab("Time")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.x = element_blank(), axis.ticks.x =element_blank() , 
        axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  xlab(NULL)+ylab(NULL)

p.sticker <- sticker(
  p, package="DAG 2 DUH165", 
  filename="docs/images/icon.png",s_width = 2,
  s_height = 1.4, s_y=1, s_x=1
)



