library(lavaan)
library(tidyverse)
# test mplus 2 lavaan code

# 6.1
syntax <- "TITLE:	this is an example of a linear growth
	model for a continuous outcome 
DATA:	FILE IS ex6.1.dat;
VARIABLE:	NAMES ARE y11-y14;
MODEL:	i s | y11@0 y12@1 y13@2 y14@3;"

#tentative:: must check also
lavSyntax <- mplus2lavaan.modelSyntax(syntax)
cat(lavSyntax)

#read data
mydata <- read.table("../mplus onsdagen/Mplus examples day 3/ex6.1.dat")
colnames(mydata) <- paste0("t", 1:4)# rename V1 to t1 etc

#look at data



mydata$id <- seq(nrow(mydata))#add student id for spaggetti plotting

#also rename variables in syntax
lavSyntax <- lavSyntax %>% str_replace_all("y1", "t")

#for plotting
long <- pivot_longer(mydata, 1:4, names_to="time")

#normality in the margins?
ggplot(long, aes(value))+geom_histogram()+facet_wrap(time ~., ncol=1)

#all
ggplot(long, aes(time, value, group=id))+geom_line()

# first 30 , with nice colors
subsample <- filter(long,id < 31)
ggplot(subsample, aes(time, value, group=id, color=factor(id)))+
  geom_line(show.legend = F)

fit <- growth(lavSyntax, data=mydata)
summary(fit)

# mixed modeling is also an option (hierarchical linear modeling)
#library(lme4)#package for random effects
#need time as a continuous..
#long$time2 <- str_extract(long$time, "\\d") %>% as.numeric()
#fm1 <- lmer(value ~ time2 + (time2 | id), long)
