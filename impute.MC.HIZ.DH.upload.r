#The aim of this script is to impute MRI phenotypes (Complicates: MC, HIZ, and DH and Core: SS + binarized DB)
#The purpose of this imputation is to generate the "lifetime slope" that better explains the lifetime change of these features instead of calculating the 
#slope of features between T1 and T2, for this slope may not be representative for lifetime change. 
#Random slope fixed intercept (0,0) model is adopted to fit the data.

library(lme4)
library(nlme)
library(lmerTest)
#read data
setwd("/path/to/data")
df <- read.csv("all.progression.phenotype.20201210.csv")
#extract id, sex, age, age squared,age cubed.

##To re-shape the mc dataframe to run LMM
####MC####
mc <- df[,c(1:5,43,59,75)]
mc.t1 <- mc[,c(1:3,6)]
mc.t1$Time <- rep("T1",nrow(mc.t1))
names(mc.t1)[3:4] <- c("age","mc")

mc.t2 <- mc[,c(1:2,4,7)]
mc.t2$Time <- rep("T2",nrow(mc.t2))
names(mc.t2)[3:4] <- c("age","mc")

mc.t3 <- mc[,c(1:2,5,8)]
mc.t3$Time <- rep("T3",nrow(mc.t3))
names(mc.t3)[3:4] <- c("age","mc")

##MC####
mc.3t <- rbind(mc.t1,mc.t2,mc.t3)
mc.3t$s.age <- scale(mc.3t$age)
mc.3t$s.age.sqrd <- mc.3t$s.age^2
mc.3t$s.age.cubed <- mc.3t$s.age^3

model.mc = lmer(mc ~ 0 + sex  + s.age + s.age.sqrd + s.age.cubed + (0+s.age|IID), 
             data = mc.3t)
fit.mc <- summary(model.mc)
#head of personal coefficient, the coefficient of s.age is the sum of fixed and random coefficients
#head(coef(model)$IID)
mc.idv <- data.frame(coef(model.mc)$IID)
write.csv(mc.idv, "mc.slope.csv",quote = FALSE)

###########
####HIZ####
###########

hiz <- df[,c(1:5, 42,58,74)]
#write.csv(hiz,"3Thiz.csv",quote = FALSE, row.names = FALSE)
hiz.t1 <- hiz[,c(1:3,6)]
hiz.t1$Time <- rep("T1",nrow(hiz.t1))
names(hiz.t1)[3:4] <- c("age","hiz")

hiz.t2 <- hiz[,c(1:2,4,7)]
hiz.t2$Time <- rep("T2",nrow(hiz.t2))
names(hiz.t2)[3:4] <- c("age","hiz")

hiz.t3 <- hiz[,c(1:2,5,8)]
hiz.t3$Time <- rep("T3",nrow(hiz.t3))
names(hiz.t3)[3:4] <- c("age","hiz")

hiz.3t <- rbind(hiz.t1,hiz.t2,hiz.t3)

hiz.3t$s.age <- scale(hiz.3t$age)
hiz.3t$s.age.sqrd <- hiz.3t$s.age^2
hiz.3t$s.age.cubed <- hiz.3t$s.age^3

model.hiz = lmer(hiz ~ 0 + sex  + s.age + s.age.sqrd + s.age.cubed + (0+s.age|IID), 
             data = hiz.3t)
fit.hiz <- summary(model.hiz)
#head of personal coefficient, the coefficient of s.age is the sum of fixed and random coefficients
#head(coef(model)$IID)
hiz.idv <- data.frame(coef(model.hiz)$IID)
write.csv(hiz.idv, "hiz.slope.csv",quote = FALSE)

##########
####DH####
##########
dh <- df[,c(1:5,46,62,78)]
#write.csv(dh, "3Tdh.csv",quote = FALSE, row.names = FALSE)
dh.t1 <- dh[,c(1:3,6)]
dh.t1$Time <- rep("T1",nrow(dh.t1))
names(dh.t1)[3:4] <- c("age","dh")

dh.t2 <- dh[,c(1:2,4,7)]
dh.t2$Time <- rep("T2",nrow(dh.t2))
names(dh.t2)[3:4] <- c("age","dh")

dh.t3 <- dh[,c(1:2,5,8)]
dh.t3$Time <- rep("T3",nrow(dh.t3))
names(dh.t3)[3:4] <- c("age","dh")

##3.dh
dh.3t <- rbind(dh.t1,dh.t2,dh.t3)

dh.3t$s.age <- scale(dh.3t$age)
dh.3t$s.age.sqrd <- dh.3t$s.age^2
dh.3t$s.age.cubed <- dh.3t$s.age^3

model.dh = lmer(dh ~ 0 + sex  + s.age + s.age.sqrd + s.age.cubed + (0+s.age|IID), 
                 data = dh.3t)
fit.dh <- summary(model.dh)
dh.idv <- data.frame(coef(model.dh)$IID)
write.csv(dh.idv, "dh.slope.csv",quote = FALSE)

sink("complications.fit.txt")
fit.mc
fit.hiz
fit.dh
sink()