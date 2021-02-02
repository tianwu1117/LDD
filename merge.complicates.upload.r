#The aim of this Rscript is to combine iid,sex,std.ages with the imputed complication scores.
setwd("/PATH/TO/DATA")
df <- read.csv("all.progression.phenotype.20201210.csv")
base <- df[,c(1:5)]
#MC
mc.slope <- read.csv("mc.slope.csv")
mc.slope.f <- mc.slope[,c(1,3)]
colnames(mc.slope.f)[2] <- "mc.slope"
#HIZ
hiz.slope <- read.csv("hiz.slope.csv")
hiz.slope.f <- hiz.slope[,c(1,3)]
colnames(hiz.slope.f)[2] <- "hiz.slope"
#DH
dh.slope <- read.csv("dh.slope.csv")
dh.slope.f <- dh.slope[,c(1,3)]
colnames(dh.slope.f)[2] <- "dh.slope"

#merge
step1 <- merge(base,mc.slope.f,all.x = TRUE)
step2 <- merge(step1, hiz.slope.f,all.x = TRUE)
step3 <- merge(step2, dh.slope.f, all.x = TRUE)

#write.tables
write.csv(step3,"slope.complications.csv",row.names = FALSE,quote = FALSE)