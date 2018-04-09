options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedfbsplays = read.csv("constrainedfbsplays.csv")

#regressional model with CEP predicted by run/pass
reg1= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3)
summary(reg1)

anova(reg1)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.reg1.RData")