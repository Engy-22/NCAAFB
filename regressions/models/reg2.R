options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedfbsplays = read.csv("constrainedfbsplays.csv")

#regressional model with CEP predicted by run/pass interaction with season
reg2= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3*as.factor(constrainedfbsplays$Season))
summary(reg2)

anova(reg2)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")