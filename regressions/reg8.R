options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedfbsplays = read.csv("constrainedfbsplays.csv")

#
reg8= lm(constrainedfbsplays$allCEP~as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)+ as.factor(constrainedfbsplays$Season))

summary(reg8)

anova(reg8)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")