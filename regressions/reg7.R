options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedfbsplays = read.csv("constrainedfbsplays.csv")

#
reg7= lm(constrainedfbsplays$allCEP~as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name))

summary(reg7)

anova(reg7)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")