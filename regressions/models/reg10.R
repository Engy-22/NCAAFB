options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedfbsplays = read.csv("constrainedfbsplays.csv")

# Reg 8 with play call back
reg10= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)+ as.factor(constrainedfbsplays$Season))

summary(reg10)

anova(reg10)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")