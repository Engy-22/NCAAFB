options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedscore14fbsplays = read.csv("constrainedscore14fbsplays.csv")

# reg10 with out plays where score differential is greater than 14 points
reg13= lm(constrainedscore14fbsplays$allCEP~constrainedscore14fbsplays$Play.Type3 + as.factor(constrainedscore14fbsplays$Home.On.Offense) + as.factor(constrainedscore14fbsplays$Offense.Team.Name) + as.factor(constrainedscore14fbsplays$Defense.Team.Name)+ as.factor(constrainedscore14fbsplays$Season))

summary(reg13)

anova(reg13)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")