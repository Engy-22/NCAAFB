options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedscore21fbsplays = read.csv("constrainedscore21fbsplays.csv")
#cconstrained score 21.... fix below

# reg10 with out plays where score differential is greater than 21 points
reg11= lm(constrainedscorefbsplays$allCEP~constrainedscorefbsplays$Play.Type3 + as.factor(constrainedscorefbsplays$Home.On.Offense) + as.factor(constrainedscorefbsplays$Offense.Team.Name) + as.factor(constrainedscorefbsplays$Defense.Team.Name)+ as.factor(constrainedscorefbsplays$Season))

summary(reg11)

anova(reg11)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")