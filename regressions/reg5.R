options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedfbsplays = read.csv("constrainedfbsplays.csv")

# reg model using run pass and offense team code and defense team code as predictors
reg5= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Home.On.Offense)*as.factor(constrainedfbsplays$Season) + as.factor(constrainedfbsplays$Defense.Team.Name))
summary(reg5)

anova(reg5)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")