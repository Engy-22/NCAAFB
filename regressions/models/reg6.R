options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedfbsplays = read.csv("constrainedfbsplays.csv")

# reg model using run pass and offense team code/season interaction and defense team code/sesason interaction as predictors
reg6= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Home.On.Offense)*as.factor(constrainedfbsplays$Season) + as.factor(constrainedfbsplays$Defense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)*as.factor(constrainedfbsplays$Season)+ as.factor(constrainedfbsplays$Offense.Team.Name)*as.factor(constrainedfbsplays$Season))
summary(reg6)

anova(reg6)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")