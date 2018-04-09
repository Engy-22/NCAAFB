options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedfbsplays = read.csv("constrainedfbsplays.csv")


# season, home on offense,  
reg9= lm(constrainedfbsplays$allCEP~as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)*as.factor(constrainedfbsplays$Season)+ as.factor(constrainedfbsplays$Offense.Team.Name)*as.factor(constrainedfbsplays$Season))

summary(reg9)

anova(reg9)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")