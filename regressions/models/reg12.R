options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedquarterfbsplays = read.csv("constrainedquarterfbsplays.csv")

# reg 10 with out 4th quarter
reg12= lm(constrainedquarterfbsplays$allCEP~constrainedquarterfbsplays$Play.Type3 + as.factor(constrainedquarterfbsplays$Home.On.Offense) + as.factor(constrainedquarterfbsplays$Offense.Team.Name) + as.factor(constrainedquarterfbsplays$Defense.Team.Name)+ as.factor(constrainedquarterfbsplays$Season))

summary(reg12)

anova(reg12)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")