options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedfbsplays = read.csv("constrainedfbsplays.csv")

# reg model using run/pass and home-team-on-offense
reg4= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense)*as.factor(constrainedfbsplays$Season))
summary(reg4)

anova(reg4)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")