options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedfbsplays = read.csv("constrainedfbsplays.csv")

# reg model using run/pass and home-on-offense as well as home-on-offense interaction with season
reg3= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense))
summary(reg3)

anova(reg3)

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")