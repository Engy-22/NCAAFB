library("MASS", lib.loc="/usr/lib64/R/library")

# ridge rendition of reg 15
options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedquarterscore14fbsplays = read.csv("constrainedscore21fbsplays.csv")

constrainedqscore14fbsplays2013 = filter(constrainedquarterscore14fbsplays, Season==2013)

#plays where score differential is greater than 14 points and no 4th quarter
#run pass, Defense, Offense, Home-on-offense, run/pass*Offense

mm13playtype3=model.matrix(~constrainedqscore14fbsplays2013$Play.Type3-1)
mm13homeonOff=model.matrix(~constrainedqscore14fbsplays2013$Home.On.Offense-1)
mm13Off=model.matrix(~constrainedqscore14fbsplays2013$Offense.Team.Name-1)
mm13Def=model.matrix(~constrainedqscore14fbsplays2013$Defense.Team.Name-1)

reg15=lm.ridge(constrainedqscore14fbsplays2013$allCEP~mm13playtype3+mm13homeonOff+mm13Off+mm13Def-1,lambda=1000)

passPercentage2013=tapply(constrainedqscore14fbsplays2013$Play.Type3=="PASS", constrainedqscore14fbsplays2013$Offense.Team.Name, mean)
reg15.coefficients = reg15$coef
reg15.team.effects = reg15.coefficients[4:127]

#check correlation
cor(passPercentage2013,reg15.team.effects)

save.image("~/Fellow2016.RData")