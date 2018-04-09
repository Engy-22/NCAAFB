library("MASS", lib.loc="/usr/lib64/R/library")

# ridge rendition of reg 17
options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedscore14fbsplays = read.csv("constrainedscore14fbsplays.csv")

constrainedqscore14fbsplays2011 = filter(constrainedquarterscore14fbsplays, Season==2011)

#plays where score differential is greater than 14 points and no 4th quarter
#run pass, Defense, Offense, Home-on-offense, run/pass*Offense

mm11playtype3=model.matrix(~constrainedqscore14fbsplays2011$Play.Type3-1)
mm11homeonOff=model.matrix(~constrainedqscore14fbsplays2011$Home.On.Offense-1)
mm11Off=model.matrix(~constrainedqscore14fbsplays2011$Offense.Team.Name-1)
mm11Def=model.matrix(~constrainedqscore14fbsplays2011$Defense.Team.Name-1)

reg17=lm.ridge(constrainedqscore14fbsplays2011$allCEP~mm11playtype3+mm11homeonOff+mm11Off+mm11Def-1,lambda=1000)

passPercentage2011=tapply(constrainedqscore14fbsplays2011$Play.Type3=="PASS", constrainedqscore14fbsplays2011$Offense.Team.Name, mean)

passPercentage2011=tapply(constrainedqscore14fbsplays2011$Play.Type3=="PASS", constrainedqscore14fbsplays2011$Offense.Team.Name, mean)
reg17.coefficients = reg17$coef
reg17.team.effects = reg17.coefficients[4:126]

#check correlation
cor(passPercentage2011,-reg17.team.effects)

save.image("~/Fellow2016.RData")