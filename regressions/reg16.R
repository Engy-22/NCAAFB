library("MASS", lib.loc="/usr/lib64/R/library")

# ridge rendition of reg 15
options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedscore14fbsplays = read.csv("constrainedscore21fbsplays.csv")

constrainedqscore14fbsplays2012 = filter(constrainedquarterscore14fbsplays, Season==2012)

#plays where score differential is greater than 14 points and no 4th quarter
#run pass, Defense, Offense, Home-on-offense, run/pass*Offense

mm12playtype3=model.matrix(~constrainedqscore14fbsplays2012$Play.Type3-1)
mm12homeonOff=model.matrix(~constrainedqscore14fbsplays2012$Home.On.Offense-1)
mm12Off=model.matrix(~constrainedqscore14fbsplays2012$Offense.Team.Name-1)
mm12Def=model.matrix(~constrainedqscore14fbsplays2012$Defense.Team.Name-1)

reg16=lm.ridge(constrainedqscore14fbsplays2012$allCEP~mm12playtype3+mm12homeonOff+mm12Off+mm12Def-1,lambda=1000)

passPercentage2012=tapply(constrainedqscore14fbsplays2012$Play.Type3=="PASS", constrainedqscore14fbsplays2012$Offense.Team.Name, mean)
reg16.coefficients = reg16$coef
reg16.team.effects = reg16.coefficients[4:127]

#check correlation
cor(passPercentage2012,reg16.team.effects)

save.image("~/Fellow2016.RData")