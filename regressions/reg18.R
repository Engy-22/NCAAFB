library("MASS", lib.loc="/usr/lib64/R/library")

# ridge rendition of reg 15
options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedscore14fbsplays = read.csv("constrainedscore21fbsplays.csv")

constrainedqscore14fbsplays2010 = filter(constrainedquarterscore14fbsplays, Season==2010)

#plays where score differential is greater than 14 points and no 4th quarter
#run pass, Defense, Offense, Home-on-offense, run/pass*Offense

mm10playtype3=model.matrix(~constrainedqscore14fbsplays2010$Play.Type3-1)
mm10homeonOff=model.matrix(~constrainedqscore14fbsplays2010$Home.On.Offense-1)
mm10Off=model.matrix(~constrainedqscore14fbsplays2010$Offense.Team.Name-1)
mm10Def=model.matrix(~constrainedqscore14fbsplays2010$Defense.Team.Name-1)

reg18=lm.ridge(constrainedqscore14fbsplays2010$allCEP~mm10playtype3+mm10homeonOff+mm10Off+mm10Def-1,lambda=1000)

passPercentage2010=tapply(constrainedqscore14fbsplays2010$Play.Type3=="PASS", constrainedqscore14fbsplays2010$Offense.Team.Name, mean)

#Effect of team on CEP 
reg18.coefficients = reg18$coef
reg18.team.effects = reg18.coefficients[4:125]

#check correlation
cor(passPercentage2010,reg18.team.effects)

save.image("~/Fellow2016.RData")