library("MASS", lib.loc="/usr/lib64/R/library")

# ridge rendition of reg 19
options(stringsAsFactors=FALSE)
rm(list = ls())
constrainedscore14fbsplays = read.csv("constrainedscore21fbsplays.csv")

constrainedqscore14fbsplays2009 = filter(constrainedquarterscore14fbsplays, Season==2009)

#plays where score differential is greater than 14 points and no 4th quarter
#run pass, Defense, Offense, Home-on-offense, run/pass*Offense

mm09playtype3=model.matrix(~constrainedqscore14fbsplays2009$Play.Type3-1)
mm09homeonOff=model.matrix(~constrainedqscore14fbsplays2009$Home.On.Offense-1)
mm09Off=model.matrix(~constrainedqscore14fbsplays2009$Offense.Team.Name-1)
mm09Def=model.matrix(~constrainedqscore14fbsplays2009$Defense.Team.Name-1)

reg19=lm.ridge(constrainedqscore14fbsplays2009$allCEP~mm09playtype3+mm09homeonOff+mm09Off+mm09Def-1,lambda=1000)

passPercentage2009=tapply(constrainedqscore14fbsplays2009$Play.Type3=="PASS", constrainedqscore14fbsplays2009$Offense.Team.Name, mean)

#Effect of team on CEP 
reg19.coefficients = reg19$coef
reg19.team.effects = reg19.coefficients[4:125]

#check correlation
cor(passPercentage2009,reg19.team.effects)

save.image("~/Fellow2016.RData")