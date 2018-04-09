library("MASS", lib.loc="/usr/lib64/R/library")

options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedquarterfbsplays = read.csv("constrainedquarterfbsplays.csv")

mmquarterplaytype3=model.matrix(~constrainedquarterfbsplays$Play.Type3-1)
mmquarterhomeonOff=model.matrix(~constrainedquarterfbsplays$Home.On.Offense-1)
mmquarterOff=model.matrix(~constrainedquarterfbsplays$Offense.Team.Name-1)
mmquarterDef=model.matrix(~constrainedquarterfbsplays$Defense.Team.Name-1)
mmquarterSeason=model.matrix(~as.factor(constrainedquarterfbsplays$Season)-1)

# reg model using run pass and offense team code/season interaction and defense team code/sesason interaction as predictors
reg12ridge = lm.ridge(constrainedquarterfbsplays$allCEP~mmquarterplaytype3 + mmquarterhomeonOff + mmquarterOff + mmquarterDef + mmquarterSeason -1,lambda=1000)

#get the run effect
coef(reg12ridge)[2]-coef(reg12ridge)[1]

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")