library("MASS", lib.loc="/usr/lib64/R/library")

options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedfbsplays = read.csv("constrainedfbsplays.csv")

mmplaytype3=model.matrix(~constrainedfbsplays$Play.Type3-1)
mmhomeonOff=model.matrix(~constrainedfbsplays$Home.On.Offense-1)
mmOff=model.matrix(~constrainedfbsplays$Offense.Team.Name-1)
mmDef=model.matrix(~constrainedfbsplays$Defense.Team.Name-1)
mmSeason=model.matrix(~as.factor(constrainedfbsplays$Season)-1)
mmhomeonOffSeason=model.matrix(~as.factor(constrainedfbsplays$Season)*as.factor(constrainedfbsplays$Home.On.Offense)-1)

# reg model using run pass and offense team code/season interaction and defense team code/sesason interaction as predictors
reg5ridge = lm.ridge(constrainedfbsplays$allCEP~mmplaytype3 + mmhomeonOff + mmOff + mmDef + mmhomeonOffSeason -1,lambda=1000)

#get the run effect
coef(reg5ridge)[2]-coef(reg5ridge)[1]

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")