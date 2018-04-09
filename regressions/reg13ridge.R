library("MASS", lib.loc="/usr/lib64/R/library")

options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedscore14fbsplays = read.csv("constrainedscore14fbsplays.csv")

mmscore14playtype3=model.matrix(~constrainedscore14fbsplays$Play.Type3-1)
mmscore14homeonOff=model.matrix(~constrainedscore14fbsplays$Home.On.Offense-1)
mmscore14Off=model.matrix(~constrainedscore14fbsplays$Offense.Team.Name-1)
mmscore14Def=model.matrix(~constrainedscore14fbsplays$Defense.Team.Name-1)
mmscore14Season=model.matrix(~as.factor(constrainedscore14fbsplays$Season)-1)

# reg model using run pass and offense team code/season interaction and defense team code/sesason interaction as predictors
reg13ridge = lm.ridge(constrainedfbsplays$allCEP~mmscore14playtype3 + mmscore14homeonOff + mmscore14Off + mmscore14Def + mmscore14Season -1,lambda=1000)

#get the run effect
coef(reg13ridge)[2]-coef(reg13ridge)[1]

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")