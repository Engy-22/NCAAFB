library("MASS", lib.loc="/usr/lib64/R/library")

options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedscore21fbsplays = read.csv("constrainedscore21fbsplays.csv")

mmscore21playtype3=model.matrix(~constrainedscore21fbsplays$Play.Type3-1)
mmscore21homeonOff=model.matrix(~constrainedscore21fbsplays$Home.On.Offense-1)
mmscore21Off=model.matrix(~constrainedscore21fbsplays$Offense.Team.Name-1)
mmscore21Def=model.matrix(~constrainedscore21fbsplays$Defense.Team.Name-1)
mmscore21Season=model.matrix(~as.factor(constrainedscore21fbsplays$Season)-1)

# reg model using run pass and offense team code/season interaction and defense team code/sesason interaction as predictors
reg11ridge = lm.ridge(constrainedscore21fbsplays$allCEP~mmscore21playtype3 + mmscore21homeonOff + mmscore21Off + mmscore21Def + mmscore21Season -1,lambda=1000)

#get the run effect
coef(reg11ridge)[2]-coef(reg11ridge)[1]

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")