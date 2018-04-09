library("MASS", lib.loc="/usr/lib64/R/library")

options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedquarterscore14fbsplays = read.csv("constrainedquarterscore14fbsplays")

mmQscore14playtype3=model.matrix(~constrainedquarterscore14fbsplays$Play.Type3-1)
mmQscore14homeonOff=model.matrix(~constrainedquarterscore14fbsplays$Home.On.Offense-1)
mmQscore14Off=model.matrix(~constrainedquarterscore14fbsplays$Offense.Team.Name-1)
mmQscore14Def=model.matrix(~constrainedquarterscore14fbsplays$Defense.Team.Name-1)
mmQscore14Season=model.matrix(~as.factor(constrainedquarterscore14fbsplays$Season)-1)

# reg model using run pass and offense team code/season interaction and defense team code/sesason interaction as predictors
reg14ridge = lm.ridge(constrainedquarterscore14fbsplays$allCEP~mmQscore14playtype3 + mmQscore14homeonOff + mmQscore14Off + mmQscore14Def + mmQscore14Season -1,lambda=1000)

#get the run effect
coef(reg13ridge)[2]-coef(reg13ridge)[1]

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")