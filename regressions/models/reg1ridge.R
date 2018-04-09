library("MASS", lib.loc="/usr/lib64/R/library")

options(stringsAsFactors=FALSE)
rm(list = ls())

constrainedfbsplays = read.csv("constrainedfbsplays.csv")

mmplaytype3=model.matrix(~constrainedfbsplays$Play.Type3-1)

#regressional model with CEP predicted by run/pass
reg1ridge= lm.ridge(constrainedfbsplays$allCEP~mmplaytype3 -1,lambda=1000)

#get the run effect
coef(reg1ridge)[2]-coef(reg1ridge)[1]

#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")