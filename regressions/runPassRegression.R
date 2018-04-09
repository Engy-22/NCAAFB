library("MASS", lib.loc="/usr/lib64/R/library")
options(stringsAsFactors=FALSE)

constrainedfbsplays = read.csv("constrainedfbsplays.csv")
constrainedquarterfbsplays = read.csv("constrainedquarterfbsplays.csv")
constrainedquarterscore14fbsplays = read.csv("constrainedquarterscore14fbsplays.csv")
constrainedscore14fbsplays = read.csv("constrainedscore14fbsplays.csv")
constrainedscore21fbsplays = read.csv("constrainedscore21fbsplays.csv")

constrainedqscore14fbsplays2013 = filter(constrainedquarterscore14fbsplays, Season==2013)

#regressional model with CEP predicted by run/pass
reg1= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3)

#regressional model with CEP predicted by run/pass interaction with season
reg2= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3*as.factor(constrainedfbsplays$Season))

# reg model using run/pass and home-on-offense as well as home-on-offense interaction with season
reg3= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense))

# reg model using run/pass and home-team-on-offense
reg4= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Home.On.Offense)*as.factor(constrainedfbsplays$Season))

# reg model using run pass and offense team code and defense team code as predictors
reg5= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Home.On.Offense)*as.factor(constrainedfbsplays$Season) + as.factor(constrainedfbsplays$Defense.Team.Name))

# reg model using run pass and offense team code/season interaction and defense team code/sesason interaction as predictors
reg6= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Home.On.Offense)*as.factor(constrainedfbsplays$Season) + as.factor(constrainedfbsplays$Defense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)*as.factor(constrainedfbsplays$Season)+ as.factor(constrainedfbsplays$Offense.Team.Name)*as.factor(constrainedfbsplays$Season))

#
reg7= lm(constrainedfbsplays$allCEP~as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name))

#
reg8= lm(constrainedfbsplays$allCEP~as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)+ as.factor(constrainedfbsplays$Season))

# season, home on offense,  
reg9= lm(constrainedfbsplays$allCEP~as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)*as.factor(constrainedfbsplays$Season)+ as.factor(constrainedfbsplays$Offense.Team.Name)*as.factor(constrainedfbsplays$Season))

# Reg 8 with play call back
reg10= lm(constrainedfbsplays$allCEP~constrainedfbsplays$Play.Type3 + as.factor(constrainedfbsplays$Home.On.Offense) + as.factor(constrainedfbsplays$Offense.Team.Name) + as.factor(constrainedfbsplays$Defense.Team.Name)+ as.factor(constrainedfbsplays$Season))

# reg10 with out plays where score differential is greater than 21 points
reg11= lm(constrainedscore21fbsplays$allCEP~constrainedscore21fbsplays$Play.Type3 + as.factor(constrainedscore21fbsplays$Home.On.Offense) + as.factor(constrainedscore21fbsplays$Offense.Team.Name) + as.factor(constrainedscore21fbsplays$Defense.Team.Name)+ as.factor(constrainedscore21fbsplays$Season))

# reg 10 with out 4th quarter
reg12= lm(constrainedquarterfbsplays$allCEP~constrainedquarterfbsplays$Play.Type3 + as.factor(constrainedquarterfbsplays$Home.On.Offense) + as.factor(constrainedquarterfbsplays$Offense.Team.Name) + as.factor(constrainedquarterfbsplays$Defense.Team.Name)+ as.factor(constrainedquarterfbsplays$Season))

# reg10 with out plays where score differential is greater than 14 points
reg13= lm(constrainedscore14fbsplays$allCEP~constrainedscore14fbsplays$Play.Type3 + as.factor(constrainedscore14fbsplays$Home.On.Offense) + as.factor(constrainedscore14fbsplays$Offense.Team.Name) + as.factor(constrainedscore14fbsplays$Defense.Team.Name)+ as.factor(constrainedscore14fbsplays$Season))

# reg10 with out plays where score differential is greater than 14 points and no 4th quarter
reg14= lm(constrainedquarterscore14fbsplays$allCEP~constrainedquarterscore14fbsplays$Play.Type3 + as.factor(constrainedquarterscore14fbsplays$Home.On.Offense) + as.factor(constrainedquarterscore14fbsplays$Offense.Team.Name) + as.factor(constrainedquarterscore14fbsplays$Defense.Team.Name)+ as.factor(constrainedquarterscore14fbsplays$Season))

#plays where score differential is greater than 14 points and no 4th quarter
#run pass, Defense, Offense, Home-on-offense, run/pass*Offense
reg14= lm(constrainedquarterscore14fbsplays$allCEP~constrainedquarterscore14fbsplays$Play.Type3 + as.factor(constrainedquarterscore14fbsplays$Home.On.Offense) + as.factor(constrainedquarterscore14fbsplays$Offense.Team.Name) + as.factor(constrainedquarterscore14fbsplays$Defense.Team.Name)+ as.factor(constrainedquarterscore14fbsplays$Offense.Team.Name)*constrainedquarterscore14fbsplays$Play.Type3)

#plays where score differential is greater than 14 points and no 4th quarter
#run pass, Defense, Offense, Home-on-offense, run/pass*Offense
reg15= lm(constrainedqscore14fbsplays2013$allCEP~constrainedqscore14fbsplays2013$Play.Type3 + as.factor(constrainedqscore14fbsplays2013$Home.On.Offense) + as.factor(constrainedqscore14fbsplays2013$Offense.Team.Name) + as.factor(constrainedqscore14fbsplays2013$Defense.Team.Name)+ as.factor(constrainedqscore14fbsplays2013$Offense.Team.Name)*constrainedqscore14fbsplays2013$Play.Type3)


#be sure to put save command on every file, to keep the regression we generate in BASH
save.image("~/Fellow2016.RData")