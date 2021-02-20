###EXP3B###
#What is relevant?
#-->aggressivity score(0=avoidance, 1=biting, 2=jumping) & behavioural context (tending aphids/other behaviours)
#-->ants showing tolerant/explorative behaviours are excluded
#note: Nh-05/10.08.19-->no ant aggressivity was measured=no data available

###Remove empty columns
Exp3b<-Exp3b[,-c(15:20)]

###Make date information understandable for R
Exp3b$date <- as.Date (Exp3b$date , format = "%d.%m.%Y")

###Create a column specifying the behavioural context of the irritated ant
Exp3b$context<-NA
Exp3b[which(Exp3b[,"t-1_a"]==0),"context"]<-"other behaviour"
Exp3b[which(Exp3b[,"t-1_a"]==1),"context"]<-"tending aphids"

###Create a 2nd column specifying the behavioural context of the irritated ant
#(=extended version:ants with no specified behavioural context are classified in "other behaviour")
#-->done for my BA(???)
Exp3b$context_ext<-Exp3b$context
Exp3b[which(is.na(Exp3b[,"context"])==T),"context_ext"]<-"other behaviour"

###Create a column indicating the aggressive score of the irritated ant
Exp3b$aggr_score<-NA
Exp3b[which(Exp3b[,"aggr"]==1),"aggr_score"]<-0 #avoidance
Exp3b[which(Exp3b[,"aggr"]==4),"aggr_score"]<-0 #avoidance=aggressive pose without approaching the needle
Exp3b[which(Exp3b[,"aggr"]>=5),"aggr_score"]<-1 #attack by bite
Exp3b[which(Exp3b[,"JUMP"]==1),"aggr_score"]<-2 #attack by jump

###Exclude all ants showing explorative(=3)/tolerant(=2) behaviours=artificial stimuli was not successful
Exp3b<-Exp3b[which(is.na(Exp3b[,"aggr_score"])==F),] #47 ants excluded

###Create summary table listing the average aggressivity score of irritated ant
# at *behavioural context*date*plot-level
meanAggressivity<-summaryBy(formula=aggr_score~plot.simple+plant+date+context, data=Exp3b, FUN=c(mean,sd),
                           na.rm=F)
length(which(is.na(Exp3b[,"context"])==T))#note: 24 ants with no specified behavioural context

###Create a 2nd summary table listing the average aggressivity score of irritated ant
# at *behavioural context*date*plot-level based on the extended behavioural context category
meanAggressivity_ext<-summaryBy(formula=aggr_score~plot.simple+plant+date+context_ext, data=Exp3b, 
                                FUN=c(mean,sd), na.rm=F)
      #note: in this case all ants (332) become part of the analysis (done for my BA???)

