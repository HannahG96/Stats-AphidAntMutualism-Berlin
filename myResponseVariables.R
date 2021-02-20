###MY RESPONSE VARIABLES###
#-->create tables for each of the modelled response variables that join together all explanatory variables 
   #needed to build the model

###EXPLANATORY VARIABLES
expVar<-Exp2_plot_date[,c("plot.simple", "plant", "date")]
expVar$plantPop<-paste(expVar[,"plot.simple"], expVar[,"plant"], sep=" ") #random effect=plant population
expVar<-merge(expVar, Exp2_plot_date[,c("plot.simple", "plant", "date", "meanAnt.mean", "N_aphid")], all=T) #ant+aphid number
expVar<-merge(expVar, Met_plant[,c("plot.simple", "plant", "date","prop_paras","plantStade")], all=T) #+plant stade+parasitism
expVar<-merge(expVar, Met_plot_date[,c("plot.simple", "date","mean.temp")], all=T) #+temperature
expVar<-merge(expVar, field.summary[,c("plot.simple","Seal_500")], all=T)#+sealing
glimpse(expVar)#check class of each variable
expVar$date<-as.Date(expVar$date, format = "%d.%m.%Y")#make date column understandable for R
expVar$Seal_500<-as.numeric(expVar$Seal_500)#make sealing numeric
expVar$plantStade<-as.factor(expVar$plantStade)#factorize phenological stade
expVar$plantPop<-as.factor(expVar$plantPop)#factorize plant population category
expVar$plot.simple<-as.factor(expVar$plot.simple)#factorize plot category

#Check for correlation of predictors:
ggpairs(expVar[-1,c("date","mean.temp","N_aphid","meanAnt.mean","prop_paras","Seal_500","plantStade")])
#Standardize all continuous variables:
expVar=mutate_at(expVar, vars(date,meanAnt.mean,N_aphid,prop_paras,mean.temp,Seal_500), funs(s = as.numeric( scale(.) ) ) )

###1.APHID DENSITY
Aphid_density<-merge(expVar,Exp2_plot_date[,c("plot.simple", "plant", "date", "l","N_aphid.mm")],all=T)
###2.MAXIMAL PROPORTION OF PARASITISM (plant-level)
expVar_plant<-summaryBy(formula=meanAnt.mean+N_aphid~plot.simple+plant+Seal_500, data=expVar, FUN=mean, na.rm=F)
#Standardize all continuous variables:
expVar_plant=mutate_at(expVar_plant, vars(meanAnt.mean.mean,N_aphid.mean,Seal_500), funs(s = as.numeric( scale(.) ) ) )
Max_parasitism<-merge(expVar_plant, max.paras)
Max_parasitism[which(Max_parasitism[,"max.paras"]=="no value"), "max.paras"]<-NA #make some change in data
Max_parasitism$max.paras<-as.numeric(Max_parasitism$max.paras)#transform parasitism response in numeric

###3.ANT ATTENDANCE
Ant_attendance<-merge(expVar,Exp2_plot_date[,c("plot.simple", "plant", "date","AntperAphid.mean")],all=T)
###4.ANT BEHAVIOUR (Behavioural transition rate/Tending time/Inactivity):only caretaker
Ant_behaviour<-merge(expVar, cum.task.allocation[which(cum.task.allocation[,"indv_cat"]=="caretaker"), 
                                                 c("plot.simple", "plant", "date", "record.period.sum", 
                                                "unit_switch.sum", "aphid_IA.sum", "inactive.sum")], all=T)
###5.GROUP REACTION
Group_reaction<-merge(expVar, Exp3a[, c("plot.simple", "plant", "date", "MAXREACT", "MAXATTACKS")], all=T)
###6.ANT AGGRESSIVITY
Ant_aggressivity<-merge(expVar, meanAggressivity[which(is.na(meanAggressivity[,"context"])==F), 
                                    c("plot.simple", "plant", "date","context","aggr_score.mean")], all=T)
Ant_aggressivity<-Ant_aggressivity[-which(is.na(Ant_aggressivity[,"context"])==T),]
        #remove NA observations=sampling sessions where no behavioural context was noted down 
Ant_aggressivity$context<-as.factor(Ant_aggressivity$context)
#2nd try:
#-->all ants get attributed a behavioural context=NAs classified in "other behaviours"
Ant_aggressivity_ext<-merge(expVar, meanAggressivity_ext[, c("plot.simple", "plant", "date","context_ext",
                                                         "aggr_score.mean")], all=T)
Ant_aggressivity_ext<-Ant_aggressivity_ext[-which(is.na(Ant_aggressivity_ext[,"context_ext"])==T),]
       #remove NA observation Nh-05/10.08.19=no ant aggressivity experiment was done this sampling session
Ant_aggressivity_ext$context_ext<-as.factor(Ant_aggressivity_ext$context_ext)

###Remove all object that are not needed anymore=clean workspace
rm.all.but(keep=c("scriptwd", "modelresultswd", "Aphid_density", "Max_parasitism", "Ant_attendance",
                  "Ant_behaviour", "Group_reaction", "Ant_aggressivity", "Ant_aggressivity_ext"))
