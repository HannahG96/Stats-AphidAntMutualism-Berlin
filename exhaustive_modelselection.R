###EXHAUSTIVE MODELSELECTION
#1.Fit the global model for each response variable 
#2.Apply the dredge function on this global model to detect relevant explanatory variables (criteria:AICc)
#3.Create a 95%-confidence set of models = all models whose cumulated Akaike weight exceeds 0.95
#4.Calculate the rel. importance of each explanatory variable&interaction based on the 95%-confidence set
             #rel. importance=sum of all Akaike weights of models, where the variable of interest appears
                    #-->I include all variables with a rel. importance > 0.6 in my best model


#!!!important:
#-->remove rows with NA-observations in the explanatory variables=influences modelselection

###APHID DENSITY###
Aphid_density<-Aphid_density[-which(is.na(Aphid_density[,"prop_paras"])==T),]
               ##1 lacking parasitism observation removed = 52 observations
glimpse(Aphid_density)#check classes of all variables
#Fit the global model:
#~plantStade+(N_ant+prop_paras+Seal_500)*date+plantStade:N_ant+prop_paras:Seal_500
fullAph<-lmer(N_aphid.mm~plantStade+meanAnt.mean_s+prop_paras_s+Seal_500_s+date_s+
                 date_s:meanAnt.mean_s+date_s:prop_paras_s+date_s:Seal_500_s+
                 plantStade:meanAnt.mean_s+prop_paras_s:Seal_500_s+(1|plantPop), data=Aphid_density,
               REML=FALSE,lmerControl(optimizer="bobyqa",
                                      optCtrl = list(maxfun = 1000000)))

plot(fullAph)
qqnorm(resid(fullAph))
display(fullAph)
#Exhaustive modelselection:
options(na.action=NULL)
bestAph.dredge <- dredge(fullAph)
#Create 95%-confidence set:
confint95.Aph<-subset(bestAph.dredge,cumsum(weight) <= .95)#28 models
#Calculate rel. importance of each variable:
importance(subset(bestAph.dredge,cumsum(weight) <= .95))
  #-->My model: ~N_ant+plantStade+date+date:N_ant
#Export list of all models in EXCEL:
write.xlsx(bestAph.dredge, paste(modelresultswd,"/dredge.N_aphid.mm.xlsx",sep=""))

############################################
###MAX PARASITISM AT PLANT-LEVEL###
Max_parasitism<-Max_parasitism[-which(is.na(Max_parasitism[,"max.paras"])==T),]
              ##I remove 1 missing observation in the response variable-->28 observations
glimpse(Max_parasitism)#check classes of all variables
#Fit the global model:
#~(N_ant+N_aphid)*Seal_500
fullParas<-lmer(max.paras~meanAnt.mean.mean_s+N_aphid.mean_s+Seal_500_s
                +meanAnt.mean.mean_s:Seal_500_s+N_aphid.mean_s:Seal_500_s+(1|plot.simple),
                data=Max_parasitism, REML=FALSE)

plot(fullParas)
qqnorm(resid(fullParas))
display(fullParas)
   ##-->full model is a singular fit+model diagnostics look bad
#Exhaustive modelselection:
options(na.action=NULL)
bestParas.dredge <- dredge(fullParas)
#Create 95%-confidence set:
confint95.Paras<-subset(bestParas.dredge,cumsum(weight) <= .95)#8 models
#Calculate rel. importance of each variable:
importance(subset(bestParas.dredge,cumsum(weight) <= .95))
  #-->My model: ~N_ant
  #note: probably this model is a singular fit
#Export list of all models in EXCEL:
write.xlsx(bestAph.dredge, paste(modelresultswd,"/dredge.Max_paras.xlsx",sep=""))


###########################################
###ANT ATTENDANCE
Ant_attendance<-Ant_attendance[-which(is.na(Ant_attendance[,"prop_paras"])==T),]
  ##1 lacking parasitism observation removed = 52 observations
glimpse(Ant_attendance)#check classes of all variables
#Fit the global model:
#~plantStade+(N_ant+prop_paras+Seal_500)*date+plantStade:N_ant+prop_paras:Seal_500
fullAntAtt<-lmer(AntperAphid.mean~N_aphid_s+prop_paras_s+Seal_500_s+date_s+
                date_s:N_aphid_s+date_s:prop_paras_s+date_s:Seal_500_s+
                prop_paras_s:Seal_500_s+(1|plantPop), data=Ant_attendance,
              REML=FALSE,lmerControl(optimizer="bobyqa",
                                     optCtrl = list(maxfun = 1000000)))

plot(fullAntAtt)
qqnorm(resid(fullAntAtt))
display(fullAntAtt)
####Log-transformed response:
fullAntAtt.log<-lmer(log(AntperAphid.mean)~N_aphid_s+prop_paras_s+Seal_500_s+date_s+
                   date_s:N_aphid_s+date_s:prop_paras_s+date_s:Seal_500_s+
                   prop_paras_s:Seal_500_s+(1|plantPop), data=Ant_attendance,
                 REML=FALSE,lmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun = 1000000)))
plot(fullAntAtt.log)#-->looks less skewed
qqnorm(resid(fullAntAtt.log))
display(fullAntAtt.log)
#Exhaustive modelselection (log-transformed response):
options(na.action=NULL)
bestAntAtt.dredge <- dredge(fullAntAtt.log)
#Create 95%-confidence set:
confint95.AntAtt<-subset(bestAntAtt.dredge,cumsum(weight) <= .95)#17 models
#Calculate rel. importance of each variable:
importance(subset(bestAntAtt.dredge,cumsum(weight) <= .95))
#-->My model: ~date+N_aphid+date:N_aphid+prop_paras+Seal_500
#Export list of all models in EXCEL:
write.xlsx(bestAntAtt.dredge, paste(modelresultswd,"/dredge.AntAtt.xlsx",sep=""))

##############################################
###ANT BEHAVIOUR
Ant_behaviour<-Ant_behaviour[-c(which(is.na(Ant_behaviour[,"prop_paras"])==T),
                                which(is.na(Ant_behaviour[,"record.period.sum"])==T)),]
  ##1 lacking parasitism observation removed + observation Nh-04/03.08 (this day no caretaker recorded) 
       #= 51 observations
glimpse(Ant_behaviour)#check classes of all variables

#TENDING TIME
#Fit the global model:
#~(N_aphid+prop_paras+Seal_500)*date+prop_paras:N_aphid+prop_paras:Seal_500
fullTend<-lmer(aphid_IA.sum~N_aphid_s+prop_paras_s+Seal_500_s+date_s+mean.temp_s+
                date_s:N_aphid_s+date_s:prop_paras_s+date_s:Seal_500_s+
                prop_paras_s:N_aphid_s+prop_paras_s:Seal_500_s+(1|plantPop),
              data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun = 1000000)))
plot(fullTend)
qqnorm(resid(fullTend))
display(fullTend)
  #note:full model is a singular fit
#Exhaustive modelselection:
options(na.action=NULL)
bestTend.dredge <- dredge(fullTend)
#Create 95%-confidence set:
confint95.Tend<-subset(bestTend.dredge,cumsum(weight) <= .95)#71 models
#Calculate rel. importance of each variable:
importance(subset(bestTend.dredge,cumsum(weight) <= .95))
#-->My model: ~date
#Export list of all models in EXCEL:
write.xlsx(bestTend.dredge, paste(modelresultswd,"/dredge.TendingTime.xlsx",sep=""))

#BEHAVIOURAL TRANSITIONS (switch/sec)
#Fit the global model:
#~(N_aphid+prop_paras+Seal_500)*date+prop_paras:N_aphid+prop_paras:Seal_500
fullSwitch<-lmer(unit_switch.sum~N_aphid_s+prop_paras_s+Seal_500_s+date_s+mean.temp_s+
                 date_s:N_aphid_s+date_s:prop_paras_s+date_s:Seal_500_s+
                 prop_paras_s:N_aphid_s+prop_paras_s:Seal_500_s+(1|plantPop),
               data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun = 1000000)))
plot(fullSwitch)
qqnorm(resid(fullSwitch))
display(fullSwitch)
#Exhaustive modelselection:
options(na.action=NULL)
bestSwitch.dredge <- dredge(fullSwitch)
#Create 95%-confidence set:
confint95.Switch<-subset(bestSwitch.dredge,cumsum(weight) <= .95)#47 models
#Calculate rel. importance of each variable:
importance(subset(bestSwitch.dredge,cumsum(weight) <= .95))
#-->My model: ~temperature
#Export list of all models in EXCEL:
write.xlsx(bestSwitch.dredge, paste(modelresultswd,"/dredge.BhvTransitions.xlsx",sep=""))

#INACTIVITY
#Fit the global model:
#~(N_aphid+prop_paras+Seal_500)*date+prop_paras:N_aphid+prop_paras:Seal_500
fullInact<-lmer(inactive.sum~N_aphid_s+prop_paras_s+Seal_500_s+date_s+mean.temp_s+
                  date_s:N_aphid_s+date_s:prop_paras_s+date_s:Seal_500_s+
                  prop_paras_s:N_aphid_s+prop_paras_s:Seal_500_s+(1|plantPop),
                data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                            optCtrl = list(maxfun = 1000000)))
plot(fullInact)
qqnorm(resid(fullInact))
display(fullInact)
#Exhaustive modelselection:
options(na.action=NULL)
bestInact.dredge <- dredge(fullInact)
#Create 95%-confidence set:
confint95.Inact<-subset(bestInact.dredge,cumsum(weight) <= .95)#74 models
#Calculate rel. importance of each variable:
importance(subset(bestInact.dredge,cumsum(weight) <= .95))
#-->My model: ~N_aphid+Seal_500
#Export list of all models in EXCEL:
write.xlsx(bestInact.dredge, paste(modelresultswd,"/dredge.Inactivity.xlsx",sep=""))

##############################################
###GROUP REACTION
Group_reaction<-Group_reaction[-which(is.na(Group_reaction[,"prop_paras"])==T),]
   ##1 lacking parasitism observation+group reaction measurement (same day) removed = 52 observations
glimpse(Group_reaction)#check classes of all variables
#NB OF REACTIVE ANTS
#Fit the global model:
#~(N_ant+prop_paras+Seal_500)*date+temperature+prop_paras:N_ant+prop_paras:Seal_500+N_ant:Seal_500
fullGroupDef<-glmer(MAXREACT~meanAnt.mean_s+prop_paras_s+Seal_500_s+date_s+mean.temp_s+
                      date_s:meanAnt.mean_s+date_s:prop_paras_s+date_s:Seal_500_s+
                      prop_paras_s:meanAnt.mean_s+prop_paras_s:Seal_500_s+meanAnt.mean_s:Seal_500_s+(1|plantPop),
                    data=Group_reaction, family=poisson, glmerControl(optimizer="bobyqa",
                                                                     optCtrl = list(maxfun = 1000000)))

plot(fullGroupDef)#looks somewhat skewed...
qqnorm(resid(fullGroupDef))
display(fullGroupDef)
#Calculate c-hat of the glmer model (=deviance(model) / df.residual(model))-->indicates oversdispersion
#=if c-hat>1 model is overdispersed
deviance(fullGroupDef) / df.residual(fullGroupDef)#1.102297-->use QAIC (=accounts for overdispersion)
#Exhaustive modelselection:
options(na.action=NULL)
bestGroupDef.dredge <- dredge(fullGroupDef, rank="QAIC", chat=deviance(fullGroupDef) / df.residual(fullGroupDef))
#Create 95%-confidence set:
confint95.GroupDef<-subset(bestGroupDef.dredge,cumsum(weight) <= .95)#119 models
#Calculate rel. importance of each variable:
importance(subset(bestGroupDef.dredge,cumsum(weight) <= .95))
#-->My model: ~N_ant
#Export list of all models in EXCEL:
write.xlsx(bestGroupDef.dredge, paste(modelresultswd,"/dredge.GroupDef.xlsx",sep=""))

#NB OF ATTACKS
#Fit the global model:
#~(N_ant+prop_paras+Seal_500)*date+temperature+prop_paras:N_ant+prop_paras:Seal_500+N_ant:Seal_500
fullGroupAtk<-glmer(MAXATTACKS~meanAnt.mean_s+prop_paras_s+Seal_500_s+date_s+mean.temp_s+
                      date_s:meanAnt.mean_s+date_s:prop_paras_s+date_s:Seal_500_s+
                      prop_paras_s:meanAnt.mean_s+prop_paras_s:Seal_500_s+meanAnt.mean_s:Seal_500_s+(1|plantPop),
                    data=Group_reaction, family=poisson, glmerControl(optimizer="bobyqa",
                                                                      optCtrl = list(maxfun = 1000000)))

plot(fullGroupAtk)
qqnorm(resid(fullGroupAtk))
display(fullGroupAtk)
#-->full model is singular fit
#Calculate c-hat of the glmer model (=deviance(model) / df.residual(model))-->indicates oversdispersion
#=if c-hat>4 model is overdispersed
deviance(fullGroupAtk) / df.residual(fullGroupAtk)#0.9540656 ; ALLRIGHT!
#Exhaustive modelselection:
options(na.action=NULL)
bestGroupAtk.dredge <- dredge(fullGroupAtk)
#Create 95%-confidence set:
confint95.GroupAtk<-subset(bestGroupAtk.dredge,cumsum(weight) <= .95)#104 models
#Calculate rel. importance of each variable:
importance(subset(bestGroupAtk.dredge,cumsum(weight) <= .95))
#-->My model: ~prop_paras+N_ant+prop_paras:N_ant
#Export list of all models in EXCEL:
write.xlsx(bestGroupAtk.dredge, paste(modelresultswd,"/dredge.GroupAttacks.xlsx",sep=""))

###############################################
###ANT AGGRESSIVITY
Ant_aggressivity<-Ant_aggressivity[-which(is.na(Ant_aggressivity[,"prop_paras"])==T),]
##1 lacking parasitism observation removed = 94 observations
glimpse(Ant_aggressivity)#check classes of all variables
#Fit the global model:
#~context+(N_aphid+prop_paras+Seal_500)*date+temperature+prop_paras:N_aphid+prop_paras:Seal_500+N_aphid:Seal_500
fullAntAggr<-lmer(aggr_score.mean~context+N_aphid_s+prop_paras_s+Seal_500_s+date_s+mean.temp_s+
                date_s:N_aphid_s+date_s:prop_paras_s+date_s:Seal_500_s+
                prop_paras_s:N_aphid_s+prop_paras_s:Seal_500_s+N_aphid_s:Seal_500_s+(1|plantPop), 
                    data=Ant_aggressivity, REML=FALSE)

plot(fullAntAggr)#looks a bit skewed...
qqnorm(resid(fullAntAggr))
display(fullAntAggr)
#Exhaustive modelselection:
options(na.action=NULL)
bestAntAggr.dredge <- dredge(fullAntAggr)
#Create 95%-confidence set:
confint95.AntAggr<-subset(bestAntAggr.dredge,cumsum(weight) <= .95)#190 models
#Calculate rel. importance of each variable:
importance(subset(bestAntAggr.dredge,cumsum(weight) <= .95))
#-->My model: ~context+date+N_aphid+Seal_500
      #note: in my BA temperature was also a relevant variable...
#Export list of all models in EXCEL:
write.xlsx(bestAntAggr.dredge, paste(modelresultswd,"/dredge.AntAggressivity.xlsx",sep=""))

#2nd try (done for my BA???):
#-->use all aggressivity observations=ants with no specified behavioural context are classified in 
#"other behaviour"
Ant_aggressivity_ext<-Ant_aggressivity_ext[-which(is.na(Ant_aggressivity_ext[,"prop_paras"])==T),]
          ##1 lacking parasitism observation removed = 97 observations
glimpse(Ant_aggressivity_ext)#check classes of all variables
#Fit the global model:
#~context+(N_aphid+prop_paras+Seal_500)*date+temperature+prop_paras:N_aphid+prop_paras:Seal_500+N_aphid:Seal_500
fullAntAggr2<-lmer(aggr_score.mean~context_ext+N_aphid_s+prop_paras_s+Seal_500_s+date_s+mean.temp_s+
                    date_s:N_aphid_s+date_s:prop_paras_s+date_s:Seal_500_s+
                    prop_paras_s:N_aphid_s+prop_paras_s:Seal_500_s+N_aphid_s:Seal_500_s+(1|plantPop), 
                  data=Ant_aggressivity_ext, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                                     optCtrl = list(maxfun = 1000000)))

plot(fullAntAggr2)#looks a bit skewed...
qqnorm(resid(fullAntAggr2))
display(fullAntAggr2)
#Exhaustive modelselection:
options(na.action=NULL)
bestAntAggr2.dredge <- dredge(fullAntAggr2)
#Create 95%-confidence set:
confint95.AntAggr2<-subset(bestAntAggr2.dredge,cumsum(weight) <= .95)#209 models
#Calculate rel. importance of each variable:
importance(subset(bestAntAggr2.dredge,cumsum(weight) <= .95))
#-->My model: ~context+date+Seal_500+N_aphid
      #note: Still does NOT(!!!) give the same results as in the BA
#Export list of all models in EXCEL:
write.xlsx(bestAntAggr2.dredge, paste(modelresultswd,"/dredge.AntAggressivity2.xlsx",sep=""))
