###BEST MODELS###
#-->look at the model predictions, calculate deltaAICc, calculate deviance explained by each variable

####################################################################################
###APHID DENSITY
##delta AICc (extracted from the 95%-confidence model set)=1.180962
#Fit the best model:
bestAph<-lmer(N_aphid.mm~meanAnt.mean_s+plantStade+date_s+date_s:meanAnt.mean_s+(1|plantPop), 
              data=Aphid_density, REML=FALSE,lmerControl(optimizer="bobyqa",
                                     optCtrl = list(maxfun = 1000000)))
     #note: Best model is a singular fit
            #-->log-transformed the model does not create issues of singularity...
#Look at diagnostics:
plot(bestAph)
qqnorm(resid(bestAph))
     #somewhat skewed...
#Look at the model summary:
display(bestAph)#Model Deviance=99.3
     #note: in my BA I divided all aphid number by a mean length of focal zone(=65.66038mm)

#Calculate % deviance explained by each variable x (sequentially removed):
#-->(model deviance excl. x - model deviance incl. x) / nullmodel deviance

##Deviance of nullmodel=149.6
display(nullAph<-lmer(N_aphid.mm~1+(1|plantPop), 
              data=Aphid_density, REML=FALSE,lmerControl(optimizer="bobyqa",
                                                         optCtrl = list(maxfun = 1000000))))
AICc(nullAph)-120.4081 #deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=35.72606

##Expl. deviance by date:N_ant=0.0428-->4.28%
display(lmer(N_aphid.mm~meanAnt.mean_s+plantStade+date_s+(1|plantPop), 
             data=Aphid_density, REML=FALSE,lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun = 1000000))))#dev=105.7
##Expl. deviance by date=0.0047-->0.47%
display(lmer(N_aphid.mm~meanAnt.mean_s+plantStade+(1|plantPop), 
             data=Aphid_density, REML=FALSE,lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun = 1000000))))#dev=106.4
##Expl. deviance by phenol. stade=0.2039-->20.39%
display(lmer(N_aphid.mm~meanAnt.mean_s+(1|plantPop), 
             data=Aphid_density, REML=FALSE,lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun = 1000000))))#dev=136.9
##Expl. deviance by N_ant=0.0849-->8.49%
##Total % of explained deviance=0.3362-->33.62%
	
############################################################################################
###MAX. PARASITISM
##delta AICc (extracted from the 95%-confidence model set)=1.262278
#Fit the best model:
bestparas<-lmer(max.paras~meanAnt.mean.mean_s+(1|plot.simple),
                data=Max_parasitism, REML=FALSE)
 #note: Best model is a singular fit

#Look at diagnostics:
plot(bestparas)
qqnorm(resid(bestparas))
    #-->diagnostics are very bad

#Look at the model summary:
display(bestparas)#Model Deviance=261.2
#Look at the nullmodel statistics:
display(nullparas<-lmer(max.paras~1+(1|plot.simple),
                        data=Max_parasitism, REML=FALSE))#Null model deviance=264.2
AICc(nullparas)-269.7065#deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=1.480096
#-->Model is a POOR FIT=no further diagnostics are done!

#############################################################################
###ANT ATTENDANCE
##delta AICc (extracted from the 95%-confidence model set)=0.8889277
#Fit the best model:
bestAntAtt<-lmer(log(AntperAphid.mean)~date_s+N_aphid_s+
                   date_s:N_aphid_s+prop_paras_s+Seal_500_s+(1|plantPop), data=Ant_attendance,
                 REML=FALSE,lmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun = 1000000)))

#Look at diagnostics:
plot(bestAntAtt)
qqnorm(resid(bestAntAtt))

#Look at the model summary:
display(bestAntAtt)#Model Deviance=59.1

#Calculate % deviance explained by each variable x (sequentially removed):
#-->(model deviance excl. x - model deviance incl. x) / nullmodel deviance

##Deviance of nullmodel=113.9
display(nullAntAtt<-lmer(log(AntperAphid.mean)~1+(1|plantPop), data=Ant_attendance,
                      REML=FALSE,lmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun = 1000000))))
AICc(nullAntAtt)-77.53950 #deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=42.91049

##Expl. deviance by Seal_500=0.0281-->2.81%
display(lmer(log(AntperAphid.mean)~date_s+N_aphid_s+
               date_s:N_aphid_s+prop_paras_s+(1|plantPop), data=Ant_attendance,
             REML=FALSE,lmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun = 1000000))))#dev=62.3
##Expl. deviance by prop_paras=0.0334-->3.34%
display(lmer(log(AntperAphid.mean)~date_s+N_aphid_s+
               date_s:N_aphid_s+(1|plantPop), data=Ant_attendance,
             REML=FALSE,lmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun = 1000000))))#dev=66.1
##Expl. deviance by date:N_aphid=0.0492-->4.92%
display(lmer(log(AntperAphid.mean)~date_s+N_aphid_s+(1|plantPop), data=Ant_attendance,
             REML=FALSE,lmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun = 1000000))))#dev=71.7
##Expl. deviance by N_aphid=0.3696-->36.96%
display(lmer(log(AntperAphid.mean)~date_s+(1|plantPop), data=Ant_attendance,
             REML=FALSE,lmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun = 1000000))))#113.8
##Expl. deviance by date=0.0008-->0.08%
##Total % of explained deviance=0.4811-->48.11%

###################################################################################
###TENDING TIME
##delta AICc (extracted from the 95%-confidence model set)=0
#Fit the best model:
bestTend<-lmer(aphid_IA.sum~date_s+(1|plantPop),
                 data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                             optCtrl = list(maxfun = 1000000)))

#Look at diagnostics:
plot(bestTend)
qqnorm(resid(bestTend))

#Look at the model summary:
display(bestTend)#Model Deviance=-74.6

#Calculate % deviance explained by each variable x (sequentially removed):
#-->(model deviance excl. x - model deviance incl. x) / nullmodel deviance

##Deviance of nullmodel=-65.4
display(nullTend<-lmer(aphid_IA.sum~1+(1|plantPop),
                       data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                                   optCtrl = list(maxfun = 1000000))))
AICc(nullTend)-AICc(bestTend) #deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=6.84765

##Expl. deviance by date=(???)
##Total % of explained deviance=(???)
#note: I probably transformed the time fractions of tending aphids in % for the BA(=pahid_IA.sum*100)

#################################################
###BEHAVIOURAL TRANSITION RATE
##delta AICc (extracted from the 95%-confidence model set)=0
#Fit the best model:
bestSwitch<-lmer(unit_switch.sum~mean.temp_s+(1|plantPop),
                 data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                             optCtrl = list(maxfun = 1000000)))

#Look at diagnostics:
plot(bestSwitch)
qqnorm(resid(bestSwitch))

#Look at the model summary:
display(bestSwitch)#Model Deviance=-131.4 
#Look at the nullmodel statistics:
display(nullSwitch<-lmer(unit_switch.sum~1+(1|plantPop),
                        data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun = 1000000))))#Null model deviance=-121.7
AICc(nullSwitch)-AICc(bestSwitch)#deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=7.253067
#-->Model is not so an interesting outcome=results are not further discussed

######################################################################################
###INACTIVITY
##delta AICc (extracted from the 95%-confidence model set)=0
#Fit the best model:
bestInact<-lmer(inactive.sum~N_aphid_s+Seal_500_s+(1|plantPop),
                data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                            optCtrl = list(maxfun = 1000000)))

#Look at diagnostics:
plot(bestInact)
qqnorm(resid(bestInact))
  #-->diagnostics look not so good...

#Look at the model summary:
display(bestInact)#Model Deviance=-108.3  
#Look at the nullmodel statistics:
display(nullInact<-lmer(inactive.sum~1+(1|plantPop),
                         data=Ant_behaviour, REML=FALSE, lmerControl(optimizer="bobyqa",
                                optCtrl = list(maxfun = 1000000))))#Null model deviance=-100.9
AICc(nullInact)-AICc(bestInact)#deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=2.598232
#-->Model is a poor fit=results are not further discussed

#############################################################################
###GROUP REACTION: MAX.NUMBER OF REACTIVE ANTS
##delta QAIC (extracted from the 95%-confidence model set)=0
#Fit the best model:
bestGroupDef<-glmer(MAXREACT~meanAnt.mean_s+(1|plantPop),
                    data=Group_reaction, family=poisson, glmerControl(optimizer="bobyqa",
                                                                      optCtrl = list(maxfun = 1000000)))

#Look at diagnostics:
plot(bestGroupDef)
qqnorm(resid(bestGroupDef))
  #diagnostics look not super nice...

#Look at the model summary:
display(bestGroupDef)#Model Deviance=42.2

#Calculate % deviance explained by each variable x (sequentially removed):
#-->(model deviance excl. x - model deviance incl. x) / nullmodel deviance

##Deviance of nullmodel=39.8
display(nullGroupDef<-glmer(MAXREACT~1+(1|plantPop),
                            data=Group_reaction, family=poisson, glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun = 1000000))))
QAIC(nullGroupDef, chat=deviance(fullGroupDef) / df.residual(fullGroupDef))-QAIC(bestGroupDef, chat=deviance(fullGroupDef) / df.residual(fullGroupDef)) 
         #deltaQAIC of nullmodel (bestmodelQAIC with delta of 0 - nullmodelQAIC)=27.10325

##Expl. deviance by N_ant=(???)
##Total % of explained deviance=(???)
            #-->Deviance of the best model is larger than that of the nullmodel...

############################################################################
###GROUP REACTION: MAX.NUMBER OF ATTACKS
##delta AICc (extracted from the 95%-confidence model set)=0
#Fit the best model:
bestGroupAtk<-glmer(MAXATTACKS~prop_paras_s+meanAnt.mean_s+prop_paras_s:meanAnt.mean_s+(1|plantPop),
                    data=Group_reaction, family=poisson, glmerControl(optimizer="bobyqa",
                                                                      optCtrl = list(maxfun = 1000000)))

#Look at diagnostics:
plot(bestGroupAtk)
qqnorm(resid(bestGroupAtk))
#-->diagnostics look bad...

#Look at the model summary:
display(bestGroupAtk)#Model Deviance=43.7  
#Look at the nullmodel statistics:
display(nullGroupAtk<-glmer(MAXATTACKS~1+(1|plantPop),
                            data=Group_reaction, family=poisson, glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun = 1000000))))#Null model deviance=50.1
AICc(nullGroupAtk)-AICc(bestGroupAtk)#deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=3.908984
#-->Model is a poor fit=results are not further discussed

##################################################################################
###ANT AGGRESSIVITY
##delta AICc (extracted from the 95%-confidence model set)=0.7588192
#Fit the best model:
bestAntAggr<-lmer(aggr_score.mean~context+N_aphid_s+Seal_500_s+date_s+(1|plantPop), 
                  data=Ant_aggressivity, REML=FALSE)

#Look at diagnostics:
plot(bestAntAggr)
qqnorm(resid(bestAntAggr))

#Look at the model summary:
display(bestAntAggr)#Model Deviance=119.6

#Calculate % deviance explained by each variable x (sequentially removed):
#-->(model deviance excl. x - model deviance incl. x) / nullmodel deviance

##Deviance of nullmodel=136.8
display(nullAntAggr<-lmer(aggr_score.mean~1+(1|plantPop), 
                          data=Ant_aggressivity, REML=FALSE))
AICc(nullAntAggr)-AICc(bestAntAggr) #deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=8.230011

##Expl. deviance by date=0.0270-->2.7%
display(lmer(aggr_score.mean~context+N_aphid_s+Seal_500_s+(1|plantPop), 
             data=Ant_aggressivity, REML=FALSE))#dev=123.3
##Expl. deviance by Seal_500=0.0314-->3.14%
display(lmer(aggr_score.mean~context+N_aphid_s+(1|plantPop), 
             data=Ant_aggressivity, REML=FALSE))#dev=127.6
##Expl. deviance by N_aphid=0.0227-->2.27%
display(lmer(aggr_score.mean~context+(1|plantPop), 
             data=Ant_aggressivity, REML=FALSE))#dev=130.7
##Expl. deviance by behavioural context=0.0446-->4.46%
##Total % of explained deviance=0.1257-->12.57%

###2nd try: ANT AGGRESSIVITY (with all ants included)
##delta AICc (extracted from the 95%-confidence model set)=0
#Fit the best model:
bestAntAggr2<-lmer(aggr_score.mean~context_ext+date_s+N_aphid_s+Seal_500_s+mean.temp_s+(1|plantPop), 
                  data=Ant_aggressivity_ext, REML=FALSE, lmerControl(optimizer="bobyqa",
                                                                    optCtrl = list(maxfun = 1000000)))

#Look at diagnostics:
plot(bestAntAggr2)
qqnorm(resid(bestAntAggr2))

#Look at the model summary:
display(bestAntAggr2)#Model Deviance=121.0 

#Calculate % deviance explained by each variable x (sequentially removed):
#-->(model deviance excl. x - model deviance incl. x) / nullmodel deviance

##Deviance of nullmodel=145.9
display(nullAntAggr2<-lmer(aggr_score.mean~1+(1|plantPop), 
                          data=Ant_aggressivity_ext, REML=FALSE))
AICc(nullAntAggr2)-AICc(bestAntAggr2) #deltaAICc of nullmodel (bestmodelAICc with delta of 0 - nullmodelAICc)=13.49444
##Expl. deviance by temperature=0.0267-->2.67%
display(lmer(aggr_score.mean~context_ext+date_s+N_aphid_s+Seal_500_s+(1|plantPop), 
             data=Ant_aggressivity_ext, REML=FALSE))#dev=124.9
##Expl. deviance by Seal_500=0.0336-->3.36%
display(lmer(aggr_score.mean~context_ext+date_s+N_aphid_s+(1|plantPop), 
             data=Ant_aggressivity_ext, REML=FALSE))#dev=129.8
##Expl. deviance by N_aphid=0.0418-->4.18%
display(lmer(aggr_score.mean~context_ext+date_s+(1|plantPop), 
             data=Ant_aggressivity_ext, REML=FALSE))#dev=135.9
##Expl. deviance by date=0.0103-->1.03%
display(lmer(aggr_score.mean~context_ext+(1|plantPop), 
             data=Ant_aggressivity_ext, REML=FALSE))#dev=137.4
##Expl. deviance by behavioural context=0.0583-->5.83%
##Total % of explained deviance=0.1706-->17.06%
