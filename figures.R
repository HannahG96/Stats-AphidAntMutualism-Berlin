###FIGURES###

###SUMMARY OF FIELDWORK
AphParas.Summary<-Aphid_density[,c("date","N_aphid","prop_paras")]

#Produce figure: N_aphid+prop_paras~date represented in 2 y-axes
AphParas.Summary$prop_paras.fig<-AphParas.Summary$prop_paras*2.7
                    #Change scale of prop_paras to fit it in figure

ggplot(AphParas.Summary, aes(x=date)) +
  theme_light()+
  geom_point(aes(y=N_aphid,color="wheat4"), color="wheat4") +
  scale_color_manual(labels=c("%parasitoids","N_aphid"), values=c("tomato3","wheat4"))+
  geom_point(aes(y=prop_paras.fig), color="tomato3", shape=17,size=2) +
  scale_y_continuous(
    "Number of aphids ", 
    sec.axis = sec_axis(~ . * 0.37, name = "% of parasitized aphids"))+
  geom_smooth(mapping=aes(x=date, y=N_aphid),formula=y~s(x,k=20), method="gam", colour="wheat4", , se=FALSE)+
  stat_smooth(mapping=aes(x=date,y=prop_paras.fig),formula=y~s(x,k=10),method="gam",col="tomato3", se=FALSE, size=1)+
  theme(axis.title.y = element_text(face="bold",color = "wheat4"),
        axis.text.y = element_text(color = "wheat4"),
        axis.title.y.right = element_text(color = "tomato3"),
        axis.text.y.right = element_text(color = "tomato3"),
        axis.title.x = element_text(face="bold"),
        axis.text.x = element_text(face="bold"))+
  xlab("Date")


###APHID DENSITY
#Create a "season"-column (begin/mid/end of fieldwork) to visualize in a figure the interaction date:N_ant:
setwd(datawd)
Season<-fread(file="Met_plot_date.csv",na.strings ="kA", dec = ",", data.table = FALSE)[,c("date","monthSeason")]
Season$date<-as.Date(Season$date, format = "%d.%m.%Y")
Aphid_density<-merge(Season,Aphid_density, by="date", all=T)
Aphid_density<-unique(Aphid_density)#-->merge() causes a doubling of columns
setwd(scriptwd)#restore script working directory

#Produce figure: aphid density~N_ant*date
ggplot(Aphid_density, aes(x=meanAnt.mean, y=N_aphid.mm, group=monthSeason, color=monthSeason))+
  theme_light()+
  geom_jitter(size=2.8,shape=18,aes(color=monthSeason))+
  scale_color_manual("Month of August",values=c("wheat4","lightgoldenrod3","tomato3"),
                     breaks=c("begin","mid","end"),
                     labels=c("Aug. 1st-10th","Aug. 11th-20th","Aug.21st-Sep.1st"))+
  geom_smooth(method="lm", se=FALSE)+
  ylab("Number of aphids per mm")+
  xlab("Number of attending ants")

#Produce figure: aphid density~phenological stade
ggplot(Aphid_density, aes(x=plantStade, y=N_aphid.mm, fill=plantStade))+
  theme_bw()+
  geom_boxplot()+
  scale_fill_manual(values=c("lightgoldenrod2", "lightgoldenrod3", "lightgoldenrod4", "wheat4"), 
                    name="Phenological stage",
                    breaks=c("1", "2","3","4"),
                    labels=c("budding", "full flowering", "end of flowering", "desiccated"))+
  theme(axis.title.x =element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_text(face="bold"))+
  ylab("Number of aphids per mm")


###ANT ATTENDANCE
#Create a categorical Sealing variable to visualize the effect of the %Sealing on the ant per aphid ratio:
Ant_attendance$Sealing_level<-NA
Ant_attendance[which(Ant_attendance[,"Seal_500"]>40), "Sealing_level"]<-"high"
Ant_attendance[which(Ant_attendance[,"Seal_500"]<40), "Sealing_level"]<-"medium"
Ant_attendance[which(Ant_attendance[,"Seal_500"]<20), "Sealing_level"]<-"low"

#Produce figure: ant per aphid~N_aphid+Seal_500
ggplot(Ant_attendance, aes(x=N_aphid, y=log(AntperAphid.mean)))+
  theme_light()+
  geom_jitter(size=2.4,aes(color=Seal_500,shape=Sealing_level))+
  scale_color_gradient2("Percentage of \n sealed surface",midpoint=26,low = "grey34", 
                        high="tomato3",mid="sandybrown")+
  new_scale_color()+
  geom_smooth(mapping=aes(x=N_aphid, y=log(AntperAphid.mean),color=Sealing_level,group=Sealing_level), 
              method="lm", se=FALSE)+
  scale_color_manual(NULL,values=c("wheat4","tomato3","sandybrown"),
                     breaks=c("high","medium","low"),
                     labels=c("[>40%]","[20-40%]","[<20%]"))+
  scale_shape_manual(NULL,values=c(16,17,15),
                     breaks=c("high","medium","low"),
                     labels=c("[>40%]","[20-40%]","[<20%]"))+
  ylab("Log(Ant / Aphid)")+
  xlab("Number of aphids")

###Tending Time
#Produce figure: proportion of time caretaker-ant tended aphids~date
ggplot(Ant_behaviour, aes(x=date, y=aphid_IA.sum))+
  theme_bw()+
  geom_jitter(shape=18, size=2.5, color="grey34")+
  geom_smooth(method="lm",color="grey34",se=FALSE)+
  xlab("Date")+
  ylab("Relative tending time in %")+
  theme(axis.text.x=element_text(face="bold"),
        axis.text.y=element_text(face="bold"))

###GROUP REACTION
#Produce figure: max. number of reactive ant / attacks~N_ant
ggplot(Group_reaction, aes(x=meanAnt.mean))+
  theme_bw()+
  geom_point(aes(y=MAXATTACKS,color="max.attack"),shape=18, size=2)+
  geom_jitter(aes(y=MAXREACT,color="max.reaction"), shape=18, size=2.5)+
  geom_smooth(mapping=aes(x=meanAnt.mean, y=MAXATTACKS,color="max.attack"),method="lm",linetype=2, se=FALSE)+
  geom_smooth(mapping=aes(x=meanAnt.mean, y=MAXREACT,color="max.reaction"),method="lm", se=FALSE)+
  scale_color_manual("",values=c("black","wheat4"), breaks=c("max.attack","max.reaction"),labels=c("Attacks","All type of \nreactions"))+
  ylab("Number of reactive ants")+
  xlab("Number of ants")

###ANT AGGRESSIVITY
#Produce figure: mean ant aggressivity score ~ behavioural context + Seal_500
#-->in BA "defense.scale.median" was used to produce figure (???)
ggplot(data=Ant_aggressivity, aes(x=Seal_500, y=aggr_score.mean, group=context,color=context,shape=context))+
  theme_bw()+
  geom_point(size=2)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_manual("Ant behaviour \nbefore irritation",values=c("black","wheat4"),breaks=c("tending aphids","other behaviour"),labels=c("tending aphids","other behaviour"))+
  scale_shape_manual("Ant behaviour \nbefore irritation",values=c(18,23),breaks=c("tending aphids","other behaviour"),labels=c("tending aphids","other behaviour"))+
  xlab("% of sealed surface in 500m")+
  ylab("Mean aggressivity score")


ggplot(data=Ant_aggressivity_ext, aes(x=Seal_500, y=aggr_score.mean, group=context_ext,color=context_ext,shape=context_ext))+
  theme_bw()+
  geom_point(size=2)+
  geom_smooth(method="lm",se=FALSE)+
  scale_color_manual("Ant behaviour \nbefore irritation",values=c("black","wheat4"),breaks=c("tending aphids","other behaviour"),labels=c("tending aphids","other behaviour"))+
  scale_shape_manual("Ant behaviour \nbefore irritation",values=c(18,23),breaks=c("tending aphids","other behaviour"),labels=c("tending aphids","other behaviour"))+
  xlab("% of sealed surface in 500m")+
  ylab("Mean aggressivity score")

