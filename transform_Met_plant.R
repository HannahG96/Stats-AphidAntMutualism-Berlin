###MET_PLANT###
#What is relevant?
#-->includes plantStade & parasitism info

###Make date information understandable for R:
Met_plant$date <- as.Date (Met_plant$date , format = "%d.%m.%Y")

###Simplify the plant stade info:
Met_plant$plantStade[(Met_plant$stade1==1)]<-1
Met_plant$plantStade[(Met_plant$stade2==1)]<-2
Met_plant$plantStade[(Met_plant$stade3==1)]<-3
Met_plant$plantStade[(Met_plant$stade4==1)]<-4
Met_plant<-Met_plant[,-c(8:11)]

#########################################################################################

###Calculate the maximal proportion of parasitism at plant level:
max.paras<-data.frame(unique(Met_plant[,c(1,7)]), max.paras=rep(NA, nrow(unique(Met_plant[,c(1,7)]))))
paras<-tapply(X=Met_plant[,17], INDEX=Met_plant[,c(1,7)], FUN=max)
paras[1,1]<-"no value"
order<-rep(NA,length(unique(max.paras[,1])))#rearrange rows of paras
for(i in 1:length(unique(max.paras[,1]))){
  order[i]<-which(rownames(paras)==unique(max.paras[,1])[i])}
paras<-paras[order,]  
for(j in 1:nrow(paras)){
    i<-length(which(is.na(max.paras[,3])==F))+1
    z<-i+length(which(is.na(paras[j,])==F))-1
  max.paras[c(i:z),3]<-paras[j,which(is.na(paras[j,])==F) ]}



