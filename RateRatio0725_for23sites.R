library(dplyr)
library(data.table)
library(reshape)

#DT: DT_15 add group info
#pop:population for rate calculation POP:mean pop2010-2015, POPs:standard pop 2000 add group info

#running full this code can produce one region compare and one race compare at the same time
#for diff gender, make sure change pop for male and femal at both region and race code.

setwd("E:/job/interm/CA rate/")

DT<- read.csv(file = 'DT.csv')
pop<- read.csv("pop.csv")

#remove duplicate tumor
sDT<-unique(setDT(DT),by=c("PatientID","PSite","DxDate"))
#use following if your research interests treat cases with same id ,same psite come in same year once.
#DT$year<-substring(DT$DxDate,1,4)
#sDT<-unique(setDT(DT),by=c("PatientID","PSite","year"))

# only serious case
#limit to serious case--> sDT is a data set with only serious cases and no replicated tumor
sDT$ICO3<-ifelse(sDT$Cancer=="bladder"& (sDT$BehaviorICDO3==2|sDT$BehaviorICDO3==3),1,ifelse(sDT$BehaviorICDO3==3,1,0))
sDT<-subset(sDT,sDT$ICO3==1)
#### US 2000 standard popoulation(produce popjs, totalpopjs);
pop_2000 <- c(18986520, 19919840,  20056779,	19819518, 18257225, 17722067, 19511370,	 22179956,  22479229,  19805793, 17224359, 13307234,	 10654272, 9409940, 8725574, 7414559,  4900234, 4259173)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
sum_pop2000<-sum(pop_2000)
popjs<-cbind((agelabels),melt(pop_2000))
popjs$sumpopjs<-sum(pop_2000)
colnames(popjs)[1] <-"Age"
colnames(popjs)[2] <-"popjs"

##focus on catchment-->DT1 is a data set only include catchment area  ###
DT1<-subset(sDT,sDT$catchment == "catchment")
popc<-subset(pop,pop$catchment =="catchment") 
DT2<-subset(sDT,sDT$catchment != "catchment")
pop2<-subset(pop,pop$catchment !="catchment") 

##########################
#
#total cancer by Region group (catchment)
#
###########################
##Group1
###########
##rural=="rural"/ "metro"                #     
##Apstatus=="Appa"/"nonAp"               #
##Group1###
#
###
# "bladder","brain_oth_nervous","breast","cervical","colorectal","corpus_uterus",
#"esophagus","hodgkin_lymph","kidney_renal_pelvis","larynx","leukemia","liver_ibd",
#"lung_bronchus","melanoma","myeloma","non_hodgkin_lymphoma","oral_pharyn","ovary",
#"pancreas","prostate","stomach","testis","thyroid"

#Mcancer <-c("testis","prostate")
#Fcancer <-c("corpus_uterus","cervical","ovary","breast")

## 3 places need to change cancer sites 
## adjusted code for different gender:3 places to chance (sex=1 or 2) and 7 places to change(each group+PA) popj=>Tmale, Tfmale 

##################################################
DTpa<-subset(sDT,sDT$Cancer=="thyroid") # this is for PA, last part of the code
DTpa<-subset(DTpa,DTpa$Sex==2)
DT<-subset(DT1,DT1$Cancer=="thyroid")
DT<-subset(DT,DT$Sex==2)
################################################
subDT<-DT
subpop<-popc
###data#####
t1 <- table(Age=subDT$agegroups)
t2<-melt(t1)
##population##
##
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"
Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
#total1$popj<- total1$Total  ###Total
#total1$popj<-total1$Tmale ###male
total1$popj<-total1$Tfmale####femal
group1<- dsrTest(total1$value, total1$popj, total1$popjs, method = "gamma"  , mult = 1e5)

############
###Group2###
############
subDT<-subset(DT2,DT2$Cancer=="thyroid")
subDT<-subset(subDT,subDT$Sex==2)
subpop<-pop2
###data#####
t1 <- table(Age=subDT$agegroups)
t2<-melt(t1)

##population##
##
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"
Total2<- merge(t2,pop1,by=c("Age"))
total2<-merge(Total2,popjs,by=c("Age"))
#total2$popj<- total2$Total  ###Total
#total2$popj<-total2$Tmale ###male
total2$popj<-total2$Tfmale####femal
group2<-dsrTest(total2$value, total2$popj, total2$popjs, method = "gamma"  , mult = 1e5)

####################
#hand calculate
####################
total1$v<-with(total1,value/(popj^2)*(popjs/sumpopjs)^2)
total1$ajrate<- with(total1,(value/popj)*(popjs/sumpopjs))


total2$v<-with(total2,value/(popj^2)*(popjs/sumpopjs)^2)
total2$ajrate<- with(total2,(value/popj)*(popjs/sumpopjs))

ajrate1=sum(total1$ajrate)*100000
ajrate2=sum(total2$ajrate)*100000
SRR=(sum(total1$ajrate)/sum(total2$ajrate))
var<-sum(total1$v)/(sum(total1$ajrate))^2+sum(total2$v)/(sum(total2$ajrate))^2
CIlow= exp(log(SRR)-1.96*sqrt(var))
CIup= exp(log(SRR)+1.96*sqrt(var))

rr1<-t(c(0,group2$estimate,SRR,CIlow,CIup))
a1<-t(c(sum(total1$value),sum(total1$popj),group1$estimate,group1$conf.int[1],group1$conf.int[2]))
b1<-t(c(sum(total2$value),sum(total2$popj),group2$estimate,group2$conf.int[1],group2$conf.int[2]))

#####
#
#Group APPA
#
#####
#DT<-subset(DT,DT$Sex==1)
subDT<-subset(DT,Apstatus=="Appa")   ### Add this when Regoin Group,close when total  
subpop<-subset(popc,Apstatus=="Appa") ### Add this when Regoin Group,close when total

###data#####
t1 <- table(Age=subDT$agegroups)
t2<-melt(t1)
##population##
##
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"
Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
#total1$popj<- total1$Total  ###Total
#total1$popj<-total1$Tmale ###male
total1$popj<-total1$Tfmale####femal
group1<- dsrTest(total1$value, total1$popj, total1$popjs, method = "gamma"  , mult = 1e5)

############
###Group2###
############

subDT<-subset(DT,Apstatus=="nonAp")   ### Add this when Regoin Group,close when total 
subpop<-subset(popc,Apstatus=="nonAp") ### Add this when Regoin Group,close when total
#
###data#####
t1 <- table(Age=subDT$agegroups)
t2<-melt(t1)

##population##
##
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"
Total2<- merge(t2,pop1,by=c("Age"))
total2<-merge(Total2,popjs,by=c("Age"))
#total2$popj<- total2$Total  ###Total
#total2$popj<-total2$Tmale ###male
total2$popj<-total2$Tfmale####femal
group2<-dsrTest(total2$value, total2$popj, total2$popjs, method = "gamma"  , mult = 1e5)

####################
#hand calculate
####################
total1$v<-with(total1,value/(popj^2)*(popjs/sumpopjs)^2)
total1$ajrate<- with(total1,(value/popj)*(popjs/sumpopjs))


total2$v<-with(total2,value/(popj^2)*(popjs/sumpopjs)^2)
total2$ajrate<- with(total2,(value/popj)*(popjs/sumpopjs))

ajrate1=sum(total1$ajrate)*100000
ajrate2=sum(total2$ajrate)*100000
SRR=(sum(total1$ajrate)/sum(total2$ajrate))
var<-sum(total1$v)/(sum(total1$ajrate))^2+sum(total2$v)/(sum(total2$ajrate))^2
CIlow= exp(log(SRR)-1.96*sqrt(var))
CIup= exp(log(SRR)+1.96*sqrt(var))

rr2<-t(c(0,group2$estimate,SRR,CIlow,CIup))
a2<-t(c(sum(total1$value),sum(total1$popj),group1$estimate,group1$conf.int[1],group1$conf.int[2]))
b2<-t(c(sum(total2$value),sum(total2$popj),group2$estimate,group2$conf.int[1],group2$conf.int[2]))

##############
#
# Group Rural
#
##############
subDT<-subset(DT,rural=="rural")   ### Add this when Regoin Group,close when total  
subpop<-subset(popc,rural=="rural") ### Add this when Regoin Group,close when total

###data#####
t1 <- table(Age=subDT$agegroups)
t2<-melt(t1)
##population##
##
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"
Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
#total1$popj<- total1$Total  ###Total
#total1$popj<-total1$Tmale ###male
total1$popj<-total1$Tfmale####femal
group1<- dsrTest(total1$value, total1$popj, total1$popjs, method = "gamma"  , mult = 1e5)

############
###Group2###
############

subDT<-subset(DT,rural=="metro")   ### Add this when Regoin Group,close when total 
subpop<-subset(popc,rural=="metro") ### Add this when Regoin Group,close when total
#
###data#####
t1 <- table(Age=subDT$agegroups)
t2<-melt(t1)

##population##
##
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"
Total2<- merge(t2,pop1,by=c("Age"))
total2<-merge(Total2,popjs,by=c("Age"))
#total2$popj<- total2$Total  ###Total
#total2$popj<-total2$Tmale ###male
total2$popj<-total2$Tfmale####femal
group2<-dsrTest(total2$value, total2$popj, total2$popjs, method = "gamma"  , mult = 1e5)

####################
#hand calculate
####################
total1$v<-with(total1,value/(popj^2)*(popjs/sumpopjs)^2)
total1$ajrate<- with(total1,(value/popj)*(popjs/sumpopjs))


total2$v<-with(total2,value/(popj^2)*(popjs/sumpopjs)^2)
total2$ajrate<- with(total2,(value/popj)*(popjs/sumpopjs))

ajrate1=sum(total1$ajrate)*100000
ajrate2=sum(total2$ajrate)*100000
SRR=(sum(total1$ajrate)/sum(total2$ajrate))
var<-sum(total1$v)/(sum(total1$ajrate))^2+sum(total2$v)/(sum(total2$ajrate))^2
CIlow= exp(log(SRR)-1.96*sqrt(var))
CIup= exp(log(SRR)+1.96*sqrt(var))

rr3<-t(c(0,group2$estimate,SRR,CIlow,CIup))
a3<-t(c(sum(total1$value),sum(total1$popj),group1$estimate,group1$conf.int[1],group1$conf.int[2]))
b3<-t(c(sum(total2$value),sum(total2$popj),group2$estimate,group2$conf.int[1],group2$conf.int[2]))

#################
# ALL PA / run three times(total,gender1,gender2) per cancer sites
###############
###data#####
t1 <- table(Age=DTpa$agegroups)
t2<-melt(t1)
##population##
subpop<-pop
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"
Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
#total1$popj<- total1$Total  ###Total
#total1$popj<-total1$Tmale ###male
total1$popj<-total1$Tfmale####femal
c<- dsrTest(total1$value, total1$popj, total1$popjs, method = "gamma"  , mult = 1e5)
c<-t(c(sum(total1$value),sum(total1$popj),c$estimate,c$conf.int[1],c$conf.int[2] ))

tt3<-rbind(a1,b1,a3,b3,a2,b2,c,rr1,rr3,rr2)
write.csv(tt3,"E:/job/interm/CA rate/temp.csv")
