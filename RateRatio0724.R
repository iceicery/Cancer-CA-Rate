library(dplyr)
library(data.table)
library(reshape)

#DT: our data
#pop:population for rate calculation POP:mean pop2010-2015, POPs:standard pop 2000

setwd("E:/job/interm/CSHO_data/")

DT<- read.csv(file = 'DT11_15.csv')
pop<- read.csv("pop5.csv")
##pop data##
colnames(pop)[2] <-"County"
colnames(pop)[3] <-"Age"

county_name <- c("Adams","Allegheny","Armstrong","Beaver","Bedford","Berks","Blair","Bradford","Bucks","Butler",
                 "Cambria","Cameron","Carbon","Centre","Chester","Clarion","Clearfield","Clinton","Columbia","
                 Crawford","Cumberland","Dauphin","Delaware","Elk","Erie","Fayette","Forest","Franklin","Fulton","Greene","Huntingdon","Indiana","Jefferson","Juniata","Lackawanna","Lancaster","Lawrence","Lebanon","Lehigh","Luzerne","Lycoming","McKean","Mercer","Mifflin","Monroe","Montgomery","Montour","Northampton","Northumberland","Perry","Philadelphia","Pike","Potter","Schuylkill","Snyder","Somerset","Sullivan","Susquehanna","Tioga",
                 "Union","Venango","Warren","Washington","Wayne","Westmoreland","Wyoming","York")

#identify region group#################
#####coding dummy variable for grouping########

catchment_name <- c("Adams", "Berks", "Blair", "Carbon","Centre","Clinton","Columbia","Cumberland","Dauphin","Franklin",
                    "Fulton","Huntingdon", "Juniata", "Lancaster", "Lebanon", "Lehigh", "Luzerne", "Lycoming", "Mifflin",
                    "Montour","Northumberland", "Perry", "Schuylkill", "Snyder", "Sullivan", "Union", "Wyoming","York")
DT$catchment<- ifelse(DT$County %in% catchment_name,"catchment","noncatch")
pop$catchment<- ifelse(pop$County %in% catchment_name,"catchment","noncatch")

# group Latino/Hispanic: is defined as hispanic_orig between 1-8. This is separate from race.
DT$Latino<- ifelse(DT$Hispanic %in% c(1,2,3,4,5,6,7,8),"Latino","NonLatino")

#### group AficanAmerican Race1==2
DT$Afican<-ifelse(DT$Race1==2,"Afican","Non")
# identify Applachia and non
Appa <-c("Allegheny", "Armstrong", "Beaver", "Bedford", "Blair", "Bradford", "Butler", 
         "Cambria", "Cameron", "Carbon", "Centre", "Clarion", 
         "Clearfield","Clinton", "Columbia", "Crawford", "Elk", "Erie", "Fayette","Forest", "Fulton", "Greene","Huntingdon",
         "Indiana","Jefferson", "Juniata", "Lackawanna", "Lawrence", "Luzerne", "Lycoming","McKean","Mercer", "Mifflin", 
         "Monroe","Montour","Northumberland", "Perry", "Pike", "Potter", "Schuylkill", "Snyder", 
         "Somerset", "Sullivan","Susquehanna", "Tioga", "Union", "Venango", "Warren", "Washington","Wayne", "Westmoreland","Wyoming")
DT$Apstatus<-ifelse(DT$County %in% Appa, "Appa","nonAp")
pop$Apstatus<-ifelse(pop$County %in% Appa, "Appa","nonAp")

###group metro, rural(based on ruralurbancodes2013_temp)
#Metro - rucc<4                                                                                 
#rural - rucc>=4 area                                                                                                               
metro<-c("Allegheny","Armstrong","Beaver","Bucks","Butler","Chester","Delaware","Fayette","Montgomery","Philadelphia",
         "Pike","Washington","Westmoreland","Berks","Carbon","Cumberland","Dauphin","Erie","Lackawanna","Lancaster",
         "Lehigh","Luzerne","Mercer","Northampton","Perry","Wyoming","York","Adams","Blair","Cambria","Centre",
         "Columbia","Franklin","Lebanon","Lycoming","Monroe","Montour")
rural<-c("Bedford","Bradford","Clarion","Greene","Huntingdon","Juniata","Susquehanna","Tioga","Warren","Wayne","Cameron",
         "Elk","Jefferson","McKean","Snyder","Clearfield","Clinton","Crawford","Indiana","Lawrence","Mifflin",
         "Northumberland","Schuylkill","Somerset","Union","Venango","Fulton","Sullivan","Forest","Potter")
DT$rural<-ifelse(DT$County %in% metro, "metro","rural")
pop$rural<-ifelse(pop$County %in% metro, "metro","rural")

#remove duplicate tumor
DT$year<-substring(DT$DxDate,1,4)
sDT<-unique(setDT(DT),by=c("PatientID","PSite","year"))
# only serious case
#limit to serious case--> sDT is a data set with only serious cases and no replicated tumor
sDT$ICO3<-ifelse(sDT$Cancer=="bladder"& (sDT$BehaviorICDO3==2|sDT$BehaviorICDO3==3),1,ifelse(sDT$BehaviorICDO3==3,1,0))
sDT<-subset(sDT,sDT$ICO3==1)
##focus on catchment-->DT1 is a data set only include catchment area  ###
DT1<-subset(sDT,sDT$catchment == "catchment")
popc<-subset(pop,pop$catchment =="catchment") 
DT2<-subset(sDT,sDT$catchment != "catchment")
pop2<-subset(pop,pop$catchment !="catchment") 

#### US 2000 standard popoulation(produce popjs, totalpopjs);
pop_2000 <- c(18986520, 19919840,  20056779,	19819518, 18257225, 17722067, 19511370,	 22179956,  22479229,  19805793, 17224359, 13307234,	 10654272, 9409940, 8725574, 7414559,  4900234, 4259173)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
sum_pop2000<-sum(pop_2000)
popjs<-cbind((agelabels),melt(pop_2000))
popjs$sumpopjs<-sum(pop_2000)
colnames(popjs)[1] <-"Age"
colnames(popjs)[2] <-"popjs"

##########################
#total cancer by Region group
###########################
##Group1
###########
##rural=="rural"/ "metro"                #     
##Apstatus=="Appa"/"nonAp"               #
##Group1###
subDT<-DT1
#subDT<-subset(subDT,subDT$Cancer=="melanoma")
#subDT<-subset(subDT,subDT$Sex==1)
subDT<-subset(subDT,Apstatus=="Appa")   ### Add this when Regoin Group,close when total  
subpop<-popc
subpop<-subset(subpop,Apstatus=="Appa") ### Add this when Regoin Group,close when total
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
Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
total1$popj<- total1$Total  ###Total

############
###Group2###
############
subDT<-DT1
#subDT<-subset(subDT,subDT$Cancer=="melanoma")
#subDT<-subset(subDT,subDT$Sex==1)
subDT<-subset(subDT,Apstatus=="nonAp")   ### Add this when Regoin Group,close when total 
subpop<-popc
subpop<-subset(subpop,Apstatus=="nonAp") ### Add this when Regoin Group,close when total
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
total2$popj<- total2$Total  ###Total

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

ajrate1
ajrate2
SRR
CIlow
CIup

###############
#using package popEpi 
#the CI is very narrow, some problems with the package
##################
#install.packages("popEpi")
#library(popEpi)
#total1$popj<-as.numeric(total1$popj)
#total1$value<-as.numeric(total1$value)

#total2$popj<-as.numeric(total2$popj)
#total2$value<-as.numeric(total2$value)

#a<-rate(total1, obs = total1$value, pyrs = total1$popj, adjust =total1$Age,
#       weights = total1$popjs )
#b<-rate(total2, obs = total2$value, pyrs = total2$popj, adjust =total2$Age,
#      weights = total2$popjs )
#rate_ratio(a, b, crude = FALSE, SE.method = TRUE)

##############################################
#
#  Code below is for total cancer(Race group)
#
##############################################
#######specified group###########
##Afican=="Afican/Non"         #     
##Latino=="Latino/NonLatino"   #
##focus on catchment-->DT1 is a data set only include catchment area  ###

###################################
##
##Gourp1
##
###############################################
subDT<-DT1#
#subDT<-subset(subDT,subDT$Cancer=="melanoma")
#subDT<-subset(subDT,subDT$Sex==1)
#subDT<-subset(subDT,subDT$Afican=="Afican") #
#subDT<-subset(subDT,subDT$Latino=="Latino") #
subpop<-popc

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

#
#
# Race pop
##############

total1$popj<- total1$Total  ###Total
#total1$popj<-total1$Bfmale  ###Black
#total1$popj<-total1$Latino ###Latino 

###################################
##
##Gourp2
##
###############################################
subDT<-DT2 
#subDT<-subset(subDT,subDT$Cancer=="melanoma")
#subDT<-subset(subDT,subDT$Sex==1) #
#subDT<-subset(subDT,subDT$Afican=="Non") #
#subDT<-subset(subDT,subDT$Latino=="Latino") #
subpop<-pop2

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

#
#
# Race pop
##############
#total2$popjt<-total2$Total ### non Group need this
total2$popj<-total2$Total  ###pop of group2
 
##############
#Non Race pop
##############
#total2$popj<-with(total2,popjt-popj) ###non Group need this
####################

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

ajrate1
ajrate2
SRR
CIlow
CIup


