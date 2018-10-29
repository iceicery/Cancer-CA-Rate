
setwd("E:/job/interm/CSHO_data/")

DT<- read.csv(file = 'DT11_15.csv')
pop<- read.csv(file = 'pop5.csv')
colnames(pop)[2] <-"County"
colnames(pop)[3] <-"Age"

#identify region group#################
#####coding dummy variable for grouping########

catchment_name <- c("Adams", "Berks", "Blair", "Carbon","Centre","Clinton","Columbia","Cumberland","Dauphin","Franklin",
                    "Fulton","Huntingdon", "Juniata", "Lancaster", "Lebanon", "Lehigh", "Luzerne", "Lycoming", "Mifflin",
                    "Montour","Northumberland", "Perry", "Schuylkill", "Snyder", "Sullivan", "Union", "Wyoming","York")
DT$catchment<- ifelse(DT$County %in% catchment_name,"catchment","noncatch")
pop$catchment<- ifelse(pop$County %in% catchment_name,"catchment","noncatch")

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

# group Latino/Hispanic: is defined as hispanic_orig between 1-8. This is separate from race.
DT$Latino<- ifelse(DT$Hispanic %in% c(1,2,3,4,5,6,7,8),"Latino",ifelse(DT$Hispanic==0,"NonLatino","Unknown"))

#### group AficanAmerican Race1==2
DT$Afican<-ifelse(DT$Race1==2,"Afican",ifelse(DT$Race1==99,"unknown","Non"))


#remove duplicate tumor
sDT<-unique(setDT(DT),by=c("PatientID","PSite","DxDate"))
#use following if your research interests treat cases with same id ,same psite come in same year once.
#DT$year<-substring(DT$DxDate,1,4)
#sDT<-unique(setDT(DT),by=c("PatientID","PSite","year"))

# only serious case
#limit to serious case--> sDT is a data set with only serious cases and no replicated tumor
sDT$ICO3<-ifelse(sDT$Cancer=="bladder"& (sDT$BehaviorICDO3==2|sDT$BehaviorICDO3==3),1,ifelse(sDT$BehaviorICDO3==3,1,0))
sDT<-subset(sDT,sDT$ICO3==1)

##focus on catchment-->DT1 is a data set only include catchment area  ###
DT1<-subset(sDT,sDT$catchment != "catchment")
popc<-subset(pop,pop$catchment !="catchment") 

######################################
#
# hand caculation normal CI approach
#
#######################################

#### US 2000 standard popoulation(produce popjs, totalpopjs)--> popjs includes populaton and all age adn total population in US 2000;
pop_2000 <- c(18986520, 19919840,  20056779,	19819518, 18257225, 17722067, 19511370,	 22179956,  22479229,  19805793, 17224359, 13307234,	 10654272, 9409940, 8725574, 7414559,  4900234, 4259173)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
#sum_pop2000<-sum(pop_2000)
popjs<-cbind((agelabels),melt(pop_2000))
popjs$sumpopjs<-sum(pop_2000)
colnames(popjs)[1] <-"Age"
colnames(popjs)[2] <-"popjs"
#######
#####data#######
###get different group with 23 cancer sites 
###
###


##############################################
#
#  Code below is for Region Group
#
##############################################

#######specified group here ###########
##rural=="rural"/ "metro"                #     
##Apstatus=="Appa"/"nonAp"               #
subDT<-DT1
#subDT<-subset(subDT,Apstatus=="nonAp")   ### Add this when Regoin Group,close when total  
subpop<-popc
#subpop<-subset(subpop,Apstatus=="nonAp") ### Add this when Regoin Group,close when total
                                        #
###################################

###data#####
t1 <- table(Age=subDT$agegroups,Cancer=subDT$Cancer)
t2<-melt(t1,id=c('Age'))

##population##
##
Mcancer <-c("testis","prostate")
Fcancer <-c("corpus_uterus","cervical","ovary","breast")

#Mcancer <-c("Testis","Prostate")
#Fcancer <-c("Corpus_uterus","cervical","Ovary","Breast")
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"

Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
total1$popj<- ifelse(total1$Cancer %in% Mcancer,total1$Tmale,ifelse(total1$Cancer %in% Fcancer,total1$Tfmale,total1$Total))
total1$ajrate<- with(total1,(value/popj)*(popjs/sumpopjs)*100000)
total1$sei<-with(total1,(popjs/sumpopjs)^2*(value/popj^2))
summary<-aggregate(list(N=total1$value,Pop=total1$popj,aaRate=total1$ajrate,SE=total1$sei),by=list(total1$Cancer),FUN='sum')

summary$low <- with(summary,aaRate- 1.96*100000*sqrt(SE))
summary$up <-with(summary,aaRate+1.96*100000*sqrt(SE))
summary$Rate<-with(summary,N/Pop*100000)
summary<-subset(summary, select = -c(SE))
colnames(summary)[1]<-"Cancer"
summary<-summary[c(1,2,3,7,4,5,6)]
summary
##########################
#total cancer by Region group
###########################
##rural=="rural"/ "metro"                #     
##Apstatus=="Appa"/"nonAp"               #
subDT<-sDT
subDT<-subset(subDT,Apstatus=="nonAp")   ### Add this when Regoin Group,close when total  
subpop<-pop
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
Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
total1$popj<- total1$Total  ###Total

###using package dsrTest
#install.packages("dsrTest")
library(dsrTest)
dsrTest(total1$value, total1$popj, total1$popjs, method = "asym", mult = 1e5)

##crude rate w/o adjustment
N1 <-sum(total1$value)
Pop1<-sum(total1$popj)
rate<-N1/Pop1*100000
N1
Pop1
rate

##############################################
#
#  Code below is for Race Group
#
##############################################

#######specified group###########
##Afican=="Afican/Non"         #     
##Latino=="Latino/NonLatino"   #
##focus on catchment-->DT1 is a data set only include catchment area  ###
DT1<-subset(sDT,sDT$catchment != "catchment")
popc<-subset(pop,pop$catchment !="catchment") 
###############################################
subDT<-DT1                                    #
#subDT<-subset(subDT,subDT$Afican=="Afican") #
subDT<-subset(subDT,subDT$Latino=="Latino") #
subpop<-popc
###################################

###data#####
t1 <- table(Age=subDT$agegroups,Cancer=subDT$Cancer)
t2<-melt(t1,id=c('Age'))

##population##
##
Mcancer <-c("testis","prostate")
Fcancer <-c("corpus_uterus","cervical","ovary","breast")

#Mcancer <-c("Testis","Prostate")
#Fcancer <-c("Corpus_uterus","cervical","Ovary","Breast")
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
colnames(pop1)[1]<-"Age"

Total1<- merge(t2,pop1,by=c("Age"))
total1<-merge(Total1,popjs,by=c("Age"))
total1$popjt<-ifelse(total1$Cancer %in% Mcancer,total1$Tmale,ifelse(total1$Cancer %in% Fcancer,total1$Tfmale,total1$Total))
############################################################################
#Race pop
##############
#total1$popj<- ifelse(total1$Cancer %in% Mcancer,total1$Bmale,ifelse(total1$Cancer %in% Fcancer,total1$Bfmale,total1$Black))
total1$popj<- ifelse(total1$Cancer %in% Mcancer,total1$Lmale,ifelse(total1$Cancer %in% Fcancer,total1$Lfmale,total1$Latino))
##############
#Non Race pop
##############
#total1$popj<-with(total1,popjt-popj)
#############################################################################
total1$ajrate<- with(total1,(value/popj)*(popjs/sumpopjs)*100000)
total1$sei<-with(total1,(popjs/sumpopjs)^2*(value/popj^2))
summary<-aggregate(list(N=total1$value,Pop=total1$popj,aaRate=total1$ajrate,SE=total1$sei),by=list(total1$Cancer),FUN='sum')

summary$low <- with(summary,aaRate- 1.96*100000*sqrt(SE))
summary$up <-with(summary,aaRate+1.96*100000*sqrt(SE))
summary$Rate<-with(summary,N/Pop*100000)
summary<-subset(summary, select = -c(SE))
colnames(summary)[1]<-"Cancer"
summary<-summary[c(1,2,3,7,4,5,6)]
summary


##############################################
#
#  Code below is for total cancer(Race group)
#
##############################################
#######specified group###########
##Afican=="Afican/Non"         #     
##Latino=="Latino/NonLatino"   #
##focus on catchment-->DT1 is a data set only include catchment area  ###
DT1<-subset(sDT,sDT$catchment != "catchment")
popc<-subset(pop,pop$catchment !="catchment") 
###############################################
subDT<-DT1                                    #
#subDT<-subset(subDT,subDT$Afican=="Afican") #
subDT<-subset(subDT,subDT$Latino=="Latino") #
subpop<-popc
###################################

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

#
#
# Race pop
##############
total1$popjt<-total1$Total ### non Group need this
#total1$popj<- total1$Total  ###Total
#total1$popj<-total1$Black  ###Balek
total1$popj<-total1$Latino ###Latino 
##############
#Non Race pop
##############
#total1$popj<-with(total1,popjt-popj) ###non Group need this
#############################################################################
###using package dsrTest
#install.packages("dsrTest")
library(dsrTest)
dsrTest(total1$value, total1$popj, total1$popjs, method = "asym", mult = 1e5)

##crude rate w/o adjustment
N1 <-sum(total1$value)
Pop1<-sum(total1$popj)
rate<-N1/Pop1*100000
N1
Pop1
rate


#######
#plot CI
#######
library(ggplot2)

ggplot(summary, aes(x = Cancer, y = AjRate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = up, ymin = low))

##############
#test
##############
test<-subset(pop,pop$Race=="Black")
test<-subset(test,test$CountyState !="Pennsylvania")
sum(test$POP)

##########
#Age distribution
##########
subpop<-pop
#subpop<-subset(pop,pop$rural=="rural")
#subpop<-subset(pop,pop$Apstatus=="Appa")
pop1<- aggregate(list(Bmale=subpop$Bmale, Bfmale=subpop$Bfmale,Black=subpop$Black, 
                      Lmale=subpop$Lmale,Lfmale=subpop$Lfmale,Latino=subpop$Latino,
                      Tmale=subpop$Tmale,Tfmale=subpop$Tfmale,Total=subpop$Total),by=list(subpop$Age),FUN='sum')
pop1
