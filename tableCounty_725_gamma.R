library(dplyr)
library(data.table)
library(reshape)

#DT: our data
#pop:population for rate calculation POP:mean pop2010-2015, POPs:standard pop 2000
setwd("E:/job/interm/CSHO_data/")

DT<- read.csv(file = 'DT11_15.csv')
pop<- read.csv("pop5.csv")
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
DT$Latino<- ifelse(DT$Hispanic %in% c(1,2,3,4,5,6,7,8),"Latino",ifelse(DT$Hispanic==0,"NonLatino","Unknown"))

#### group AficanAmerican Race1==2
DT$Afican<-ifelse(DT$Race1==2,"Afican",ifelse(DT$Race1==99,"unknown","Non"))

# remove duplicate id 
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


#############################################
subDT<-sDT ### use this when total cancer for each county area, no other group
#subDT<-subset(sDT,sDT$Cancer=="melanoma")
#subDT<-subset(sDT,sDT$Afican=="Afican")   ### for African American group
subDT<-subset(sDT,sDT$Latino=="Latino")  ### for Latino group
#subDT<-subset(sDT,sDT$Afican=="Non") ### for non African
#subDT<-subset(sDT,sDT$Latino=="NonLatino") ### for non Latino
#############################################


##data##
t1 <- table(Age=subDT$agegroups,County=subDT$County)
t2<-melt(t1,id=c('Age'))

##pop data##
colnames(pop)[2] <-"County"
colnames(pop)[3] <-"Age"

Total1<- merge(t2,pop,by=c("Age","County"))
total1<-merge(Total1,popjs,by=c("Age"))
######################################
## For different Race group, Change"Total" to "Black" or "Latino" (below)
#########################################
#setnames(total1, "Total", "popj")    ### for total
#setnames(total1,"Black","popj")       ### for African American Group
setnames(total1,"Latino","popj")     ### for Latino Group
#total1$popj<-with(total1,Total-Black) ### for non Black group
#total1$popj<-with(total1,Total-Latino)### for non Latino 
##

total1$ajrate<- with(total1,(value/popj)*(popjs/sumpopjs)*100000)
total1$sei<-with(total1,(popjs/sumpopjs)^2*(value/popj^2))
s1<-aggregate(total1$ajrate,by=list(total1$County),FUN='sum')
s2<-aggregate(total1$sei,by=list(total1$County),FUN='sum')
s3<-aggregate(total1$value,by=list(total1$County),FUN='sum')
s4<-aggregate(total1$popj,by=list(total1$County),FUN='sum')

summary<-merge(s1,s2,by=c('Group.1'))
summary<-merge(s4,summary,by=c('Group.1'))
summary<-merge(s3,summary,by=c('Group.1'))
colnames(summary)[1] <-"County"
colnames(summary)[2] <-"No"
colnames(summary)[3] <-"Pop"
colnames(summary)[4] <-"AjRate"
summary$Rate<-with(summary,No/Pop*100000)
summary<-summary[,c(1,2,3,6,4)]
summary

# subset catchment area here
cat_summary<-subset(summary,summary$County %in% catchment_name)
cat_summary


##########################
### Gamma Approach CI ###
####loop for CI gamma approach#####

# Set up result dataframe before the loop
#install.packages("magicfor")
library(magicfor)               # load library
library(dsrTest)
county_name <- c("Adams","Allegheny","Armstrong","Beaver","Bedford","Berks","Blair","Bradford","Bucks","Butler",
                 "Cambria","Cameron","Carbon","Centre","Chester","Clarion","Clearfield","Clinton","Columbia",
                 "Crawford","Cumberland","Dauphin","Delaware","Elk","Erie","Fayette","Forest","Franklin","Fulton","Greene","Huntingdon","Indiana","Jefferson","Juniata","Lackawanna","Lancaster","Lawrence","Lebanon","Lehigh","Luzerne","Lycoming","McKean","Mercer","Mifflin","Monroe","Montgomery","Montour","Northampton","Northumberland","Perry","Philadelphia","Pike","Potter","Schuylkill","Snyder","Somerset","Sullivan","Susquehanna","Tioga",
                 "Union","Venango","Warren","Washington","Wayne","Westmoreland","Wyoming","York")

magic_for(silent = TRUE)

for (x in catchment_name){
  test<-subset(total1,total1$County==x)
  a<-dsrTest(test$value, test$popj, test$popjs, method = "gamma"  , mult = 1e5)
  # df<-data.frame(x,a$conf.int)
  put (a$estimate ,a$conf.int[1], a$conf.int[2])
}

CI<-magic_result_as_dataframe()     # get the result




