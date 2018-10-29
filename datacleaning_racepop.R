setwd("E:/job/interm/CSHO_data/")
pop_race<- read.csv(file='racepop.csv')
#YEAR: 3:2010(estimate)  4:2011 5:2012 .... 8:2015
#AGEGRP:0=Total, 1=0-4,2=5-9,.....18=85+
#COUNTY:FIPS code
#CIYNAME:COUNTY NAME: Cambria County ...
#TOT_POP:total population, TOT_MALE, TOT_FEMALE
#
###following is formatting match the calculation code####
### format county name--> CountyState
library(stringr)
pop_race$CountyState<-str_split_fixed(pop_race$CTYNAME, " ", 2)[,1]
### format AGEGRP --> Age
#omit total
pop_race<-subset(pop_race,pop_race$AGEGRP!=0)
#label
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")
pop_race<- pop_race[order(pop_race$CountyState,pop_race$YEAR),]
pop_race$Age<-ifelse(pop_race$AGEGRP %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), agelabels,"NA")

#total pop for  black and latino
pop_race$black<-with(pop_race,BA_MALE+BA_FEMALE)
pop_race$latino<-with(pop_race,H_MALE+H_FEMALE)

#p6:is 2010-2015 
#p5:is 2011-2015
p5<-subset(pop_race,pop_race$YEAR %in% c(4,5,6,7,8))
p6<-subset(pop_race,pop_race$YEAR %in% c(3,4,5,6,7,8))

p5_t <- aggregate(list(Bmale=p5$BA_MALE, Bfmale=p5$BA_FEMALE,Black=p5$black, 
                       Lmale=p5$H_MALE,Lfmale=p5$H_FEMALE,Latino=p5$latino,
                       Tmale=p5$TOT_MALE,Tfmale=p5$TOT_FEMALE,Total=p5$TOT_POP), by = list(p5$CountyState,p5$Age), sum)
p6_t <- aggregate(list(Bmale=p6$BA_MALE, Bfmale=p6$BA_FEMALE,Black=p6$black, 
                       Lmale=p6$H_MALE,Lfmale=p6$H_FEMALE,Latino=p6$latino,
                       Tmale=p6$TOT_MALE,Tfmale=p6$TOT_FEMALE,Total=p6$TOT_POP), by = list(p6$CountyState,p6$Age), sum)
write.csv(p5_t, file = 'E:/job/interm/CSHO_data/pop5.csv')
write.csv(p6_t, file = 'E:/job/interm/CSHO_data/pop6.csv')

