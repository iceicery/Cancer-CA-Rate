#install.packages("xlsx")
#install.packages("gtools")
#install.packages("knitr")
#install.packages("reporttools")
#install.packages("xtable")
library(data.table)
library(xlsx)
library(gtools)
library(knitr)
library(reporttools)
library(xtable)
library(dplyr)
library(reshape)
library(plyr)

path_wu= "E:/job/interm/CSHO_data/"

###############organize data for using
############### output DT

##Read into the dataset for quick check;
data_2010 <- read.delim(file=paste(path_wu,"1F0400_2010_upd.txt", sep=""), header=TRUE, sep=",")
data_2011 <- read.delim(file=paste(path_wu,"1F0400_2011_upd.txt", sep=""), header=TRUE, sep=",")
data_2012 <- read.delim(file=paste(path_wu,"1F0400_2012_upd.txt", sep=""), header=TRUE, sep=",")
data_2013 <- read.delim(file=paste(path_wu,"1F0400_2013_upd.txt", sep=""), header=TRUE, sep=",")
data_2014 <- read.delim(file=paste(path_wu,"1F0400_2014_upd.txt", sep=""), header=TRUE, sep=",")
data_2015 <- read.delim(file=paste(path_wu,"1F0400_2015_fullexport.txt", sep=""), header=TRUE, sep=",")

####combine data together;
data_2010_2015 <- smartbind(data_2011, data_2012, data_2013, data_2014, data_2015)
###define the year based on diagnosis date;
#data_2010_2015$year_diag <- apply(as.matrix(data_2010_2015$DxDate), 2, function(x){as.numeric(substring(x, 1, 4))})
#data_2010_2015$year_month_diag <- apply(as.matrix(data_2010_2015$DxDate), 2, function(x){as.numeric(substring(x, 1, 6))})

#recode county name
data_2010_2015$County<- recode(data_2010_2015$DxCounty, '1'= "Adams",	'69'="Lackawanna",'3'= "Allegheny",'71'="Lancaster",'5'="Armstrong",'73'="Lawrence",
                      '7'="Beaver",'75'="Lebanon",'9'="Bedford",'77'="Lehigh",'11'="Berks",'79'="Luzerne",'13'="Blair",'81'="Lycoming",'15'="Bradford",	
                      '83'="McKean",'17'="Bucks",'85'="Mercer",'19'="Butler",'87'="Mifflin",'21'="Cambria",	'89'="Monroe",'23'="Cameron",	'91'="Montgomery",
                      '25'="Carbon",		'93'="Montour",'27'="Centre",'95'="Northampton",'29'="Chester",	'97'="Northumberland",'31'="Clarion",	'99'="Perry",
                      '33'="Clearfield",'101'="Philadelphia",'35'="Clinton",'103'="Pike",'37'="Columbia",'105'="Potter",'39'="Crawford", '107'="Schuylkill",
                      '41'="Cumberland", '109'="Snyder",'43'="Dauphin",'111'="Somerset",'45'="Delaware", '113'="Sullivan",'47'="Elk", '115'="Susquehanna",
                      '49'="Erie", '117'="Tioga",'51'="Fayette", '119'="Union",'53'="Forest", '121'="Venango",'55'="Franklin",'123'="Warren", '57'="Fulton",'125'="Washington",
                      '59'="Greene", '127'="Wayne", '61'="Huntingdon", '129'="Westmoreland",'63'="Indiana", '131'="Wyoming",'65'="Jefferson",'133'="York",'67'="Juniata")


## transform age to agegroup
agebreaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,200)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")
setDT(data_2010_2015)[ ,agegroups := cut(AgeDx, breaks = agebreaks,right = FALSE,labels = agelabels)]

#########define specific cancer types: PSite=C500-C509 for female breast; PSite=C619 for male prostate; PSite=C340-C349 for lung/Bronchus; PSite=C180-C189, C199, C209, C260 while excluding ((histtypeicdo3>=9050 & histtypeicdo3<=9055) | histtypeicdo3==9140 | (histtypeicdo3>=9590 & histtypeicdo3<=9992)) for Colorectal; PSite=C000-C148 for Oral Cavity/Pharynx; PSite=C569 for female Ovary; PSite=C739 for Thyroid; PSite=C530-C539 for female Cervix. 
###note that sex=1 for male and sex=2 for female;
data_2010_2015$exclude <- (data_2010_2015$HistTypeICDO3>=9050 & data_2010_2015$HistTypeICDO3<=9055) | data_2010_2015$HistTypeICDO3==9140 | (data_2010_2015$HistTypeICDO3>=9590 & data_2010_2015$HistTypeICDO3<=9992)

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C180","C181","C182","C183","C184","C185","C186","C187","C188")|data_2010_2015$PSite %in% c("C189","C260","C199","C209") & data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "colorectal"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C500","C501","C502","C503","C504","C505","C506","C507","C508","C509")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=1 & data_2010_2015$BehaviorICDO3==3] <- "breast"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C619")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=2 & data_2010_2015$BehaviorICDO3==3] <- "prostate"

data_2010_2015$Cancer[(data_2010_2015$PSite %in% c("C710","C711","C712","C713","C714","C715","C716","C717","C718","C719","C700","C701","C702","C703","C704","C705","C706","C707","C708","C720","C721","C722","C723","C724","C725","C726","C727","C728","C719","C709","C729") & data_2010_2015$exclude!=1)|(data_2010_2015$PSite %in% c("C710","C711","C712","C713","C714","C715","C716","C717","C718","C719") & data_2010_2015$HistTypeICDO3 >=9530 & data_2010_2015$HistTypeICDO3 <=9539) & data_2010_2015$BehaviorICDO3==3] <- "brain_oth_nervous"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C530","C531","C532","C533","C534","C535","C536","C537","C538","C539")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=1 & data_2010_2015$BehaviorICDO3==3] <- "cervical"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C540","C541","C542","C543","C544","C545","C546","C547","C548","C549","C559")& data_2010_2015$exclude !=1 & data_2010_2015$BehaviorICDO3==3] <- "corpus_uterus"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C150","C151","C152","C153","C154","C155","C156","C157","C158","C159")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "esophagus"

data_2010_2015$Cancer[data_2010_2015$HistTypeICDO3>=9650 & data_2010_2015$HistTypeICDO3 <=9667 & data_2010_2015$BehaviorICDO3==3] <- "hodgkin_lymph"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C649","C659")& data_2010_2015$exclude!=1  & data_2010_2015$BehaviorICDO3==3] <- "kidney_renal_pelvis"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C320","C321","C322","C323","C324","C325","C326","C327","C328","C329")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "larynx"

data_2010_2015$Cancer[(data_2010_2015$PSite %in% c("C420","C421","C424") & data_2010_2015$HistTypeICDO3 %in% c(9811,9812,9813,9814,9815,9816,9817,9818,9837,9823,9827))|data_2010_2015$HistTypeICDO3 %in% c(9826,9835,9836,9820,9832,9833,9834,9940,9840,9861,9865,9866,9867,9869,9871,9872,9873,9874,9895,9896,9897,9898,9910,9911,9920,9891,9863,9875,9876,9945,9946,9860,9930,9801,9805,9806,9807,9808,9809,9931,9733,9742,9800,9831,9870,9948,9963,9964) & data_2010_2015$BehaviorICDO3==3] <- "leukemia"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C340","C341","C342","C343","C344","C345","C346","C347","C348","C349")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "lung_bronchus"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C220","C221")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "liver_ibd"

data_2010_2015$Cancer[data_2010_2015$HistTypeICDO3 %in% c(9731,9732,9734) & data_2010_2015$BehaviorICDO3==3] <- "myeloma"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C440","C441","C442","C443","C444","C445","C446","C447","C448","C449")& data_2010_2015$HistTypeICDO3>=8720 & data_2010_2015$HistTypeICDO3<=8790 & data_2010_2015$BehaviorICDO3==3] <- "melanoma"

data_2010_2015$Cancer[data_2010_2015$HistTypeICDO3 >=9590 & data_2010_2015$HistTypeICDO3 <=9597 | data_2010_2015$HistTypeICDO3 %in% c(9670,9671,9673,9675,9678,9679,9680,9684,9687,9688,9689,9690,9691,9695,9698,9699,9700,9701,9702,9705,9708,9709,9712,9714,9715,9716,9717,9718,9719,9724,9725,9726,9727,9728,9729,9735,9737,9738) |(! data_2010_2015$PSite %in% c("C421","C424")& data_2010_2015$HistTypeICDO3 %in% c(9811,9812,9813,9814,9815,9816,9817,9818,9823,9827,9837))& data_2010_2015$BehaviorICDO3==3] <- "non_hodgkin_lymphoma"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C000","C001","C002","C003","C004","C005","C006","C007","C008","C009","C019","C029","C049","C059","C069","C079","C089","C039","C099","C109","C119","C129","C139","C020","C021","C022","C023","C024","C025","C026","C027","C028","C080","C081","C082","C083","C084","C085","C086","C087","C088","C040","C041","C042","C043","C044","C045","C046","C047","C048","C030","C031","C032","C033","C034","C035","C036","C037","C038","C050","C051","C052","C053","C054","C055","C056","C057","C058","C060","C061","C062","C063","C064","C065","C066","C067","C068","C110","C111","C112","C113","C114","C115","C116","C117","C118","C090","C091","C092","C093","C094","C095","C096","C097","C098","C100","C101","C102","C103","C104","C105","C106","C107","C108","C130","C131","C132","C133","C134","C135","C136","C137","C138","C140","C142","C148") & data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "oral_pharyn"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C569")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=1 & data_2010_2015$BehaviorICDO3==3] <- "ovary"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C250","C251","C252","C253","C254","C255","C256","C257","C258","C259")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "pancreas"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C160","C161","C162","C163","C164","C165","C166","C167","C168","C169")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "stomach"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C620","C621","C622","C623","C624","C625","C626","C627","C628","C629")& data_2010_2015$exclude!=1 & data_2010_2015$Sex!=2 & data_2010_2015$BehaviorICDO3==3] <- "testis"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C739")& data_2010_2015$exclude!=1 & data_2010_2015$BehaviorICDO3==3] <- "thyroid"

data_2010_2015$Cancer[data_2010_2015$PSite %in% c("C670","C671","C672","C673","C674","C675","C676","C677","C678","C679")& data_2010_2015$exclude!=1] <- "bladder"

####
####
####
#export data for future use##
write.csv(data_2010_2015, file = 'E:/job/interm/CSHO_data/DT11_15.csv')

####prepare cencus data for population
####### Readinto the PA census data;




#population data for standard 
##########
###population data
##########
Pop<- read.table("Population_County.csv", header = TRUE, sep=",",fill=TRUE)
#transform 0 to 4 to 0-4
age<-c("0 to 4","5 to 9","10 to 14","15 to 19","20 to 24",
       "25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59",
       "60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85+")
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")
Pop<- Pop[order(Pop$CountyState,Pop$Year,Pop$Sex,Pop$Race),]
Pop$Age<-ifelse(Pop$Age2 %in% age, agelabels,"other")


#outputdata for future use##
write.table(Pop, "E:/job/interm/CSHO_data/pop.txt", sep=",")

