rm(list = ls())
library(sqldf)
library(plyr) # for data cleaning
library("readxl")
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(reshape2)
library(zoo)
library(readr)
library(dplyr)
library(MASS)
library(car)
library(caTools)

##Raw data of Customer's Billing Details
totalbill<-read.csv("D:/surveyc/bill.csv")

names(totalbill)<-gsub(" ","_",fixed=TRUE,names(totalbill))
names(totalbill)<-gsub(".","_",fixed=TRUE,names(totalbill))
names(totalbill)<-gsub("survey_s_","",fixed=TRUE,names(totalbill))

names(totalbill)

totalbill$posting_date=as.character(totalbill$posting_date)
totalbill$start_bill_period=as.character(totalbill$start_bill_period)
totalbill$end_bill_period=as.character(totalbill$end_bill_period)

totalbill$posting_date=as.Date(totalbill$posting_date,format = "%Y%m%d")
totalbill$start_bill_period=as.Date(totalbill$start_bill_period,format="%Y%m%d")
totalbill$end_bill_period=as.Date(totalbill$end_bill_period,format="%Y%m%d")

str(totalbill)

totalbill$year=format(totalbill$posting_date,"%Y")
totalbill$month_start=format(totalbill$start_bill_period,"%m")
totalbill$month_end=format(totalbill$end_bill_period,"%m")

totalbill$year=as.numeric(totalbill$year)
totalbill$month_start=as.numeric(totalbill$month_start)
totalbill$month_end=as.numeric(totalbill$month_end)


totalbill$days<-difftime(totalbill$end_bill_period,totalbill$start_bill_period,units = "days")
totalbill$originaldays_start<-ifelse(totalbill$month_start %in% c(1,3,5,7,8,10,12) & !totalbill$month_start==2,31,
                                     ifelse(!totalbill$month_start %in% c(1,3,5,7,8,10,12) & !totalbill$month_start==2,30,28))

totalbill$originaldays_end<-ifelse(totalbill$month_end %in% c(1,3,5,7,8,10,12) & !totalbill$month_end==2,31,
                                   ifelse(!totalbill$month_end %in% c(1,3,5,7,8,10,12) & !totalbill$month_end==2,30,28))

totalbill$days=as.numeric(totalbill$days)
totalbill$originaldays_start=as.numeric(totalbill$originaldays_start)
totalbill$originaldays_end=as.numeric(totalbill$originaldays_end)
totalbill$billperday=ifelse(totalbill$billed_unit!=0,totalbill$billed_unit/totalbill$days,0)
totalbill$bill_start=abs((totalbill$billperday)*(totalbill$originaldays_start-as.numeric(format(totalbill$start_bill_period,"%d"))))
totalbill$bill_end=abs((totalbill$billperday)*(as.numeric(format(totalbill$end_bill_period,"%d"))))

##Master Data of customers
master<-read.csv("D:/surveyc/master.csv")

totalbill<-merge(totalbill,master,by="ca",all.x=TRUE)
totalbill$move_in_date<-as.character(totalbill$move_in_date)
totalbill$move_in_date<-as.Date(totalbill$move_in_date,"%Y%m%d")
totalbill$time_tpddl<-(Sys.Date()-totalbill$move_in_date)/365
totalbill$ca=as.character(totalbill$ca)

totalbill2<-totalbill[c(1,3,4,6,9,12,14,28,29,30,31,34,35,36,37,40,41)]
totalbill2<-totalbill2[!is.na(totalbill2$start_bill_period),]
totalbill2<-totalbill2[!totalbill2$days==0,]


totalbill2$time_tpddl=as.numeric(totalbill2$time_tpddl)
sorted_total<-sqldf("select * from totalbill2 order by CA,year,month_start ")

sorted_total$add_bill<-c()
sorted_total$add_bill<-lapply(1:NROW(sorted_total),function(i){
  ifelse(sorted_total$ca[i]==sorted_total$ca[i+1] & sorted_total$year[i]==sorted_total$year[i+1] &
           sorted_total$month_end[i]==sorted_total$month_start[i+1],
         sorted_total$bill_end[i]+sorted_total$bill_start[i+1], sorted_total$bill_end[i])
})



sorted_total$add_bill_s<-c()

sorted_total$add_bill_s<-lapply(1:NROW(sorted_total),
                                function(i){ifelse(sorted_total$ca[i-1]==sorted_total$ca[i] & sorted_total$year[i-1]!=sorted_total$year[i] & sorted_total$month_end[i-1]!=sorted_total$month_start[i],
                                                   sorted_total$bill_start[i],0)})

sorted_total$add_bill_s[1]<-sorted_total$bill_start[1]
sorted_total$add_bill_s=as.numeric(sorted_total$add_bill_s)
sorted_total$add_bill=as.numeric(sorted_total$add_bill)

summary(sorted_total$add_bill)
summary(sorted_total$add_bill_s)

sorted_total<-sorted_total[!is.na(sorted_total$add_bill),]

summary1=sqldf("select ca,year,month_end, month_start,avg(abs(add_bill)) as Avg_bill_end, avg(abs(add_bill_s)) as Avg_bill_start from sorted_total group by ca, year,month_end")


summary1$Avg_bill_end<-lapply(1:NROW(summary1), function(i){
  ifelse(summary1$ca[i]==summary1$ca[i+1] & summary1$year[i]==summary1$year[i+1] & summary1$month_end[i]==summary1$month_start[i+1] & summary1$Avg_bill_end[i+1]!=0.0000,summary1$Avg_bill_end[i]+summary1$Avg_bill_start[i+1],
         summary1$Avg_bill_end[i])    
})

summary1$Avg_bill_end<-as.numeric(summary1$Avg_bill_end)
summary(summary1$Avg_bill_end)
summary1$Avg_bill_end<-as.numeric(summary1$Avg_bill_end)

#summary2<-summary1[!summary1$Avg_bill_end==0 & !is.na(summary1$Avg_bill_end),]
summary2<-filter(summary1,!summary1$Avg_bill_end<1 & !is.na(summary1$Avg_bill_end))
summary(summary2$Avg_bill_end)

#uni=unique(summary$CA)
summary2$time<-paste(summary2$year,summary2$month_end)
a=dummy(summary2$time)
s1=cbind(summary2[!is.na(summary2$month_end),],a)
names(s1)<-gsub(" ","_",names(s1))
s2<-s1
names(s2)
s2[c(8:97)]<-s2[["Avg_bill_end"]]*s2[c(8:97)]

s3<-aggregate(s2[,c(8:97)],by=list(s2$ca),sum)
names(s3)
s3<-s3[-c(2:35,84:91)]


names(s3)

#Function for Data Cleaning of variables of dataframe
var_Summ=function(x){
  if(class(x)=="numeric"|class(x)=="int"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=mean(x,na.rm=T)+2*sd(x,na.rm=T)
    LC2=mean(x,na.rm=T)+2*sd(x,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,LC1_=LC1,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC1= UC1, UC2= UC2,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

num_var1=sapply(s3,is.numeric)
Other_var1=!sapply(New_cust,is.numeric)

my_num_data0<-t(data.frame(apply(s3[num_var1], 2, var_Summ)))

#Outlier Capping Function by 95 percentiles.


s4<-s3

M1_fun <- function(x){
  quantiles <- quantile( x,.50,na.rm=TRUE )
  x<-ifelse(x<quantiles,quantiles,x)
  
}


names(s4)
#s4$time2015_10<-ifelse(s4$time2015_10<quantile( s4$time2015_10, .0375,na.rm=TRUE ),quantile( s4$time2015_10, .0375,na.rm=TRUE ),s4$time2015_10)
s4[2:49]<-sapply(s4[2:49],function(x){M1_fun(x)})

##Feature Extraction
##Avg Bill and Percentage Change in Bills from previous months,last year, etc.
s4$avg_bill_4yrs<-rowMeans(s4[2:49])
summary(s4$avg_bill_4yrs)
names(s4)
colnames(s4)<-paste("time",colnames(s4))
names(s4)<-gsub(" ","",names(s4))

s4$change_perc_jan1<-(s4$time2014_1-s4$time2015_1)/s4$time2014_1
s4$change_perc_feb1<-(s4$time2014_2-s4$time2015_2)/s4$time2014_2
s4$change_perc_mar1<-(s4$time2014_3-s4$time2015_3)/s4$time2014_3
s4$change_perc_apr1<-(s4$time2014_4-s4$time2015_4)/s4$time2014_4
s4$change_perc_may1<-(s4$time2014_5-s4$time2015_5)/s4$time2014_5
s4$change_perc_jun1<-(s4$time2014_6-s4$time2015_6)/s4$time2014_6
s4$change_perc_jul1<-(s4$time2014_7-s4$time2015_7)/s4$time2014_7
s4$change_perc_aug1<-(s4$time2014_8-s4$time2015_8)/s4$time2014_8
s4$change_perc_sep1<-(s4$time2014_9-s4$time2015_9)/s4$time2014_9

s4$change_perc_jan2<-(s4$time2015_1-s4$time2016_1)/s4$time2015_1
s4$change_perc_feb2<-(s4$time2015_2-s4$time2016_2)/s4$time2015_2
s4$change_perc_mar2<-(s4$time2015_3-s4$time2016_3)/s4$time2015_3
s4$change_perc_apr2<-(s4$time2015_4-s4$time2016_4)/s4$time2015_4
s4$change_perc_may2<-(s4$time2015_5-s4$time2016_5)/s4$time2015_5
s4$change_perc_jun2<-(s4$time2015_6-s4$time2016_6)/s4$time2015_6
s4$change_perc_jul2<-(s4$time2015_7-s4$time2016_7)/s4$time2015_7
s4$change_perc_aug2<-(s4$time2015_8-s4$time2016_8)/s4$time2015_8
s4$change_perc_sep2<-(s4$time2015_9-s4$time2016_9)/s4$time2015_9

s4$change_perc_jan3<-(s4$time2016_1-s4$time2017_1)/s4$time2016_1
s4$change_perc_feb3<-(s4$time2016_2-s4$time2017_2)/s4$time2016_2
s4$change_perc_mar3<-(s4$time2016_3-s4$time2017_3)/s4$time2016_3
s4$change_perc_apr3<-(s4$time2016_4-s4$time2017_4)/s4$time2016_4
s4$change_perc_may3<-(s4$time2016_5-s4$time2017_5)/s4$time2016_5
s4$change_perc_jun3<-(s4$time2016_6-s4$time2017_6)/s4$time2016_6
s4$change_perc_jul3<-(s4$time2016_7-s4$time2017_7)/s4$time2016_7
s4$change_perc_aug3<-(s4$time2016_8-s4$time2017_8)/s4$time2016_8
s4$change_perc_sep3<-(s4$time2016_9-s4$time2017_9)/s4$time2016_9

names(s4)[1]<-"CA"
names(s4)

#summer winter spring autumn Avg Bill Variables
#summer apr may june rain:july aug : autum :sep oct winter:nov dec jan spring: feb march
s4$sum14<-rowMeans(s4[8:10])
s4$sum15<-rowMeans(s4[20:22])
s4$sum16<-rowMeans(s4[32:34])
s4$sum17<-rowMeans(s4[44:46])

s4$rain14<-rowMeans(s4[11:12])
s4$rain15<-rowMeans(s4[23:24])
s4$rain16<-rowMeans(s4[35:36])
s4$rain17<-rowMeans(s4[47:48])

s4$autm14<-rowMeans(s4[c(13,3)])
s4$autm15<-rowMeans(s4[c(15,25)])
s4$autm16<-rowMeans(s4[c(27,37)])
s4$autm17<-rowMeans(s4[c(49,39)])

s4$win14<-rowMeans(s4[c(2,4,5)])
s4$win15<-rowMeans(s4[c(14,16,17)])
s4$win16<-rowMeans(s4[c(26,28,29)])
s4$win17<-rowMeans(s4[c(40,41,38)])

s4$spr14<-rowMeans(s4[6:7])
s4$spr15<-rowMeans(s4[18:19])
s4$spr16<-rowMeans(s4[30:31])
s4$spr17<-rowMeans(s4[42:43])

s4$change_perc_sum1<-(s4$sum14-s4$sum15)/s4$sum14
s4$change_perc_rain1<-(s4$rain14-s4$rain15)/s4$rain14
s4$change_perc_win1<-(s4$win14-s4$win15)/s4$win14
s4$change_perc_spr1<-(s4$spr14-s4$spr15)/s4$spr14
s4$change_perc_autm1<-(s4$autm14-s4$autm15)/s4$autm14


s4$change_perc_sum2<-(s4$sum15-s4$sum16)/s4$sum15
s4$change_perc_rain2<-(s4$rain15-s4$rain16)/s4$rain15
s4$change_perc_win2<-(s4$win15-s4$win16)/s4$win15
s4$change_perc_spr2<-(s4$spr15-s4$spr16)/s4$spr15
s4$change_perc_autm2<-(s4$autm15-s4$autm16)/s4$autm15

s4$change_perc_sum3<-(s4$sum16-s4$sum17)/s4$sum16
s4$change_perc_rain3<-(s4$rain16-s4$rain17)/s4$rain16
s4$change_perc_win3<-(s4$win16-s4$win17)/s4$win16
s4$change_perc_spr3<-(s4$spr16-s4$spr17)/s4$spr16
s4$change_perc_autm3<-(s4$autm16-s4$autm17)/s4$autm16
names(s4)
s4$avgsum<-rowMeans(s4[c(78:81)])
s4$avgrain<-rowMeans(s4[c(82:85)])
s4$avgautm<-rowMeans(s4[c(86:89)])
s4$avgwin<-rowMeans(s4[c(90:93)])
s4$avgspr<-rowMeans(s4[c(94:97)])


names(s4)
finalbill<-s4[c(1,50:77,98:117)]
names(finalbill)
names(totalbill2)[1]<-"CA"
totalbill3<-totalbill2[c(1,5,6,15:17)]
totl4<-data.frame(unique(totalbill3))
tot15<-sqldf("select CA,rate_category,avg(sanction_load) as sanction_load,classification_of_business_partner,
             connection_status,time_tpddl from totl4 group by CA ")

finaldata<-merge(finalbill,tot15,by="CA",all.x = TRUE)
write.csv(finaldata,"D:/surveyc/modeldata1.csv")

#variables from survey data solar and dsm

d1<-read_excel("D:/survey/data.xlsx",na=c("",NA),sheet=1)
d2<-read_excel("D:/survey/data.xlsx",na=c("",NA),sheet=2)
d3<-read_excel("D:/survey/data.xlsx",na=c("",NA),sheet=3)

datac<-d1[c(10,13,15,16,17,20,21,22,25,26,27,28,29,32,33,34:90,130:160,91:129)]
datac1<-datac[-c(1,5,6:15,17:47,48,53:54,63,65:70,72,73,78,83,88,93,98:103,117,122,134,140)]

datac1[is.na(datac1)]<-"No Response"
y<-which(is.na(datac1)==TRUE)
datac1[y]<-"No Response"
replace(datac1,is.na(datac1),"No Response")
replace(datac1,'NA',"No Response")

datac1$`SEC_C/C_Solar_Rooftop_System_Provided_by_TPDDL`=='No Response'
names(datac1)[1]<-"CA"
findd<-merge(finaldata,datac1,by="CA")
write.csv(findd,"D:/surveyc/modelwithsurevydata.csv")
#calling model data

model<-read.csv("D:/surveyc/modelwithsurevydata.csv")
levels(model$SEC_C.C_Solar_Rooftop_System_Provided_by_TPDDL)
levels(model$SEC_C.C_Solar_Rooftop_System_Provided_by_TPDDL)[c(1,4)]<-0
levels(model$SEC_C.C_Solar_Rooftop_System_Provided_by_TPDDL)[c(2,3)]<-1

levels(model$R3_Gender)
levels(model$R3_Gender)[3]<-"male"
levels(model$SEC_D.D1_Wi.Fi_or_Broadband_Connection)
table(model$SEC_D.D1_Wi.Fi_or_Broadband_Connection)
table(model$SEC_D.D2.D2a_AC_Split)
table(model$SEC_D.D2.D2b_AC_Window)

table(model$SEC_D.D2.D2c_Oven)
table(model$SEC_D.D2.D2d_Refrigerator)
table(model$SEC_D.D2.D2e_Lights)
table(model$SEC_D.D2.D2f_Water_Pumping_System)
table(model$SEC_D.D2.D2g_Geyser)
table(model$SEC_D.D2.D2h_Micowave)
table(model$SEC_D.D2.D2j_Ceiling_Fans)
table(model$SEC_D.D2.D2k_Washing_Machine)
table(model$classification_of_business_partner)
table(model$SEC_D.D2.D2l_Room_Heater)
meant<-model$R4_Age
meant<-as.numeric(meant)
mean(meant[meant<85])
levels(model$rate_category)
table(model$rate_category)
levels(model$R4_Age)[c(64,65)]<-34
model2<-model[-c(51,53,54,58:127)]
model2$R4_Age=as.numeric(model2$R4_Age)
model2[4:45]<-100*model2[4:45]
#Correlation Matrix
corrm<- cor(model2[-c(1,2,53,54,46:50)]) 

require(psych)
require(GPArotation)
#Scree Plot
scree(corrm, factors=T, pc=T, main="Scree Plot", hline=NULL, add=FALSE) ### SCREE PLOT

#Eigenvalues are : 
eigen(corrm)$values 

# CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values)) 

# EXPORTING EIGEN VALUE SUMMARY

#write.csv(eigen_values, "EigenValues.csv")  

#Performing Factor Analysis with 6 as optimum factors from the Scree Plot above

FA<-fa(r=corrm,11, rotate="varimax",SMC=FALSE, fm="minres")  
#FA<-fa(r=corrm,11, rotate="varimax", fm="pa")   
#FA<-fa.parallel(corrm,fm="minresE",fa="PC")  


print(FA) 
FA_SORT<-fa.sort(FA)
ls(FA_SORT)
FA_SORT$loadings
Loadings<-data.frame(FA_SORT$loadings[1:ncol(model2[-c(1,2,53,54,46:50)]),])
write.csv(Loadings,"D:/surveyc/loadings_R.csv")
#clustering and factor analysis
model3<-abs(model2[-c(1,2,53,54,46:50)])

#Outlier Capping Function by 95 percentiles.
model4<-model3[c(35,38,18,40,39,43,37,13,7,1,32,6,4,9,45,15,2)]
num_var1=sapply(model4,is.numeric)
my_num_data0<-t(data.frame(apply(model4[num_var1], 2, var_Summ)))

M1_fun <- function(x){
  quantiles <- quantile( x,.99,na.rm=TRUE )
  x<-ifelse(x>quantiles,(mean(x)+3*sd(x)),x)
  
}

model4[1:8]<-sapply(model4[1:8],function(x){M1_fun(x)})
M1_fun <- function(x){
  quantiles <- quantile( x,.95,na.rm=TRUE )
  x<-ifelse(x>quantiles,(mean(x)+3*sd(x)),x)
  
}
model4[c(9:14,16,17)]<-sapply(model4[c(9:14,16,17)],function(x){M1_fun(x)})
num_var2=sapply(model4,is.numeric)
my_num_data1<-t(data.frame(apply(model4[num_var2], 2, var_Summ)))
M1_fun <- function(x){
  quantiles <- quantile( x,.95,na.rm=TRUE )
  x<-ifelse(x>quantiles,quantiles,x)
  
}
model4[-15]<-sapply(model4[-15],function(x){M1_fun(x)})
num_var3=sapply(model4,is.numeric)
my_num_data3<-t(data.frame(apply(model4[num_var3], 2, var_Summ)))
M1_fun <- function(x){
  quantiles <- quantile( x,.95,na.rm=TRUE )
  x<-ifelse(x>quantiles,(mean(x)+3*sd(x)),x)
  
}

model4[c(1,3,4,7:14,16,17)]<-sapply(model4[c(1,3,4,7:14,16,17)],function(x){M1_fun(x)})
num_var4=sapply(model4,is.numeric)
my_num_data4<-t(data.frame(apply(model4[num_var4], 2, var_Summ)))
M1_fun <- function(x){
  quantiles <- quantile( x,.95,na.rm=TRUE )
  x<-ifelse(x>quantiles,(mean(x)+3*sd(x)),x)
  
}
model4[c(1,3,4,7:14,16,17)]<-sapply(model4[c(1,3,4,7:14,16,17)],function(x){M1_fun(x)})
num_var5=sapply(model4,is.numeric)
my_num_data5<-t(data.frame(apply(model4[num_var5], 2, var_Summ)))

M1_fun <- function(x){
  quantiles <- quantile( x,.95,na.rm=TRUE )
  x<-ifelse(x>quantiles,quantiles,x)
  
}
model4[c(1,3,4,7:14,16,17)]<-sapply(model4[c(1,3,4,7:14,16,17)],function(x){M1_fun(x)})
num_var5=sapply(model4,is.numeric)
my_num_data5<-t(data.frame(apply(model4[num_var5], 2, var_Summ)))

M1_fun <- function(x){
  quantiles <- quantile( x,.93,na.rm=TRUE )
  x<-ifelse(x>quantiles,quantiles,x)
  
}
model4[c(3,4,7:11,13,14,16,17)]<-sapply(model4[c(3,4,7:11,13,14,16,17)],function(x){M1_fun(x)})
num_var5=sapply(model4,is.numeric)
my_num_data5<-t(data.frame(apply(model4[num_var5], 2, var_Summ)))


M1_fun <- function(x){
  quantiles <- quantile( x,.93,na.rm=TRUE )
  x<-ifelse(x>quantiles,quantiles,x)
  
}
model4[9]<-sapply(model4[9],function(x){M1_fun(x)})
num_var5=sapply(model4,is.numeric)
my_num_data5<-t(data.frame(apply(model4[num_var5], 2, var_Summ)))


#writing outlier removed file

write.csv(model4,"D:/surveyc/outlier_model_factor.csv")

model4<-read.csv("D:/surveyc/outlier_model_factor.csv")
inputdata_final2<-model4
inputdata_final3= scale(inputdata_final2) #standardization
inputdata_final3=as.matrix(inputdata_final3)
set.seed(656803)

cluster_three <- kmeans(inputdata_final3,3,nstart = 20,iter.max = 100)
cluster_four <- kmeans(inputdata_final3,4,nstart = 20,iter.max = 100)
cluster_five <- kmeans(inputdata_final3,5,nstart = 20,iter.max = 100)
cluster_six <- kmeans(inputdata_final3,6,nstart = 20,iter.max = 100)

(cluster_three$size)/578
(cluster_four$size)/578
(cluster_five$size)/578
(cluster_six$size)/578
model5<-cbind(model[c(2,51,52,53,54,56,57:127)],model4)
d1_new<-cbind(model5,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
class(cluster_three$cluster)
###Profiling

#Converting into factors
d1_new$km_clust_3=factor(d1_new$km_clust_3)
d1_new$km_clust_4=factor(d1_new$km_clust_4)
d1_new$km_clust_5=factor(d1_new$km_clust_5)
d1_new$km_clust_6=factor(d1_new$km_clust_6)
table(d1_new$rate_category,d1_new$km_clust_5)
table(d1_new$sanction_load,d1_new$km_clust_5)
table(d1_new$R3_Gender,d1_new$km_clust_5)
table(d1_new$R4_Age,d1_new$km_clust_5)
table(d1_new$time_tpddl,d1_new$km_clust_5)
d1_new$R4_Age=as.numeric(d1_new$R4_Age)
write.csv(d1_new,"D:/surveyc/d1_new.csv")
require(tables)
profile<-tabular(1+change_perc_rain2+change_perc_autm2+change_perc_aug2+change_perc_rain3+change_perc_sum3                                                                        
                 +change_perc_autm3+change_perc_spr2+change_perc_mar2+change_perc_jun1+timeavg_bill_4yrs                                                                        
                 +change_perc_spr1+change_perc_may1+change_perc_mar1+change_perc_aug1+time_tpddl+change_perc_may2                                                                     
                 +change_perc_jan1 +sanction_load+ R4_Age ~
                   mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),
                 data=d1_new)

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)

profilee<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                  data=d1_new)
profile2<-as.matrix(profilee)

install.packages(c("factoextra", "fpc", "NbClust"))
library(factoextra)
library(fpc)
library(NbClust)
km.res <- eclust(inputdata_final3, "kmeans", k = 5, nstart = 25, graph = FALSE)

fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())
hc.res <- eclust(inputdata_final3, "hclust", k = 5, hc_metric = "euclidean", 
                 hc_method = "ward.D2", graph = FALSE)
# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

fviz_silhouette(cluster_five, palette = "jco", 
                ggtheme = theme_classic())
silinfo <- cluster_five$silinfo
names(silinfo)
# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
(km.res$size)/578

#Briefly, connectivity indicates the degree of connectedness of the clusters, as determined by k-nearest neighbors. Connectedness corresponds to what extent items are placed in the same cluster as their nearest neighbors in the data space. The connectivity has a value between 0 and infinity and should be minimized.

# Silhouette width and Dunn index combine measures of compactness and separation of the clusters. Recall that the values of silhouette width range from -1 (poorly clustered observations) to 1 (well clustered observations). The Dunn index is the ratio between the smallest distance between observations not in the same cluster to the largest intra-cluster distance. It has a value between 0 and infinity and should be maximized.

km_stats <- cluster.stats(dist(inputdata_final3),  cluster_five$cluster)
names(km_stats)
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(inputdata_final3, nClust = 2:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)
plot(intern)
#Recall that the connectivity should be minimized, while both the Dunn index and the silhouette width should be maximized.
# Stability measures
#The values of APN, ADM and FOM ranges from 0 to 1, with smaller value corresponding with highly consistent clustering results. AD has a value between 0 and infinity, and smaller values are also preferred.


clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(inputdata_final3, nClust = 2:6, clMethods = clmethods,
                validation = "stability")
optimalScores(stab)
summary(stab)

plot(stab)

#DOING HCLUST WITH K=5 
diss<-dist(inputdata_final3,method = "euclidean")
hclust1<-hclust(diss,method="ward.D2")
memb <- cutree(hclust1, k = 5)
memb
d1_new1<-cbind(model5,km_clust_5=memb)
d1_new1$km_clust_5<-as.factor(d1_new1$km_clust_5)
table(d1_new1$km_clust_5)/578

require(tables)
profile<-tabular(1+change_perc_rain2+change_perc_autm2+change_perc_aug2+change_perc_rain3+change_perc_sum3                                                                        
                 +change_perc_autm3+change_perc_spr2+change_perc_mar2+change_perc_jun1+timeavg_bill_4yrs                                                                        
                 +change_perc_spr1+change_perc_may1+change_perc_mar1+change_perc_aug1+time_tpddl+change_perc_may2                                                                     
                 +change_perc_jan1 +sanction_load+ R4_Age ~
                   (mean*km_clust_5),
                 data=d1_new1)

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
write.csv(d1_new1,"D:/surveyc/d1_new1.csv")
