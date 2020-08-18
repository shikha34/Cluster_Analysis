#-------CLUSTERING----------------
rm(list=ls())

library(sqldf)
library(stats)
library(base)
library(plyr)
library(base)
library(readxl)
library(dplyr)
library(lubridate)
library(qcc)
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

##Reading the billing data of employees
finaldsm <- read.csv("D:/BILL/finaldsm.csv")
dsmg=finaldsm[-c(1,2,17:20,22,24,34:45)]
str(dsmg)
dsmg$x1.0_TR_Split_BEE_5_STAR=(dsmg$X.x1.0_TR_Split_BEE_5_STAR-min(dsmg$X.x1.0_TR_Split_BEE_5_STAR))/(max(dsmg$X.x1.0_TR_Split_BEE_5_STAR)-min(dsmg$X.x1.0_TR_Split_BEE_5_STAR))
dsmg$Sanction_Load_KW=(dsmg$Sanction_Load_KW-min(dsmg$Sanction_Load_KW))/(max(dsmg$Sanction_Load_KW)-min(dsmg$Sanction_Load_KW))
dsmg$Amount_Paid=(dsmg$Amount_Paid-min(dsmg$Amount_Paid))/(max(dsmg$Amount_Paid)-min(dsmg$Amount_Paid))
dsmg$x1.0_TR_Window_BEE_5_STAR=(dsmg$x1.0_TR_Window_BEE_5_STAR-min(dsmg$x1.0_TR_Window_BEE_5_STAR))/(max(dsmg$x1.0_TR_Window_BEE_5_STAR)-min(dsmg$x1.0_TR_Window_BEE_5_STAR))
dsmg$x1.5_TR_Inverter=(dsmg$x1.5_TR_Inverter-min(dsmg$x1.5_TR_Inverter))/(max(dsmg$x1.5_TR_Inverter)-min(dsmg$x1.5_TR_Inverter))
dsmg$x1.5_TR_Split_BEE_5_STAR=(dsmg$x1.5_TR_Split_BEE_5_STAR-min(dsmg$x1.5_TR_Split_BEE_5_STAR))/(max(dsmg$x1.5_TR_Split_BEE_5_STAR)-min(dsmg$x1.5_TR_Split_BEE_5_STAR))
dsmg$x1.5_TR_Window_BEE_5_STAR=(dsmg$x1.5_TR_Window_BEE_5_STAR-min(dsmg$x1.5_TR_Window_BEE_5_STAR))/(max(dsmg$x1.5_TR_Window_BEE_5_STAR)-min(dsmg$x1.5_TR_Window_BEE_5_STAR))
dsmg$x20W_LED_TUBELIGHT=(dsmg$x20W_LED_TUBELIGHT-min(dsmg$x20W_LED_TUBELIGHT))/(max(dsmg$x20W_LED_TUBELIGHT)-min(dsmg$x20W_LED_TUBELIGHT))
dsmg$x50W_CEILING_FAN=(dsmg$x50W_CEILING_FAN-min(dsmg$x50W_CEILING_FAN))/(max(dsmg$x50W_CEILING_FAN)-min(dsmg$x50W_CEILING_FAN))
dsmg$x9W_LED_BULB=(dsmg$x9W_LED_BULB-min(dsmg$x9W_LED_BULB))/(max(dsmg$x9W_LED_BULB)-min(dsmg$x9W_LED_BULB))
dsmg$Avg_Jan_bill=(dsmg$Avg_Jan_bill-min(dsmg$Avg_Jan_bill))/(max(dsmg$Avg_Jan_bill)-min(dsmg$Avg_Jan_bill))
dsmg$Avg_Feb_bill=(dsmg$Avg_Feb_bill-min(dsmg$Avg_Feb_bill))/(max(dsmg$Avg_Feb_bill)-min(dsmg$Avg_Feb_bill))
dsmg$Avg_Mar_bill=(dsmg$Avg_Mar_bill-min(dsmg$Avg_Mar_bill))/(max(dsmg$Avg_Mar_bill)-min(dsmg$Avg_Mar_bill))
dsmg$Avg_Apr_bill=(dsmg$Avg_Apr_bill-min(dsmg$Avg_Apr_bill))/(max(dsmg$Avg_Apr_bill)-min(dsmg$Avg_Apr_bill))
dsmg$Avg_May_bill=(dsmg$Avg_May_bill-min(dsmg$Avg_May_bill))/(max(dsmg$Avg_May_bill)-min(dsmg$Avg_May_bill))
dsmg$Avg_Jun_bill=(dsmg$Avg_Jun_bill-min(dsmg$Avg_Jun_bill))/(max(dsmg$Avg_Jun_bill)-min(dsmg$Avg_Jun_bill))
dsmg$Avg_Jul_bill=(dsmg$Avg_Jul_bill-min(dsmg$Avg_Jul_bill))/(max(dsmg$Avg_Jul_bill)-min(dsmg$Avg_Jul_bill))
dsmg$Avg_Aug_bill=(dsmg$Avg_Aug_bill-min(dsmg$Avg_Aug_bill))/(max(dsmg$Avg_Aug_bill)-min(dsmg$Avg_Aug_bill))
dsmg$Avg_Sep_bill=(dsmg$Avg_Sep_bill-min(dsmg$Avg_Sep_bill))/(max(dsmg$Avg_Sep_bill)-min(dsmg$Avg_Sep_bill))
dsmg$Avg_Oct_bill=(dsmg$Avg_Oct_bill-min(dsmg$Avg_Oct_bill))/(max(dsmg$Avg_Oct_bill)-min(dsmg$Avg_Oct_bill))
dsmg$Avg_Nov_bill=(dsmg$Avg_Nov_bill-min(dsmg$Avg_Nov_bill))/(max(dsmg$Avg_Nov_bill)-min(dsmg$Avg_Nov_bill))
dsmg$Avg_Dec_bill=(dsmg$Avg_Dec_bill-min(dsmg$Avg_Dec_bill))/(max(dsmg$Avg_Dec_bill)-min(dsmg$Avg_Dec_bill))
dsmg$Two_Year_Avg_PF=(dsmg$Two_Year_Avg_PF-min(dsmg$Two_Year_Avg_PF))/(max(dsmg$Two_Year_Avg_PF)-min(dsmg$Two_Year_Avg_PF))
dsmg$Two_Year_Avg_VL=(dsmg$Two_Year_Avg_VL-min(dsmg$Two_Year_Avg_VL))/(max(dsmg$Two_Year_Avg_VL)-min(dsmg$Two_Year_Avg_VL))

#For reproproducibility
set.seed(76221853)

# Calcualting Distance Matrix
dist <- daisy(dsmg,metric = "euclidean",stand = FALSE)
summary(dist)
mat <- as.matrix(dist)

#k-mediods Clustering
sil_width <- c()
for(i in 2:10){
  
  pam_fit <- pam(dist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(1:10, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
lines(1:10, sil_width)

pam_fit3<-pam(dist, diss = TRUE, k = 3)
pam_fit4<-pam(dist, diss = TRUE, k = 4)
pam_fit5<-pam(dist, diss = TRUE, k = 5)
pam_fit6<-pam(dist, diss = TRUE, k = 6)

fcluster<-cbind(dsm2,clust_3=pam_fit3$clustering,clust_4=pam_fit4$clustering,clust_5=pam_fit5$clustering,clust_6=pam_fit6$clustering)

write.csv(fcluster,"D:/DSM/output2/pam.csv")
###Profiling

#Converting into factors
fcluster$clust_3=factor(fcluster$clust_3)
fcluster$clust_4=factor(fcluster$clust_4)
fcluster$clust_5=factor(fcluster$clust_5)
fcluster$clust_6=factor(fcluster$clust_6)

require(tables)
names(fcluster)=gsub(".","_",fixed=TRUE,names(fcluster))
colnames(fcluster)[7]<-"x1_0_TR_Split_BEE_5_STAR"
colnames(fcluster)[2]<-"District"
prof
ile<-tabular(1+Sanction_Load_KW+Issued_Qty+Amount_Paid+daysin_tpddl+x1_0_TR_Split_BEE_5_STAR+x1_0_TR_Window_BEE_5_STAR+x1_5_TR_Inverter+x1_5_TR_Split_BEE_5_STAR+x1_5_TR_Window_BEE_5_STAR+x20W_LED_TUBELIGHT
                 +x50W_CEILING_FAN+x9W_LED_BULB+CROMPTON_GREAVES+COMPACT_LAMPS+GODREJ+HITACHI+HQ_LAMPS+LEDVANCE+MARC_ENTERPRISES_PVT_LTD+ORIENT_ELECTRICS+OSRAM_INDIA_PVT_LTD     
                 +PHILIPS+SURYA_ROSHNI+VOLTAS+dsm2$purchase_times~
                   mean+(mean*clust_3)+(mean*clust_4)+(mean*clust_5),
                 data=fcluster)
profile1<-as.matrix(profile)
profile2<-data.frame(profile1)
write.csv(profile2,"D:/DSM/output2/contniuous_pam.csv")
profilee<-tabular(1+Ownership_Group+Rate_Category+District~
                    clust_3+clust_4+clust_5,
                  data=fcluster)

profile11<-as.matrix(profilee)
profile22<-data.frame(profile11)
write.csv(profile22,"D:/DSM/output2/categorical.csv")
pro<-tabular(1~length+(length*clust_3)+(length*clust_4)+(length*clust_5),
             data=fcluster)

pro2<-as.matrix(pro)
pro22<-data.frame(pro2)

#--------visualisation--------
#taking orginal data
tsne_obj <- Rtsne(dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(km_fit4$cluster))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#-------------KMEANS------------------
wss<-c()
for (i in 2:15) {
  wss[i] <- sum(kmeans(dsmg,centers=i)$withinss)
}   
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km_fit3<-kmeans(dsmg,3)
km_fit4<-kmeans(dsmg,4)
km_fit5<-kmeans(dsmg,5)
km_fit6<-kmeans(dsmg,6)

fcluster1<-cbind(finaldsm,clust_3=km_fit3$cluster,clust_4=km_fit4$cluster,clust_5=km_fit5$cluster,clust_6=km_fit6$cluster)

write.csv(fcluster1,"D:/DSM/bill/km.csv")

###Profiling

#Converting into factors
fcluster1$clust_3=factor(fcluster1$clust_3)
fcluster1$clust_4=factor(fcluster1$clust_4)
fcluster1$clust_5=factor(fcluster1$clust_5)
fcluster1$clust_6=factor(fcluster1$clust_6)

require(tables)
names(fcluster1)=gsub(".","_",fixed=TRUE,names(fcluster1))
#colnames(fcluster1)[7]<-"x1_0_TR_Split_BEE_5_STAR"
#colnames(fcluster1)[2]<-"District"
p<-tabular(1+Avg_Jan_bill+Avg_Feb_bill+Avg_Mar_bill+Avg_Apr_bill+Avg_May_bill+Avg_Jun_bill+Avg_Jul_bill+
             Avg_Aug_bill+Avg_Sep_bill+Avg_Oct_bill+Avg_Nov_bill+Avg_Dec_bill+Two_Year_Avg_PF+Two_Year_Avg_VL+
             Sanction_Load_KW+Amount_Paid+daysin_tpddl+X_x1_0_TR_Split_BEE_5_STAR+x1_0_TR_Window_BEE_5_STAR+x1_5_TR_Inverter+x1_5_TR_Split_BEE_5_STAR+x1_5_TR_Window_BEE_5_STAR+x20W_LED_TUBELIGHT
           +x50W_CEILING_FAN+x9W_LED_BULB~
             mean+(mean*clust_3)+(mean*clust_4)+(mean*clust_5)+(mean*clust_6),
           data=fcluster1)
#
p1<-as.matrix(p)
p2<-data.frame(p1)
write.csv(p2,"D:/DSM/bill/contniuous_km.csv")

fcluster1$COMPACT_LAMPS=as.character(fcluster1$COMPACT_LAMPS)
fcluster1$District__NDPL_=as.character(fcluster1$District__NDPL_)
fcluster1$Ownership_Group=as.character(fcluster1$Ownership_Group)
fcluster1$Rate_Category=as.character(fcluster1$Rate_Category)
fcluster1$CROMPTON_GREAVES=as.character(fcluster1$CROMPTON_GREAVES)
fcluster1$COMPACT_LAMPS=as.character(fcluster1$COMPACT_LAMPS)
fcluster1$GODREJ=as.character(fcluster1$GODREJ)
fcluster1$HITACHI=as.character(fcluster1$HITACHI)
fcluster1$HQ_LAMPS=as.character(fcluster1$HQ_LAMPS)
fcluster1$LEDVANCE=as.character(fcluster1$LEDVANCE)
fcluster1$MARC_ENTERPRISES_PVT_LTD=as.character(fcluster1$MARC_ENTERPRISES_PVT_LTD)
fcluster1$ORIENT_ELECTRICS=as.character(fcluster1$ORIENT_ELECTRICS)
fcluster1$OSRAM_INDIA_PVT_LTD=as.character(fcluster1$OSRAM_INDIA_PVT_LTD)
fcluster1$PHILIPS=as.character(fcluster1$PHILIPS)
fcluster1$SURYA_ROSHNI=as.character(fcluster1$SURYA_ROSHNI)
fcluster1$VOLTAS=as.character(fcluster1$VOLTAS)

pe<-tabular(1+CROMPTON_GREAVES+COMPACT_LAMPS+GODREJ+HITACHI+HQ_LAMPS+LEDVANCE+MARC_ENTERPRISES_PVT_LTD+ORIENT_ELECTRICS+OSRAM_INDIA_PVT_LTD     
            +PHILIPS+SURYA_ROSHNI+VOLTAS+Ownership_Group+Rate_Category+District__NDPL_~
              clust_3+clust_4+clust_5+clust_6,
            data=fcluster1)

p11<-as.matrix(pe)
p22<-data.frame(p11)
write.csv(p22,"D:/DSM/bill/categorical_km.csv")
proo<-tabular(1~length+(length*clust_3)+(length*clust_4)+(length*clust_5)+(length*clust_6),
              data=fcluster1)

proo2<-as.matrix(proo)
proo22<-data.frame(proo2)


#Graph based on k-means
require(cluster)

clusplot(dsmg, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

