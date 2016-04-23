#Author : Pallavi Karan
#Date: 03/26/2016
#Purpose: Santander Customer Satisfaction
library(corrgram)
rm(list = ls())
mydata = read.csv("D:\\Spring 2016\\DS_Lab\\Santander Customer Satisfaction\\Final\\train.csv")  # read csv file 
mydata

attach(mydata)

#To find the max value of a column 
colMax <- function(mydata) sapply(mydata, max, na.rm = TRUE)
colMax(mydata)
#Checking for minimum values for the columns whose max value is zero
colMin<-function(mydata) sapply(mydata, min, na.rm = TRUE)
colMin(mydata)
#Removing all the columns whose min and max, both values are zero.
Data <- subset( mydata, select = -c(ind_var2_0, ind_var2,ind_var27_0,ind_var28_0,
                                    ind_var28, ind_var27, ind_var41,ind_var46_0,
                                    ind_var46, num_var27_0, num_var28_0,num_var28,
                                    num_var27, num_var41, num_var46_0, num_var46,
                                    saldo_var28, saldo_var27, saldo_var41, saldo_var46,
                                    imp_amort_var18_hace3, imp_amort_var34_hace3, imp_reemb_var13_hace3,
                                    imp_reemb_var33_hace3, imp_trasp_var17_out_hace3, imp_trasp_var33_out_hace3, 
                                    num_var2_0_ult1, num_var2_ult1, num_reemb_var13_hace3, 
                                    num_reemb_var33_hace3, num_trasp_var17_out_hace3, num_trasp_var33_out_hace3,
                                    saldo_var2_ult1, saldo_medio_var13_medio_hace3) )
Data
View(Data)

#Sorting the Data DESC order
colSort <- function(Data, ...) sapply(Data, sort, ...)
SortedData<-colSort(Data, decreasing = TRUE)
View(SortedData)


#Boxplot for outliers after sorting from DESC to ASC
boxplot(mydata$ind_var6,main="Attribute: ind_var6")
boxplot(mydata$ind_var13_medio_0,main="Attribute: ind_var13_medio_0")
boxplot(mydata$ind_var13_medio,main="Attribute: ind_var13_medio")
boxplot(mydata$ind_var18_0,main="Attribute: ind_var18_0")
boxplot(mydata$ind_var18,main="Attribute: ind_var18")
boxplot(mydata$ind_var29,main="Attribute: ind_var29")
boxplot(mydata$ind_var34_0,main="Attribute: ind_var34_0")
boxplot(mydata$ind_var34,main="Attribute: ind_var34")
boxplot(mydata$num_var6,main="Attribute: num_var6")
boxplot(mydata$num_var13_medio_0,main="Attribute: num_var13_medio_0")
boxplot(mydata$num_var13_medio,main="Attribute: num_var13_medio")
boxplot(mydata$num_var18_0,main="Attribute: num_var18_0")
boxplot(mydata$num_var18,main="Attribute: num_var18")
boxplot(mydata$num_var29,main="Attribute: num_var29")
boxplot(mydata$num_var34_0,main="Attribute: num_var34_0")
boxplot(mydata$num_var34,main="Attribute: num_var34")
boxplot(mydata$saldo_var6,main="Attribute: saldo_var6")
boxplot(mydata$saldo_var13_medio,main="Attribute: saldo_var13_medio")
boxplot(mydata$saldo_var18,main="Attribute: saldo_var18")
boxplot(mydata$saldo_var29,main="Attribute: saldo_var29")
boxplot(mydata$saldo_var34,main="Attribute: saldo_var34")
boxplot(mydata$delta_imp_amort_var18_1y3,main="Attribute: delta_imp_amort_var18_1y3")
boxplot(mydata$delta_imp_aport_var33_1y3,main="Attribute: delta_imp_aport_var33_1y3")
boxplot(mydata$delta_imp_reemb_var33_1y3,main="Attribute: delta_imp_reemb_var33_1y3")
boxplot(mydata$delta_imp_trasp_var33_out_1y3,main="Attribute: delta_imp_trasp_var33_out_1y3")
boxplot(mydata$delta_num_aport_var33_1y3,main="Attribute: delta_num_aport_var33_1y3")
boxplot(mydata$delta_num_reemb_var33_1y3,main="Attribute: delta_num_reemb_var33_1y3")
boxplot(mydata$delta_num_trasp_var33_out_1y3,main="Attribute: delta_num_trasp_var33_out_1y3")
boxplot(mydata$imp_amort_var18_ult1,main="Attribute: imp_amort_var18_ult1")
boxplot(mydata$imp_amort_var34_ult1,main="Attribute: imp_amort_var34_ult1")
boxplot(mydata$imp_reemb_var17_hace3,main="Attribute: imp_reemb_var17_hace3")
boxplot(mydata$imp_var7_emit_ult1,main="Attribute: imp_var7_emit_ult1")
boxplot(mydata$imp_reemb_var33_ult1,main="Attribute: imp_reemb_var33_ult1")
boxplot(mydata$imp_trasp_var17_in_hace3,main="Attribute: imp_trasp_var17_in_hace3")
boxplot(mydata$imp_trasp_var33_out_ult1,main="Attribute: imp_trasp_var33_out_ult1")
boxplot(mydata$imp_venta_var44_hace3,main="Attribute: imp_venta_var44_hace3")
boxplot(mydata$ind_var7_emit_ult1,main="Attribute: ind_var7_emit_ult1")
boxplot(mydata$num_reemb_var17_hace3,main="Attribute: num_reemb_var17_hace3")
boxplot(mydata$num_meses_var13_medio_ult3,main="Attribute: num_meses_var13_medio_ult3")
boxplot(mydata$num_reemb_var33_ult1,main="Attribute: num_reemb_var33_ult1")
boxplot(mydata$num_trasp_var17_in_hace3,main="Attribute: num_trasp_var17_in_hace3")
boxplot(mydata$num_trasp_var17_in_ult1,main="Attribute: num_trasp_var17_in_ult1")
boxplot(mydata$num_trasp_var33_out_ult1,main="Attribute: num_trasp_var33_out_ult1")
boxplot(mydata$num_venta_var44_hace3,main="Attribute: num_venta_var44_hace3")
boxplot(mydata$saldo_medio_var13_medio_hace2,main="Attribute: saldo_medio_var13_medio_hace2")
boxplot(mydata$saldo_medio_var13_medio_ult1,main="Attribute: saldo_medio_var13_medio_ult1")
boxplot(mydata$saldo_medio_var13_medio_ult3,main="Attribute: saldo_medio_var13_medio_ult3")
boxplot(mydata$saldo_medio_var29_hace3,main="Attribute: saldo_medio_var29_hace3")

newdata<-mydata
newdata<-newdata[!(newdata$ind_var6==1),]
newdata<-newdata[!(newdata$ind_var13_medio==1),]
newdata<-newdata[!(newdata$ind_var13_medio_0==1),]
newdata<-newdata[!(newdata$ind_var18_0==1),]
newdata<-newdata[!(newdata$ind_var18==1),]
newdata<-newdata[!(newdata$ind_var29==1),]
newdata<-newdata[!(newdata$ind_var34==1),]
newdata<-newdata[!(newdata$num_var6==3),]
newdata<-newdata[!(newdata$num_var13_medio_0==3),]
newdata<-newdata[!(newdata$num_var13_medio==3),]
newdata<-newdata[!(newdata$num_var18_0==3),]
#newdata<-newdata[!(newdata$ind_var18==3),]
newdata<-newdata[!(newdata$num_var29==3),]
newdata<-newdata[!(newdata$num_var34_0==3),]
newdata<-newdata[!(newdata$num_var34==3),]
#newdata<-newdata[!(newdata$saldo_var6==),]
#newdata<-newdata[!(newdata$saldo_var13_medio==1),]
#newdata<-newdata[!(newdata$saldo_var18==1),]
#newdata<-newdata[!(newdata$saldo_var29==1),]
#newdata<-newdata[!(newdata$saldo_var34==1),]
newdata<-newdata[!(newdata$delta_imp_amort_var18_1y3==9999999999),]
newdata<-newdata[!(newdata$delta_imp_amort_var34_1y3==9999999999),]
newdata<-newdata[!(newdata$delta_imp_aport_var33_1y3==9999999999),]
newdata<-newdata[!(newdata$delta_imp_reemb_var33_1y3==9999999999),]
newdata<-newdata[!(newdata$delta_imp_trasp_var33_out_1y3==9999999999),]
newdata<-newdata[!(newdata$delta_num_aport_var33_1y3==9999999999),]
newdata<-newdata[!(newdata$delta_num_reemb_var33_1y3==9999999999),]
newdata<-newdata[!(newdata$delta_num_trasp_var33_out_1y3==9999999999),]
#newdata<-newdata[!(newdata$imp_amort_var18_ult1==1),]
#newdata<-newdata[!(newdata$imp_amort_var34_ult1==1),]
newdata<-newdata[!(newdata$imp_reemb_var17_hace3==12027.15),]
#newdata<-newdata[!(newdata$imp_var7_emit_ult1==1),]
newdata<-newdata[!(newdata$imp_reemb_var33_ult1==1200),]
newdata<-newdata[!(newdata$num_var7_emit_ult1==3),]
newdata<-newdata[!(newdata$imp_trasp_var33_out_ult1==3000),]
#newdata<-newdata[!(newdata$imp_venta_var44_hace3==1),]
#newdata<-newdata[!(newdata$ind_var7_emit_ult1==1),]
newdata<-newdata[!(newdata$num_reemb_var17_hace3==3),]
newdata<-newdata[!(newdata$num_meses_var13_medio_ult3==2),]
newdata<-newdata[!(newdata$num_reemb_var33_ult1==3),]
newdata<-newdata[!(newdata$num_trasp_var17_in_hace3==6 & newdata$num_trasp_var17_in_hace3==3),]
#newdata<-newdata[!(newdata$num_trasp_var17_in_ult1==1),]
newdata<-newdata[!(newdata$num_trasp_var33_out_ult1==3),]
#newdata<-newdata[!(newdata$num_venta_var44_hace3==1),]
#newdata<-newdata[!(newdata$saldo_medio_var13_medio_hace2==1),]
#newdata<-newdata[!(newdata$saldo_medio_var13_medio_ult1==1),]
#newdata<-newdata[!(newdata$saldo_medio_var13_medio_ult3==1),]
newdata<-newdata[!(newdata$saldo_medio_var29_hace3==145.2),]
newdata #76004 Observations.

#Finding co-relation between the columns with outliers and TARGET
cor(num_venta_var44_hace3, TARGET, use = "everything")
cor(num_trasp_var17_in_ult1, TARGET, use = "everything")

attach(newdata)
#After outlier reduction,, the columns which have max and min value=0 and corelation less than 0.005 are removed.
final.reduced.data <- subset( mydata, select = -c(ind_var6,ind_var13_medio,
                                                  ind_var13_medio_0,ind_var18_0,
                                                  ind_var18_0,ind_var18,ind_var29,
                                                  ind_var34,num_var6,num_var13_medio_0,
                                                  num_var13_medio,num_var18_0,num_var29,
                                                  num_var34_0,num_var34,delta_imp_amort_var18_1y3,
                                                  delta_imp_amort_var34_1y3,delta_imp_aport_var33_1y3,
                                                  delta_imp_reemb_var33_1y3,delta_imp_trasp_var33_out_1y3,
                                                  delta_num_aport_var33_1y3,delta_num_reemb_var33_1y3,
                                                  delta_num_trasp_var33_out_1y3,imp_reemb_var17_hace3,
                                                  imp_reemb_var33_ult1,num_var7_emit_ult1,
                                                  imp_trasp_var33_out_ult1,num_reemb_var17_hace3,
                                                  num_meses_var13_medio_ult3,num_reemb_var33_ult1,
                                                  num_trasp_var17_in_hace3,num_trasp_var33_out_ult1,
                                                  saldo_medio_var29_hace3,num_venta_var44_hace3,
                                                  num_trasp_var17_in_ult1))
#Final dataset after cleaning, normalization and feature reduction has 76020 obs of 337 variables


#Finding corelation between columns
corrgram(Data,order=TRUE, var3=var15,main="Correlation Plot of var3 & var15")


