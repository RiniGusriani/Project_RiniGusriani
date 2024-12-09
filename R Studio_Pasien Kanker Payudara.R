#Library("survival")
library(survival)
library(readxl)
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

#Baca data
dataklp1=read.delim("clipboard")
attach(datakankerpayudara)
View(datakankerpayudara) 
summary(datakankerpayudara)

#membuat Model Cox PH
response= Surv(stop, status)
model.coxph = coxph(response~id+umur+metode, data=datakankerpayudara)
model.coxph

#Hipotesis Serempak
#H0 : variabel bebas secara bersama-sama tidak berpengaruh terhadap variabel terikat
#H1 : setidaknya ada satu variabel bebas yang berpengaruh terhadap variabel terikat

#Hipoteisi partial
#H0: variabel grade tidak berpengaruh terhadap waktu kelangsungan hidup pasien aknker usus besar
#H1 : variabel grade berpengaruh terhadap waktu kelangsungan hidup pasien aknker usus besar

#UJI ASUMSI
residual=cox.zph(model.coxph)
residual
#H0 : variabel memenuhi asumsi Proportional Hazard
#H1 : variabel tidak memenuhi asumsi Proportional Hazard

#UNTUK MEMENUHI ASUMSI HAZARD SALAH SATUNYA BISA DIGUNAKAN MODEL STRATIFIED COX 
#PERBANDINGAN DENGAN STRATIFIED COX
response=Surv(stop, status)
cox.strata= coxph(response~id+umur+strata(metode),data=datakankerpayudara)
cox.strata

#UJI ASUMSI PH
residual2= cox.zph(cox.strata)
residual2

#Model
km.model1<-survfit(Surv(stop, status)~metode,
                   data=datakankerpayudara,type="kaplan-meier")
km.model1

#Plot
autoplot(km.model1,main="KM CURVE BERDASARKAN STATUS KEMO")
