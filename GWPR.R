#Package Uji Asumsi
library(lmtest)
library(MASS)
library(car) #Uji Multikol
library(plm) #Analisis Data Panel
#Package Analisis Spasial
library(TH.data)
library(GWmodel) #geographically weighted model#
library(sp)
library(spdep) #untuk pembobotan#
library(spgwr) #geographically weighted regression (GWR)#
#Package Pemetaan
library(plotly)
library(sf)
library(ggplot2)
library(ggpubr)
library(rgdal)
#Package Penggabungan Data
library(tidyr)
library(dplyr)
library(readxl)

#IMPORT DATA
Data.mapping=read_excel("Data_Panel_IPMIDN.xlsx")
Data.Panel=read_excel("Data_Panel_IPM_Indonesia.xlsx")
within.trans=read_excel("Data_GWR_PANEL_IPM.xlsx")

#--------------Eksploarasi Data Aktual dengan Pemetaan-----------------#
#----------------------------------------------------------------------#
##IMPORT PETA SHP
shp.IDN=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN=left_join(shp.IDN,Data.mapping,by="OBJECTID")
View(gabung.IDN)

#Pemetaan IPM dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2017),color="white")+
  labs(title="IPM INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2018),color="white") +
  labs(title="IPM INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2019),color="white") +
  labs(title="IPM INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2020),color="white") +
  labs(title="IPM INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2021),color="white") +
  labs(title="IPM INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IPM_2022),color="white") +
  labs(title="IPM INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=3,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("IPM_2017","IPM_2018","IPM_2019","IPM_2020","IPM_2021","IPM_2022",geometry) %>%
  gather(VAR, IPM, -geometry)%>%
  mutate(IPM.INDO = cut_number(IPM, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = IPM.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 3) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan IPM dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2017),color="white")+
  labs(title="TPT INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2018),color="white") +
  labs(title="TPT INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2019),color="white") +
  labs(title="TPT INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2020),color="white") +
  labs(title="TPT INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2021),color="white") +
  labs(title="TPT INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = TPT_2022),color="white") +
  labs(title="TPT INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=3,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("TPT_2017","TPT_2018","TPT_2019","TPT_2020","TPT_2021","TPT_2022",geometry) %>%
  gather(VAR, TPT, -geometry)%>%
  mutate(TPT.INDO = cut_number(TPT, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = TPT.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan Pengeluaran Perkapita dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2017),color="white")+
  labs(title="PP INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2018),color="white") +
  labs(title="PP INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2019),color="white") +
  labs(title="PP INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2020),color="white") +
  labs(title="PP INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2021),color="white") +
  labs(title="PP INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PP_2022),color="white") +
  labs(title="PP INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=3,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("PP_2017","PP_2018","PP_2019","PP_2020","PP_2021","PP_2022",geometry) %>%
  gather(VAR, PP, -geometry)%>%
  mutate(PP.INDO = cut_number(PP, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = PP.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan JUMLAH PENDUDUK dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2017),color="white")+
  labs(title="JPM INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2018),color="white") +
  labs(title="JPM INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2019),color="white") +
  labs(title="JPM INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2020),color="white") +
  labs(title="JPM INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2021),color="white") +
  labs(title="JPM INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = JPM_2022),color="white") +
  labs(title="JPM INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=3,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("JPM_2017","JPM_2018","JPM_2019","JPM_2020","JPM_2021","JPM_2022",geometry) %>%
  gather(VAR, JPM, -geometry)%>%
  mutate(JPM.INDO = cut_number(JPM, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = JPM.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan INDEKS KEMAHALAN KONSTRUKSI dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2017),color="white")+
  labs(title="IKK INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2018),color="white") +
  labs(title="IKK INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2019),color="white") +
  labs(title="IKK INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2020),color="white") +
  labs(title="IKK INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2021),color="white") +
  labs(title="IKK INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = IKK_2022),color="white") +
  labs(title="IKK INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=3,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("IKK_2017","IKK_2018","IKK_2019","IKK_2020","IKK_2021","IKK_2022",geometry) %>%
  gather(VAR, IKK, -geometry)%>%
  mutate(IKK.INDO = cut_number(IKK, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = IKK.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan ANGKA PARTISIPASI SEKOLAH dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2017),color="white")+
  labs(title="APS INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2018),color="white") +
  labs(title="APS INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2019),color="white") +
  labs(title="APS INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2020),color="white") +
  labs(title="APS INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2021),color="white") +
  labs(title="APS INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = APS_2022),color="white") +
  labs(title="APS INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=2,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("APS_2017","APS_2018","APS_2019","APS_2020","APS_2021","APS_2022",geometry) %>%
  gather(VAR, APS, -geometry)%>%
  mutate(APS.INDO = cut_number(APS, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = APS.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan RATA-RATA LAMA SEKOLAH dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2017),color="white")+
  labs(title="RRLS INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2018),color="white") +
  labs(title="RRLS INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2019),color="white") +
  labs(title="RRLS INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2020),color="white") +
  labs(title="IPM di Indonesia Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2021),color="white") +
  labs(title="RRLS INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = RRLS_2022),color="white") +
  labs(title="RRLS INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=2,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("RRLS_2017","RRLS_2018","RRLS_2019","RRLS_2020","RRLS_2021","RRLS_2022",geometry) %>%
  gather(VAR, RRLS, -geometry)%>%
  mutate(RRLS.INDO = cut_number(RRLS, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = RRLS.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan PERSEDIAAN SUMBER AIR MINUM dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2017),color="white")+
  labs(title="PSAM INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2018),color="white") +
  labs(title="PSAM INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2019),color="white") +
  labs(title="PSAM INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2020),color="white") +
  labs(title="PSAM INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2021),color="white") +
  labs(title="PSAM INDOa Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = PSAM_2022),color="white") +
  labs(title="PSAM INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=2,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("PSAM_2017","PSAM_2018","PSAM_2019","PSAM_2020","PSAM_2021","PSAM_2022",geometry) %>%
  gather(VAR, PSAM, -geometry)%>%
  mutate(PSAM.INDO = cut_number(PSAM, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = PSAM.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan GINI RATIO dalam 6 TAHUN
plot.IDN1 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2017),color="white")+
  labs(title="GR INDO Tahun 2017")

plot.IDN2 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2018),color="white") +
  labs(title="GR INDO Tahun 2018")

plot.IDN3 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2019),color="white") +
  labs(title="GR INDO Tahun 2019")

plot.IDN4 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2020),color="white") +
  labs(title="GR INDO Tahun 2020")

plot.IDN5 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2021),color="white") +
  labs(title="GR INDO Tahun 2021")

plot.IDN6 = ggplot(data=gabung.IDN) +
  geom_sf(aes(fill = GR_2022),color="white") +
  labs(title="GR INDO Tahun 2022")

ggarrange(plot.IDN1,plot.IDN2,plot.IDN3,plot.IDN4,plot.IDN5,plot.IDN6,
          nrow=2,common.legend = TRUE,legend = "bottom" )

#Pemetaan Variabel Y
varY <- gabung.IDN %>% select("GR_2017","GR_2018","GR_2019","GR_2020","GR_2021","GR_2022",geometry) %>%
  gather(VAR, GR, -geometry)%>%
  mutate(GR.INDO = cut_number(GR, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = GR.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  #geom_sf_text(aes(label = V1), colour = "black",size=2)+
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#----------------------------------------------------------------------#
#                        UJI EFEK SPASIAL
#----------------------------------------------------------------------#
#Uji pengaruh waktu
plmtest(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,
        data=Data.Panel,type="bp",effect = "time",index = c("No","Tahun"))
#Uji pengaruh Lokasi
plmtest(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,type="bp",effect = "individual",index = c("No","Tahun"))
#Uji pengaruh gabungan
plmtest(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,type="bp",effect = "twoways",index = c("No","Tahun"))

#----------------------------------------------------------------------------
### MODEL PLS, FIXED, dan RANDOM
#----------------------------------------------------------------------------
#Model Random (REM)
modelpanel1<-plm(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,model="random",
                 index = c("No","Tahun"))
summary(modelpanel1)

##Model Fixed (FEM)
modelpanel2<-plm(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR, data=Data.Panel,model="within",
                 index = c("No","Tahun"))
summary(modelpanel2)

#Model PLS
modelpanel3<-plm(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data=Data.Panel,model="pooling",
                 index = c("No","Tahun"))
summary(modelpanel3)


#----------------------------------------------------------
#                      UJI PEMILIHAN MODEL
#----------------------------------------------------------
### uji chow ### (membandingkan OLS dengan FEM)
pFtest(modelpanel2,modelpanel3)

### uji hausman ### (membandingkan FEM dengan REM)
phtest(modelpanel2,modelpanel1)

##Dengan anggapan bahwa model adalah FEM sehingga uji BP_Test langsung modelpanel2
#Uji Keragaman Spasial
bptest(modelpanel2,studentize = FALSE)

#------------------------------------------------------------------------#
#                              GWR PANEL
#------------------------------------------------------------------------#
#----------------------------------------------------------------------------
#PEMODELAN GWR PANEL
#Merubah data ke Spasial Titik Data Frame
data.sp.GWPR=within.trans
coordinates(data.sp.GWPR)=4:5 #kolom 4 dan 5 menyatakan letak Long-Lat
class(data.sp.GWPR)
head(data.sp.GWPR)

bwd.GWPRGAUS<-bw.gwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR, approach = "CV",kernel = "gaussian",adaptive = T)
bwd.GWPRBISQUR<-bw.gwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR, approach = "CV",kernel = "bisquare",adaptive = T)
bwd.GWPREXPO<-bw.gwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR, approach = "CV",kernel = "exponential",adaptive = T)

hasil.GWPRGAUS<-gwr.basic(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR,bw=bwd.GWPRGAUS, kernel = "gaussian",adaptive = T)
summary(hasil.GWPRGAUS)
hasil.GWPRBISQUR<-gwr.basic(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR,bw=bwd.GWPRBISQUR, kernel = "bisquare",adaptive = T)
summary(hasil.GWPRBISQUR)
hasil.GWPREXPO<-gwr.basic(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR,bw=bwd.GWPREXPO, kernel = "exponential",adaptive = T)
summary(hasil.GWPREXPO)

Diagnostic <- cbind(hasil.GWPRGAUS$GW.diagnostic,
      hasil.GWPRBISQUR$GW.diagnostic,
      hasil.GWPREXPO$GW.diagnostic)
colnames(Diagnostic) <- c("Gaussian","Bisquare","Exponential")
Diagnostic

GWPRBISQUR.Result<-gwr.basic(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = data.sp.GWPR,bw=bwd.GWPRBISQUR, kernel = "bisquare",adaptive = T,F123.test = TRUE)
summary(GWPRBISQUR.Result)
GWPRBISQUR.Result$Ftests
#----------------------------------------------------------------------------
# penduga parameter
#Estimasi Model GWPR Lokal (Provinsi)
longlat <- cbind(Data.Panel$LONG[1:34], Data.Panel$LAT[1:34])
Lokasi_Bandwidth <- gw.adapt(dp = longlat, fp = longlat,
                             quant = hasil.GWPRBISQUR$GW.arguments$bw/204) #204 jumlah data

#Nilai Estimasi Parameter, Std. Error dan T-Value Tiap Provinsi
Parameter_GWPR <- as.data.frame(hasil.GWPRBISQUR$SDF)
Parameter_GWPR[1:34,]
write.table(Parameter_GWPR,file="Hasil_Parameter_GWPR.csv",sep=",")

#Nilai P-Value Masing-Masing Parameter Tiap Provinsi
PValue_GWPR <- as.data.frame(gwr.t.adjust(hasil.GWPRBISQUR)$results$p)
PValue_GWPR[1:34,]
write.table(PValue_GWPR,file="Hasil_PValue_GWPR.csv",sep=",")

#Nilai R-Squared Dari Model Tiap Provinsi
RLocal_GWPR <- as.data.frame(hasil.GWPRBISQUR$SDF$Local_R2)
RLocal_GWPR[1:34,]
write.table(RLocal_GWPR,file="Hasil_Rlocal_GWPR.csv",sep=",")

#Pembuatan jarak euclidean
n <- 34 #jumlah wilayah
U <- Data.Panel$LONG #data Longitude
V <- Data.Panel$LAT #data Latitude
d <- matrix(0,n,n)
for (i in 1:n) {
  for (j in 1:n) {
    d[i,j] <- sqrt(((U[i]-U[j])^2)+((U[i]-U[j])^2))
  }
}
d
write.table(RLocal_GWPR,file="Hasil_Jarak_Euclidean.csv",sep=",")

#Menentukan Bobot Penimbang
bdwtBisquare=ggwr.sel(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = Data.Panel,
                      coords=cbind(Data.Panel$LAT,Data.Panel$LONG),adapt=TRUE,gweight=gwr.bisquare)
GRTGB=ggwr(IPM ~ TPT + PP + JPM + IKK + APS + RRLS + PSAM + GR,data = Data.Panel,
           coords=cbind(Data.Panel$LAT,Data.Panel$LONG),adapt = bdwtBisquare,gweight=gwr.bisquare)
GRTGB$bandwidth

bdwtBisquare<- GRTGB$bandwidth
bdwtBisquare<- as.matrix(bdwtBisquare)
bdwtBisquare
i<-nrow(bdwtBisquare)
pembobotB<-matrix(nrow=34,ncol=34)
for(i in 1:34){
  for(j in 1:34){
    pembobotB[i,j]=(1-(d[i,j]/bdwtBisquare[i,])**2)**2
    pembobotB[i,j]<-
      ifelse(d[i,j]<bdwtBisquare[i,],pembobotB[i,j],0)}
}


pembobotB

#----------------------------------------------------------------------------
#Export Hasil analisis GWPR ke Excel
OBJECTID=within.trans$OBJECTID
output.GWPR=as.data.frame(cbind(OBJECTID,Parameter_GWPR,PValue_GWPR,RLocal_GWPR))
write.table(output.GWPR,"hasil GWR Panel Bagus.csv")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel TPT)
#---------------------------------------------------------------#
output.GWPR$signfikansi_TPT <- NA

# Signifikan
output.GWPR[(output.GWPR$TPT_p <= 0.05), "signfikansi_TPT"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$TPT_p >= 0.05), "signfikansi_TPT"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN1 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_TPT)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN1

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel PP)
#---------------------------------------------------------------#
output.GWPR$signfikansi_PP <- NA

# Signifikan
output.GWPR[(output.GWPR$PP_p <= 0.05), "signfikansi_PP"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$PP_p >= 0.05), "signfikansi_PP"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN2 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_PP)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN2

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel JPM)
#---------------------------------------------------------------#
output.GWPR$signfikansi_JPM <- NA

# Signifikan
output.GWPR[(output.GWPR$JPM_p <= 0.05), "signfikansi_JPM"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$JPM_p >= 0.05), "signfikansi_JPM"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN3 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_JPM)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN3

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel IKK)
#---------------------------------------------------------------#
output.GWPR$signfikansi_IKK <- NA

# Signifikan
output.GWPR[(output.GWPR$IKK_p <= 0.05), "signfikansi_IKK"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$IKK_p >= 0.05), "signfikansi_IKK"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN4 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_IKK)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN4

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel APS)
#---------------------------------------------------------------#
output.GWPR$signfikansi_APS <- NA

# Signifikan
output.GWPR[(output.GWPR$APS_p <= 0.05), "signfikansi_APS"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$APS_p >= 0.05), "signfikansi_APS"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN5 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_APS)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN5

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel RRLS)
#---------------------------------------------------------------#
output.GWPR$signfikansi_RRLS <- NA

# Signifikan
output.GWPR[(output.GWPR$RRLS_p <= 0.05), "signfikansi_RRLS"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$RRLS_p >= 0.05), "signfikansi_RRLS"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN6 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_RRLS)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN6

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel PSAM)
#---------------------------------------------------------------#
output.GWPR$signfikansi_PSAM <- NA

# Signifikan
output.GWPR[(output.GWPR$PSAM_p <= 0.05), "signfikansi_PSAM"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$PSAM_p >= 0.05), "signfikansi_PSAM"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN7 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_PSAM)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN7

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel GR)
#---------------------------------------------------------------#
output.GWPR$signfikansi_GR <- NA

# Signifikan
output.GWPR[(output.GWPR$GR_p <= 0.05), "signfikansi_GR"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$GR_p >= 0.05), "signfikansi_GR"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")
View(gabung.IDN2)

plot.IDN8 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_GR)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")

plot.IDN8



#---------------------------------------------------------------#
#    Kriteria variabel (misal variabel IPM)
#---------------------------------------------------------------#
Data.Panel$Kriteria_IPM <- NA

# Rendah
Data.Panel[(Data.Panel$IPM <= 70), "Kriteria_IPM"] <- "Rendah"
# Sedang
Data.Panel[(Data.Panel$IPM == 70), "Kriteria_IPM"] <- "Sedang"
# Tinggi
Data.Panel[(Data.Panel$IPM > 70), "Kriteria_IPM"] <- "Tinggi"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")
#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,Data.Panel,by="OBJECTID")
View(gabung.IDN2)

plot.IDN1 = ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =Kriteria_IPM)) +
  scale_fill_manual(values = c("#DF536B","#00FF00","#28E2E5"))+
  labs(fill="Kriteria IPM")

plot.IDN1

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
Significant_Map <- data.frame(gabung.IDN2[,1:10],output.GWPR[,47:54])
View(Significant_Map)


# Buat kolom baru untuk kombinasi signfikansi x1, x2, dan x3
Significant_Map <- Significant_Map %>%
  mutate(Variabel_Signifikan = case_when(
    # Utuh
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,IKK,APS,RRLS,PSAM,GR",

    # Eliminasi 1
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" ~ "TPT,PP,JPM,IKK,APS,RRLS,PSAM",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,IKK,APS,RRLS,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,IKK,APS,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,IKK,RRLS,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,APS,RRLS,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,IKK,APS,RRLS,PSAM,GR",
    signfikansi_TPT == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,JPM,IKK,APS,RRLS,PSAM,GR",
    signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "PP,JPM,IKK,APS,RRLS,PSAM,GR",

    # Eliminasi 2
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" ~ "TPT,PP,JPM,IKK,APS,RRLS",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,IKK,APS,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,IKK,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,RRLS,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,APS,RRLS,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,IKK,APS,RRLS,PSAM,GR",
    signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "JPM,IKK,APS,RRLS,PSAM,GR",
    signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" ~ "PP,JPM,IKK,APS,RRLS,PSAM",

    # Eliminasi 3
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" ~ "TPT,PP,JPM,IKK,APS",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,IKK,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,RRLS,PSAM,GR",
    signfikansi_TPT == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,APS,RRLS,PSAM,GR",
    signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "IKK,APS,RRLS,PSAM,GR",
    signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" ~ "JPM,IKK,APS,RRLS,PSAM",
    signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" ~ "PP,JPM,IKK,APS,RRLS",

    # Eliminasi 4
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" ~ "TPT,PP,JPM,IKK",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,JPM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PP,PSAM,GR",
    signfikansi_TPT == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,RRLS,PSAM,GR",
    signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "APS,RRLS,PSAM,GR",
    signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" ~ "IKK,APS,RRLS,PSAM",
    signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" ~ "JPM,IKK,APS,RRLS",
    signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" ~ "TPT,PP,JPM,IKK,APS,RRLS,PSAM,GR",

    # Eliminasi 5
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" ~ "TPT,PP,JPM",
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" &
      signfikansi_GR == "Signifikan" ~ "TPT,PP,GR",
    signfikansi_TPT == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,PSAM,GR",
    signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "RRLS,PSAM,GR",
    signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" &
      signfikansi_PSAM == "Signifikan" ~ "APS,RRLS,PSAM",
    signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" ~ "IKK,APS,RRLS",
    signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" &
      signfikansi_APS == "Signifikan" ~ "JPM,IKK,APS",
    signfikansi_PP == "Signifikan" &
      signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" ~ "PP,JPM,IKK",

    # Eliminasi 6
    signfikansi_TPT == "Signifikan" & signfikansi_PP == "Signifikan" ~ "TPT,PP",
    signfikansi_TPT == "Signifikan" & signfikansi_GR == "Signifikan" ~ "TPT,GR",
    signfikansi_PSAM == "Signifikan" & signfikansi_GR == "Signifikan" ~ "PSAM,GR",
    signfikansi_RRLS == "Signifikan" & signfikansi_PSAM == "Signifikan" ~ "RRLS,PSAM",
    signfikansi_APS == "Signifikan" & signfikansi_RRLS == "Signifikan" ~ "APS,RRLS",
    signfikansi_IKK == "Signifikan" & signfikansi_APS == "Signifikan" ~ "IKK,APS",
    signfikansi_JPM == "Signifikan" & signfikansi_IKK == "Signifikan" ~ "JPM,IKK",
    signfikansi_PP == "Signifikan" & signfikansi_JPM == "Signifikan" ~ "PP,JPM",

    # Eliminasi 7
    signfikansi_TPT == "Signifikan" ~ "TPT",
    signfikansi_PP == "Signifikan" ~ "PP",
    signfikansi_JPM == "Signifikan" ~ "JPM",
    signfikansi_IKK == "Signifikan" ~ "IKK",
    signfikansi_APS == "Signifikan" ~ "APS",
    signfikansi_RRLS == "Signifikan" ~ "RRLS",
    signfikansi_PSAM == "Signifikan" ~ "PSAM",
    signfikansi_GR == "Signifikan" ~ "GR",

    TRUE ~ "Tidak Signifikan"
  ))

# Buat skema warna kustom
warna_custom <- c(
  "TPT,PP,JPM,IKK,APS,RRLS,PSAM,GR" = "black",
  "TPT,PP,JPM,IKK,APS,RRLS,PSAM" = "red",
  "TPT,PP,JPM,IKK,APS,RRLS,GR" = "green",
  "TPT,PP,JPM,IKK,APS,PSAM,GR" = "blue",
  "TPT,PP,JPM,IKK,RRLS,PSAM,GR" = "cyan",
  "TPT,PP,JPM,APS,RRLS,PSAM,GR" = "magenta",
  "TPT,PP,IKK,APS,RRLS,PSAM,GR" = "yellow",
  "TPT,JPM,IKK,APS,RRLS,PSAM,GR" = "gray",
  "PP,JPM,IKK,APS,RRLS,PSAM,GR" = "darkgray",
  "TPT,PP,JPM,IKK,APS,RRLS" = "lightgray",
  "TPT,PP,JPM,IKK,APS,GR" = "orange",
  "TPT,PP,JPM,IKK,PSAM,GR" = "brown",
  "TPT,PP,JPM,RRLS,PSAM,GR" = "pink",
  "TPT,PP,APS,RRLS,PSAM,GR" = "violet",
  "TPT,IKK,APS,RRLS,PSAM,GR" = "purple",
  "JPM,IKK,APS,RRLS,PSAM,GR" = "orchid",
  "PP,JPM,IKK,APS,RRLS,PSAM" = "lavender",
  "TPT,PP,JPM,IKK,APS" = "plum",
  "TPT,PP,JPM,IKK,GR" = "maroon",
  "TPT,PP,JPM,PSAM,GR" = "firebrick",
  "TPT,PP,RRLS,PSAM,GR" = "tomato",
  "TPT,APS,RRLS,PSAM,GR" = "aquamarine",
  "IKK,APS,RRLS,PSAM,GR" = "turquoise",
  "JPM,IKK,APS,RRLS,PSAM" = "skyblue",
  "PP,JPM,IKK,APS,RRLS" = "dodgerblue",
  "TPT,PP,JPM,IKK" = "steelblue",
  "TPT,PP,JPM,GR" = "royalblue",
  "TPT,PP,PSAM,GR" = "navyblue",
  "TPT,RRLS,PSAM,GR" = "midnightblue",
  "APS,RRLS,PSAM,GR" = "cornflowerblue",
  "IKK,APS,RRLS,PSAM" = "darkslateblue",
  "JPM,IKK,APS,RRLS" = "darkgreen",
  "PP,JPM,IKK,APS" = "seagreen",
  "TPT,PP,JPM" = "mediumseagreen",
  "TPT,PP,GR" = "darkseagreen",
  "TPT,PSAM,GR" = "lightseagreen",
  "RRLS,PSAM,GR" = "forestgreen",
  "APS,RRLS,PSAM" = "limegreen",
  "IKK,APS,RRLS" = "springgreen",
  "JPM,IKK,APS" = "mediumspringgreen",
  "PP,JPM,IKK" = "darkolivegreen",
  "TPT,PP" = "greenyellow",
  "TPT,GR" = "yellowgreen",
  "PSAM,GR" = "goldenrod",
  "RRLS,PSAM" = "darkgoldenrod",
  "APS,RRLS" = "sienna",
  "IKK,APS" = "saddlebrown",
  "JPM,IKK" = "chocolate",
  "PP,JPM" = "peru",
  "TPT" = "sandybrown",
  "PP" = "tan",
  "JPM" = "khaki",
  "IKK" = "darkkhaki",
  "APS" = "ivory",
  "RRLS" = "beige",
  "PSAM" = "salmon",
  "GR" = "coral",
  "Tidak Signifikan" = "white"
)

ggplot(data = Significant_Map) +
  geom_sf(mapping=aes(geometry = geometry,fill = Variabel_Signifikan)) +
  scale_fill_manual(values = warna_custom)+
  labs(fill="Variabel Signifikan")
