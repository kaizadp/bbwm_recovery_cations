###########################
###########################

#     BBWM stream recovery
#     Presentation for Society for Freshwater Science --  May 2019
#     Norton, Patel, Fernandez, Nelson

###########################

###########################

library(tidyverse)
library(readxl)


# I. ANNUAL CONCENTRATIONS ------------------------------------------------
## import file ----
data <- read_excel("sfs2019/BBWM_streams_SFS.xlsx",sheet = "Annual")

## process 
streams = 
  data %>% 
  dplyr::mutate(
    # set levels for Watershed_group
    Watershed_group=factor(Watershed_group,
                                        levels=c("EB (reference)",
                                                 "WB (pre-treatment)",
                                                 "WB (treatment)",
                                                 "WB (recovery)")),
    #volume weighted means
    Al_vol=Al/(H2O/Area),
    Al_ug_vol=Al_vol*27, 
    H_vol=H/(H2O/Area),
    
    # ca+mg
    CaMg_vol = Ca_vol+Mg_vol,
    WY_label=as.factor(WY_label),
    
    # set WY groups
    WY_group2 = case_when(
      WY <1990 ~ "1989",
      (WY>1989 & WY <2000) ~ "1990-99",
      (WY>1999 & WY <2010) ~ "2000-09",
      (WY>2009 & WY <2017) ~ "2010-16",
      WY>2016 ~ "2017-18"),
    WY_group2=factor(WY_group2,
             levels=c("1989","1990-99","2000-09","2010-16","2017-18")),
    
    WY_group = case_when(
      WY <1990 ~ "1989",
      (WY>1989 & WY <2000) ~ "1990-99",
      (WY>1999 & WY <2010) ~ "2000-09",
      WY>2009 ~ "2010-18"),
    WY_group=factor(WY_group,
                     levels=c("1989","1990-99","2000-09","2010-18"))
    )

#
# II. ALL STREAM CONCENTRATIONS -------------------------------------------
## import file -----
conc = read_excel("sfs2019/BBWM_streams_SFS.xlsx",sheet = "All_stream_samples")

## process
streams_conc = 
  conc %>% 
  rename(
    Ca="Ca (ueq/L)",
    Mg="Mg (ueq/L)",
    H="H+ (ueq/L)",
    Al=`Al (ppb)`,
    Q="Discharge (L/sec)") %>%
  
  dplyr::mutate(
    CaMg = Ca + Mg,
    H2 = H*H,
    H3 = H2*H
    ) %>%
  
  dplyr::mutate(
    #creating new categorical variable -- pretreatment, 2 5-yr periods, recovery
    WY_group = case_when(
      WY <1990 ~ "1989",
      (WY>1993 & WY <1998) ~ "1994-97",
      (WY>1999 & WY <2013) ~ "2010-12",
      WY>2016 ~ "2017-18"),
    WY_group=factor(WY_group, levels=c("1989","1994-97","2010-12","2017-18")),
    
    #creating new categorical variable -- pretreatment, first decade, second decade, third decade, recovery
    WY_group2 = case_when(
      WY <1990 ~ "1989",
      (WY>1989 & WY <2000) ~ "1990-99",
      (WY>1999 & WY <2010) ~ "2000-09",
      (WY>2009 & WY <2017) ~ "2010-16",
      WY>2016 ~ "2017-18"),
    WY_group2=factor(WY_group2,levels=c("1989","1990-99","2000-09","2010-16","2017-18"))
    ) %>% 
  
  filter(Watershed %in% c("EB","WB"))
    
#


# III. OUTPUT -------------------------------------------------------------
write.csv(streams, "sfs2019/stream_annual_PROCESSED.csv", row.names = F)
write.csv(streams_conc, "sfs2019/stream_all_PROCESSED.csv", row.names = F)

#

############
############
#
#### OLD GRAPHS ----
##

## time series scatter ----

library(ggplot2)

ggplot(BBWM_streams_SFS,aes(x=WY,y=NO3,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(BBWM_streams_SFS,aes(x=WY,y=SO4,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

#
## line graph ####
library(Rmisc)

sulfate=summarySE(BBWM_streams_SFS,measurevar = "SO4", groupvars=c("WY","Watershed"),na.rm=TRUE)
nitrate=summarySE(BBWM_streams_SFS,measurevar = "NO3", groupvars=c("WY","Watershed"),na.rm=TRUE)

library(ggplot2)

ggplot(sulfate,aes(x=WY,y=SO4,shape=Watershed,color=Watershed,linetype=Watershed))+
 geom_smooth(method=loess)+
#geom_line(size=1)+
  #geom_point(size=2,stroke=1.5)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(nitrate,aes(x=WY,y=NO3,shape=Watershed,color=Watershed,linetype=Watershed))+
    geom_smooth(method=loess)+
  #geom_line(size=1)+
  #geom_point(size=2,stroke=1.5)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(nitrate,aes(x=WY,y=NO3,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_line(size=1)+
  #geom_point(size=2,stroke=1.5)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

#
## to do annual concentrations (dont do) ----
library(Rmisc)
Nitr_annual = summarySEwithin(BBWM_streams_SFS,withinvars = c("Watershed","Year"),measurevar = "nitr",na.rm=TRUE)

ggplot(Nitr_annual,aes(x=Year,y=nitr,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  theme_bw()


#
## NEW GRAPHS scatter ####
library(readxl)
BBWM_stream_chemistry_1989_2018 <- read_excel("BBWM stream chemistry 1989-2018.xlsx")
names(BBWM_stream_chemistry_1989_2018)

BBWM_stream_chemistry_1989_2018$nit = BBWM_stream_chemistry_1989_2018$`NO3 (ueq/L)`
BBWM_stream_chemistry_1989_2018$sulf = BBWM_stream_chemistry_1989_2018$`SO4 (ueq/L)`
BBWM_stream_chemistry_1989_2018$cal = BBWM_stream_chemistry_1989_2018$`Ca (ueq/L)`
BBWM_stream_chemistry_1989_2018$mag = BBWM_stream_chemistry_1989_2018$`Mg (ueq/L)`
BBWM_stream_chemistry_1989_2018$al = BBWM_stream_chemistry_1989_2018$`Al (ppb)`
BBWM_stream_chemistry_1989_2018$doc = BBWM_stream_chemistry_1989_2018$`DOC (mg/L)`
BBWM_stream_chemistry_1989_2018$anc = BBWM_stream_chemistry_1989_2018$`ANC (ueq/L)`
BBWM_stream_chemistry_1989_2018$pH = BBWM_stream_chemistry_1989_2018$`Air-equilibrated pH`
BBWM_stream_chemistry_1989_2018$spcond = BBWM_stream_chemistry_1989_2018$`Specific Conductance (us/cm)`


library(ggplot2)
ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=cal,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("Ca, ueq/L")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=mag,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("Mg, ueq/L")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=al,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("Al, ppb")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=anc,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("ANC")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=nit,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  geom_smooth()+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("NO3, ueq/L")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=sulf,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("SO4, ueq/L")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=pH,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("pH")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=doc,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("DOC, mg/L")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=spcond,shape=Watershed,color=Watershed))+
  geom_point(size=2,stroke=1.5)+
  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("Sp Cond")+
  theme_bw()

## combined scatter and LOESS #### 
library(ggplot2)

ggplot(BBWM_streams_SFS,aes(x=WY,y=NO3_vol))+#,linetype=Watershed,color=Watershed,shape=Watershed))+
#  geom_point(aes(shape=Watershed),color="grey")+
  geom_smooth(size=1.5,aes(linetype=Watershed,fill=Watershed,color=Watershed))+
  scale_shape_manual(values=c(1,4),labels=c("EB","WB",""))+
  scale_color_manual(values=c("black","dodgerblue"))+
  scale_fill_manual(values=c("black","dodgerblue"))+
  
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  labs(x="",y=expression(bold("NO"[3]^-{}*"-N (" * mu * "eq L"^-1*")")))+
  theme_bw()+
  theme(panel.border=element_rect(color="black",size=1.5))+
  theme(axis.text=element_text(size=12,face="bold",color="black"),axis.title=element_text(size=14,face="bold",color="black"))
  

ggplot(BBWM_streams_SFS,aes(x=WY,y=SO4_vol))+#,linetype=Watershed,color=Watershed,shape=Watershed))+
 # geom_point(aes(shape=Watershed),color="grey")+
  geom_smooth(size=1.5,aes(linetype=Watershed,fill=Watershed,color=Watershed))+
  scale_shape_manual(values=c(1,4))+
  scale_color_manual(values=c("black","dodgerblue"))+
  scale_fill_manual(values=c("black","dodgerblue"))+
  
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=spcond))+#,linetype=Watershed,color=Watershed,shape=Watershed))+
  geom_point(aes(shape=Watershed),color="grey")+
  geom_smooth(size=1.5,aes(linetype=Watershed,fill=Watershed,color=Watershed))+
  scale_shape_manual(values=c(1,4))+
  scale_color_manual(values=c("black","dodgerblue"))+
  scale_fill_manual(values=c("black","dodgerblue"))+
  
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year,y=anc))+#,linetype=Watershed,color=Watershed,shape=Watershed))+
  geom_point(aes(shape=Watershed),color="grey")+
  geom_smooth(size=1.5,aes(linetype=Watershed,fill=Watershed,color=Watershed))+
  scale_shape_manual(values=c(1,4))+
  #  scale_linetype_manual(values=c("solid","dashed"))+
  scale_color_manual(values=c("black","dodgerblue"))+
  scale_fill_manual(values=c("black","dodgerblue"))+
  
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()


## Boxplot ####
library(ggplot2)
str(BBWM_stream_chemistry_1989_2018)
BBWM_stream_chemistry_1989_2018$Year2=as.factor(BBWM_stream_chemistry_1989_2018$Year)
ggplot(BBWM_stream_chemistry_1989_2018,aes(x=Year2,y=nit,shape=Watershed,color=Watershed))+
  geom_boxplot()+
  #geom_dotplot(binaxis='y',stackdir='center',dotsize=0.1)+
  theme_bw()

  scale_shape_manual(values=c(1,16))+
  scale_color_manual(values=c("black","darkred"))+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ylab("NO3, ueq/L")+
  theme_bw()
## LINE #### 
library(Rmisc)
sulfate=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "sulf", groupvars=c("Year","Watershed"),na.rm=TRUE)
nitrate=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "nit", groupvars=c("Year","Watershed"),na.rm=TRUE)
calcium=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "cal", groupvars=c("Year","Watershed"),na.rm=TRUE)
aluminum=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "al", groupvars=c("Year","Watershed"),na.rm=TRUE)
ANC=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "anc", groupvars=c("Year","Watershed"),na.rm=TRUE)
magnesium=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "mag", groupvars=c("Year","Watershed"),na.rm=TRUE)
conductivity=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "spcond", groupvars=c("Year","Watershed"),na.rm=TRUE)
dissorgc=summarySE(BBWM_stream_chemistry_1989_2018,measurevar = "doc", groupvars=c("Year","Watershed"),na.rm=TRUE)

library(ggplot2)

ggplot(sulfate,aes(x=Year,y=sulf,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(nitrate,aes(x=Year,y=nit,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(calcium,aes(x=Year,y=cal,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(aluminum,aes(x=Year,y=al,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(ANC,aes(x=Year,y=anc,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(magnesium,aes(x=Year,y=mag,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()


ggplot(nitrate,aes(x=Year,y=nit,shape=Watershed,color=Watershed,linetype=Watershed))+
 # geom_point(geom_point(size=2,stroke=1.5))+
  geom_errorbar(aes(ymax=nit+se,ymin=nit-se))+
  #geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(sulfate,aes(x=Year,y=sulf,shape=Watershed,color=Watershed,linetype=Watershed))+
  # geom_point(geom_point(size=2,stroke=1.5))+
  geom_errorbar(aes(ymax=sulf+se,ymin=sulf-se))+
  #geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(conductivity,aes(x=Year,y=spcond,shape=Watershed,color=Watershed,linetype=Watershed))+
  # geom_point(geom_point(size=2,stroke=1.5))+
  geom_errorbar(aes(ymax=spcond+se,ymin=spcond-se))+
  #geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()

ggplot(dissorgc,aes(x=Year,y=doc,shape=Watershed,color=Watershed,linetype=Watershed))+
  # geom_point(geom_point(size=2,stroke=1.5))+
  geom_errorbar(aes(ymax=doc+se,ymin=doc-se))+
  #geom_smooth(method=loess)+
  geom_vline(xintercept = 1989.2,linetype="dashed")+
  geom_vline(xintercept = 2016.5,linetype="dashed")+
  theme_bw()


## lines, panel fpor each year group ----
BBWM_streams_SFS_conc_1989 = BBWM_streams_SFS_conc[WY<1990,]
BBWM_streams_SFS_conc_1994_97 = BBWM_streams_SFS_conc[WY>1993&WY<1998,]
BBWM_streams_SFS_conc_2010_12 = BBWM_streams_SFS_conc[WY>2009&WY<2011,]
BBWM_streams_SFS_conc_2017_18 = BBWM_streams_SFS_conc[WY>2016&WY<2019,]

# CaMg by year groups
library(ggplot2)

CaMg_1989=ggplot(BBWM_streams_SFS_conc_1989,aes(x=H,y=CaMg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1988-89")+
  theme_bw()

CaMg_1994_97=ggplot(BBWM_streams_SFS_conc_1994_97,aes(x=H,y=CaMg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
#  geom_line(size=1)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
#  geom_vline(xintercept = 1989.2,linetype="dashed")+
#  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1994-97")+
  theme_bw()

CaMg_2010_12=ggplot(BBWM_streams_SFS_conc_2010_12,aes(x=H,y=CaMg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2010-12")+
  theme_bw()

CaMg_2017_18=ggplot(BBWM_streams_SFS_conc_2017_18,aes(x=H,y=CaMg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2017-18")+
  theme_bw()

library(cowplot)
plot_grid(CaMg_1989,CaMg_1994_97,CaMg_2010_12,CaMg_2017_18,labels=c("a","b","c","d"),align="hv",nrow=2,ncol=2)

# Al by year groups
Al_1989=ggplot(BBWM_streams_SFS_conc_1989,aes(x=H,y=Al,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1988-89")+
  theme_bw()

Al_1994_97=ggplot(BBWM_streams_SFS_conc_1994_97,aes(x=H,y=Al,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  #  geom_line(size=1)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1994-97")+
  theme_bw()

Al_2010_12=ggplot(BBWM_streams_SFS_conc_2010_12,aes(x=H,y=Al,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2010-12")+
  theme_bw()

Al_2017_18=ggplot(BBWM_streams_SFS_conc_2017_18,aes(x=H,y=Al,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2017-18")+
  theme_bw()

library(cowplot)
plot_grid(Al_1989,Al_1994_97,Al_2010_12,Al_2017_18,labels=c("a","b","c","d"),align="hv",nrow=2,ncol=2)

# Ca~Q by year groups
CaQ_1989=ggplot(BBWM_streams_SFS_conc_1989,aes(x=Q,y=Ca,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1988-89")+
  theme_bw();CaQ_1989

CaQ_1994_97=ggplot(BBWM_streams_SFS_conc_1994_97,aes(x=Q,y=Ca,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  #  geom_line(size=1)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1994-97")+
  theme_bw()

CaQ_2010_12=ggplot(BBWM_streams_SFS_conc_2010_12,aes(x=Q,y=Ca,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2010-12")+
  theme_bw()

CaQ_2017_18=ggplot(BBWM_streams_SFS_conc_2017_18,aes(x=Q,y=Ca,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2017-18")+
  theme_bw()

library(cowplot)
plot_grid(CaQ_1989,CaQ_1994_97,CaQ_2010_12,CaQ_2017_18,labels=c("a","b","c","d"),align="hv",nrow=2,ncol=2)


# Mg~Q by year groups
MgQ_1989=ggplot(BBWM_streams_SFS_conc_1989,aes(x=Q,y=Mg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1988-89")+
  theme_bw();MgQ_1989

MgQ_1994_97=ggplot(BBWM_streams_SFS_conc_1994_97,aes(x=Q,y=Mg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  #  geom_line(size=1)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("1994-97")+
  theme_bw()

MgQ_2010_12=ggplot(BBWM_streams_SFS_conc_2010_12,aes(x=Q,y=Mg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2010-12")+
  theme_bw()

MgQ_2017_18=ggplot(BBWM_streams_SFS_conc_2017_18,aes(x=Q,y=Mg,shape=Watershed,color=Watershed,linetype=Watershed))+
  geom_smooth(method=lm)+
  geom_point(size=1)+
  #scale_shape_manual(values=c(1,16))+
  #scale_color_manual(values=c("black","darkred"))+
  #  geom_vline(xintercept = 1989.2,linetype="dashed")+
  #  geom_vline(xintercept = 2016.5,linetype="dashed")+
  ggtitle("2017-18")+
  theme_bw()

library(cowplot)
plot_grid(MgQ_1989,MgQ_1994_97,MgQ_2010_12,MgQ_2017_18,labels=c("a","b","c","d"),align="hv",nrow=2,ncol=2)
