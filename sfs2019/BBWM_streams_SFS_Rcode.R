###########################
###########################

#     BBWM stream recovery
#     Presentation for Society for Freshwater Science --  May 2019
#     Norton, Patel, Fernandez, Nelson

###########################

###########################

## 1. annual values -- hysteresis plots ----

# import file
library(readxl)
BBWM_streams_SFS <- read_excel("BBWM_streams_SFS.xlsx",sheet = "Annual")
names(BBWM_streams_SFS)
BBWM_streams_SFS$Watershed_group=factor(BBWM_streams_SFS$Watershed_group,
                                        levels=c("EB (reference)","WB (pre-treatment)","WB (treatment)","WB (recovery)"))

#volume weighted means
BBWM_streams_SFS$Al_vol=BBWM_streams_SFS$Al/(BBWM_streams_SFS$H2O/BBWM_streams_SFS$Area)
BBWM_streams_SFS$Al_ug_vol=BBWM_streams_SFS$Al_vol*27
BBWM_streams_SFS$H_vol=BBWM_streams_SFS$H/(BBWM_streams_SFS$H2O/BBWM_streams_SFS$Area)

BBWM_streams_SFS$CaMg_vol = BBWM_streams_SFS$Ca_vol+BBWM_streams_SFS$Mg_vol

BBWM_streams_SFS$WY_label=as.factor(BBWM_streams_SFS$WY_label)


library(data.table)
setDT(BBWM_streams_SFS)[WY <1990, WY_group2 := "1989"]
BBWM_streams_SFS[WY>1989 & WY <2000, WY_group2 := "1990-99"]
BBWM_streams_SFS[WY>1999 & WY <2010, WY_group2 := "2000-09"]
BBWM_streams_SFS[WY>2009 & WY <2017, WY_group2 := "2010-16"]
BBWM_streams_SFS[WY>2016, WY_group2 := "2017-18"]
BBWM_streams_SFS$WY_group2=
  factor(BBWM_streams_SFS$WY_group2,
         levels=c("1989","1990-99","2000-09","2010-16","2017-18"))

setDT(BBWM_streams_SFS)[WY <1990, WY_group := "1989"]
BBWM_streams_SFS[WY>1989 & WY <2000, WY_group := "1990-99"]
BBWM_streams_SFS[WY>1999 & WY <2010, WY_group := "2000-09"]
BBWM_streams_SFS[WY>2009 , WY_group := "2010-18"]
BBWM_streams_SFS$WY_group=
  factor(BBWM_streams_SFS$WY_group,
         levels=c("1989","1990-99","2000-09","2010-18"))

attach(BBWM_streams_SFS)

#
##

library(ggplot2)
library(ggrepel)
#

## 1a. SLIDE 4. ---- 
## Ca vs Mg (volume weighted)

CaMg_annual = ggplot(BBWM_streams_SFS,
       aes(Ca_vol,Mg_vol,
           shape=Watershed_group,color=Watershed_group,
           label=WY_label))+
  geom_point(size=3,stroke=1.5)+
  geom_path()+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","black"))+
  scale_shape_manual(values=c(1,19,17,25))+
  geom_text_repel(
    # nudge_x = 150,
    # nudge_x = 150-BBWM_streams_SFS$Ca_vol,
    point.padding = unit(1.5, "lines"),
    box.padding = unit(1, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  labs (x=expression(bold("Ca"^+2*" ("*mu*"eq L"^-1*")")),
        y=expression(bold("Mg"^+2*" (" *mu* "eq L"^-1*")")))+
#  ylim (0,15)+
  
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.position = "none")+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));CaMg_annual


## NO3 vs SO4 (volume weighted)
NS_annual = ggplot(BBWM_streams_SFS,
       aes (NO3_vol,SO4_vol,
            shape=Watershed_group,color=Watershed_group,
            label=WY_label))+
  geom_point(size=3,stroke=1.5)+
  geom_path()+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","black"))+
  scale_shape_manual(values=c(1,19,17,25))+
  geom_text_repel(
    point.padding = unit(1.5, "lines"),
    box.padding = unit(1, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  
  labs (x=expression(bold("NO"[3]^-{}*" ("*mu*"eq L"^-1*")")),
        y=expression(bold("SO"[4]^"-2"*" (" *mu* "eq L"^-1*")")))+
  xlim (-10,70)+
  ylim (40,200)+
  
  theme_bw()+
  theme (legend.title = element_blank())+
  theme(legend.text=element_text(size=12))+
  theme (legend.position = c(0.8,0.2))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black")); NS_annual

plot_grid(CaMg_annual,NS_annual,align="hv")

#
## 1b. SLIDE 5. Al vs pH vol ----
ggplot (BBWM_streams_SFS,
        aes (EQPH,Al_ug_vol,shape=Watershed_group,
             color=Watershed_group,
             label=WY_label))+
  geom_point (size=3,stroke=1.5)+
  geom_path ()+
  scale_color_manual (values=c("green4","royalblue3","darkorange3","black"))+
  scale_shape_manual (values=c(1,19,17,25))+
  geom_text_repel (
    point.padding = unit(1, "lines"),
    box.padding = unit(0.4, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  
  labs (y=expression(bold("Al ("*mu*"g L"^-1*")")),
        x=expression(bold("equilibrated pH")))+
 # ylim (0,3.1)+
  
  theme_bw()+
  theme (legend.title = element_blank())+
  theme(legend.text=element_text(size=14))+
  theme (legend.position = c(0.85,0.85))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'))+
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"))



##
#
## 1c. CaMg vs. NO3SO4 ----
BBWM_streams_SFS$CaMg = BBWM_streams_SFS$Ca_vol+BBWM_streams_SFS$Mg_vol
BBWM_streams_SFS$NS = BBWM_streams_SFS$NO3_vol + BBWM_streams_SFS$SO4_vol
attach(BBWM_streams_SFS)

#EB version 1
CaMg_NS_EB = ggplot (BBWM_streams_SFS[Watershed=="EB",],
                     aes (NS,CaMg,
                         shape=WY_group2,color=WY_group2,
                         label=WY_label))+
  geom_point(size=3,stroke=1.5)+
# geom_path()+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_shape_manual(values=c(1,19,17,4,25))+
  geom_text_repel(
    # nudge_x = 150,
    # nudge_x = 150-BBWM_streams_SFS$Ca_vol,
    point.padding = unit(1.5, "lines"),
    box.padding = unit(1, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  
  labs (y = expression(bold("Ca"^+2 * "+ Mg"^+2 * " ("*mu*"eq L"^-1*")" )),
        x = expression(bold("NO"[3]^-{}*"+SO"[4] ^-2 * " ("*mu*"eq L"^-1*")" )))+
        
  xlim (35,130)+
  ylim (35,130)+
  geom_abline(slope=1,linetype = "dashed")+
  
  ggtitle("East Bear")+
  
  theme_bw()+
  theme (legend.position = c(0.12,0.77))+
  theme (legend.key = element_rect(size = 3)
         )+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));CaMg_NS_EB

#EB version 2
CaMg_NS_EB2 = ggplot (BBWM_streams_SFS[Watershed=="EB",],
                     aes (NS,CaMg,
                          shape=WY_group,color=WY_group,
                          label=WY_label))+
  geom_point(size=3,stroke=1.5)+
 geom_path()+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4"))+
  scale_shape_manual(values=c(1,19,17,4))+
  geom_text_repel(
    # nudge_x = 150,
    # nudge_x = 100-BBWM_streams_SFS$CaMg,
   # position = position_nudge(x=2,y=2),
    point.padding = unit(1.0, "lines"),
    box.padding = unit(1, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  
  labs (y = expression(bold("(Ca"^+2 * "+ Mg"^+2* ") ("*mu*"eq L"^-1*")" )),
        x = expression(bold("(NO"[3]^-{}*"+SO"[4] ^-2* ") ("*mu*"eq L"^-1*")" )))+
  
  annotate("text", label = "1:1", x = 240, y = 250, angle = 45,size=5)+ 
  
  xlim (35,250)+
  ylim (35,250)+
  geom_abline(slope=1,linetype = "dashed")+
  
  ggtitle("East Bear")+
  
  theme_bw()+
  theme (legend.position = c(0.12,0.77))+
  theme (legend.key = element_rect(size = 3)
  )+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));CaMg_NS_EB2


CaMg_NS_WB = ggplot (BBWM_streams_SFS[Watershed=="WB",],
                     aes (NS,CaMg,
                          shape=WY_group,color=WY_group,
                          label=WY_label))+
  geom_point(size=3,stroke=1.5)+
  geom_path()+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_shape_manual(values=c(1,19,17,4,25))+
 
  geom_text_repel(
    # nudge_x = 150,
    # nudge_x = 100-BBWM_streams_SFS$CaMg,
    # position = position_nudge(x=2,y=2),
    point.padding = unit(1.0, "lines"),
    box.padding = unit(0.5, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  
  labs (y = expression(bold("(Ca"^+2 * "+ Mg"^+2* ") ("*mu*"eq L"^-1*")" )),
        x = expression(bold("(NO"[3]^-{}*"+SO"[4] ^-2* ") ("*mu*"eq L"^-1*")" )))+
  
  annotate("text", label = "1:1", x = 240, y = 250, angle = 45,size=5)+ 
  
  xlim (35,250)+
  ylim (35,250)+
  geom_abline(slope=1,linetype = "dashed")+
  
  ggtitle("West Bear")+
  
  theme_bw()+
  theme (legend.position = "none")+
  theme (legend.key = element_rect(size = 3)
  )+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));CaMg_NS_WB

library(cowplot)
plot_grid(CaMg_NS_EB2,CaMg_NS_WB,ncol=2,align="hv")


#
## 2. all data points -- grouped by time period ----

# import file
library(readxl)

BBWM_streams_SFS_conc <- read_excel("BBWM_streams_SFS.xlsx",sheet = "All_stream_samples")
BBWM_streams_SFS_conc$Ca=BBWM_streams_SFS_conc$`Ca (ueq/L)`
BBWM_streams_SFS_conc$Mg=BBWM_streams_SFS_conc$`Mg (ueq/L)`
BBWM_streams_SFS_conc$H=BBWM_streams_SFS_conc$`H+ (ueq/L)`
BBWM_streams_SFS_conc$Al=BBWM_streams_SFS_conc$`Al (ppb)`
BBWM_streams_SFS_conc$Q=BBWM_streams_SFS_conc$`Discharge (L/sec)`
BBWM_streams_SFS_conc$CaMg = BBWM_streams_SFS_conc$`Ca (ueq/L)` + BBWM_streams_SFS_conc$`Mg (ueq/L)`
BBWM_streams_SFS_conc$H2 = BBWM_streams_SFS_conc$`H+ (ueq/L)`*BBWM_streams_SFS_conc$`H+ (ueq/L)`
BBWM_streams_SFS_conc$H3 = BBWM_streams_SFS_conc$`H2`*BBWM_streams_SFS_conc$`H+ (ueq/L)`

#creating new categorical variable -- pretreatment, 2 5-yr periods, recovery
library(data.table)
setDT(BBWM_streams_SFS_conc)[WY <1990, WY_group := "1989"]
BBWM_streams_SFS_conc[WY>1993 & WY <1998, WY_group := "1994-97"]
BBWM_streams_SFS_conc[WY>1999 & WY <2013, WY_group := "2010-12"]
BBWM_streams_SFS_conc[WY>2016, WY_group := "2017-18"]
BBWM_streams_SFS_conc$WY_group=
  factor(BBWM_streams_SFS_conc$WY_group,
         levels=c("1989","1994-97","2010-12","2017-18"))

#creating new categorical variable -- pretreatment, first decade, second decade, third decade, recovery
library(data.table)
setDT(BBWM_streams_SFS_conc)[WY <1990, WY_group2 := "1989"]
BBWM_streams_SFS_conc[WY>1989 & WY <2000, WY_group2 := "1990-99"]
BBWM_streams_SFS_conc[WY>1999 & WY <2010, WY_group2 := "2000-09"]
BBWM_streams_SFS_conc[WY>2009 & WY <2017, WY_group2 := "2010-16"]
BBWM_streams_SFS_conc[WY>2016, WY_group2 := "2017-18"]

BBWM_streams_SFS_conc$WY_group2=
  factor(BBWM_streams_SFS_conc$WY_group2,
         levels=c("1989","1990-99","2000-09","2010-16","2017-18"))

BBWM_streams_SFS_conc$WY_group2=as.factor(BBWM_streams_SFS_conc$WY_group2)
attach(BBWM_streams_SFS_conc)

library(ggplot2)
#
## 2a. SLIDE 6. Ca vs Q ----
# East Bear
CaQ_group2_EB=ggplot (BBWM_streams_SFS_conc[Watershed=="EB",],
                      aes (x=Q,y=Ca,
                           shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  # scale_x_continuous(trans='log2')+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_linetype_manual(values = c("solid","dashed","longdash","dotdash","dotted"))+
  
  labs (y = expression(bold("Ca"^+2 * " ("*mu*"eq L"^-1*")" )),
        x = expression(bold("discharge (L s"^-1*")" )))+
  
  ylim(0,125)+
  xlim(0,320)+
  ggtitle("East Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.position = c(0.85,0.75))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'),
         legend.key.width = unit(3.5,'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
#  plot.title=element_text(family='', face='bold', colour='purple', size=26)
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"));CaQ_group2_EB

#
# West Bear
CaQ_group2_WB=ggplot (BBWM_streams_SFS_conc[Watershed=="WB",],
                      aes (x=Q,y=Ca,
                           shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  # scale_x_continuous(trans='log2')+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_linetype_manual(values = c("solid","dashed","longdash","dotdash","dotted"))+

  labs (x = expression(bold("discharge (L s"^-1*")" )))+
  
  ylim(0,125)+
  xlim(0,320)+
  ggtitle("West Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.position = "none")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.title.y = element_blank(),
         axis.text.y = element_blank())+
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"));CaQ_group2_WB

#
library(cowplot)
plot_grid(CaQ_group2_EB,CaQ_group2_WB,align="hv")


#
## 2a. SLIDE 6. Ca vs Q-log --- not using this ----
# East Bear
CaQ_group2_EB2=ggplot (BBWM_streams_SFS_conc[Watershed=="EB",],
                      aes (x=Q,y=Ca,
                           shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  scale_x_continuous(trans='log2')+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  
  labs (y = expression(bold("Ca"^+2 * " ("*mu*"eq L"^-1*")" )),
        x = expression(bold("log-discharge (L s"^-1*")" )))+
  
  ylim(0,125)+
  ggtitle("East Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme(legend.text=element_text(size=12))+
  theme (legend.position = c(0.85,0.85))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));CaQ_group2_EB2

# West Bear
library(scales)
CaQ_group2_WB=ggplot (BBWM_streams_SFS_conc[Watershed=="WB",],
                      aes (x=Q,y=Ca,
                           shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
 #scale_x_continuous(trans='log2')+
  scale_x_continuous(trans = log2_trans())+
                 #    breaks = trans_breaks("log2", function(x) 2^x),
                  #   labels = trans_format("log2", math_format(2^.x)))+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  
  labs (y = expression(bold("Ca"^+2 * " ("*mu*"eq L"^-1*")" )),
        x = expression(bold("log-discharge (L s"^-1*")" )))+
  
  ylim(0,125)+
  ggtitle("West Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme(legend.text=element_text(size=12))+
  theme (legend.position = c(0.85,0.15))+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));CaQ_group2_WB
#
## 2b. SLIDE 7. Mg vs Q ----
# East Bear
MgQ_group2_EB=ggplot (BBWM_streams_SFS_conc[Watershed=="EB",],
                      aes (x=Q,y=Mg,
                           shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_linetype_manual(values = c("solid","dashed","longdash","dotdash","dotted"))+
  
  labs (y = expression(bold("Mg"^+2 * " ("*mu*"eq L"^-1*")" )),
        x = expression(bold("discharge (L s"^-1*")" )))+
  
  ylim(0,50)+
  xlim(0,300)+
  ggtitle("East Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.position = c(0.85,0.75))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'),
         legend.key.width = unit(3.5,'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"));MgQ_group2_EB

# West Bear
MgQ_group2_WB=ggplot (BBWM_streams_SFS_conc[Watershed=="WB",],
                      aes (x=Q,y=Mg,
                           shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+

    labs (y = expression(bold("Mg"^+2 * " ("*mu*"eq L"^-1*")" )),
        x = expression(bold("discharge (L s"^-1*")" )))+
  
  ylim(0,50)+
  xlim(0,300)+
  ggtitle("West Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.position = "none")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.title.y = element_blank(),
         axis.text.y = element_blank())+
  theme (axis.text=element_text(size=14,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));MgQ_group2_WB

#
library(cowplot)
plot_grid(MgQ_group2_EB,MgQ_group2_WB,align="hv")

##
## 2c. Ca+Mg vs H2 GROUP 1 (don't use) ----
CaMg_group=ggplot (BBWM_streams_SFS_conc[Watershed=="EB",],
                   aes (x=H2,y=CaMg,
                        shape=WY_group,color=WY_group,linetype=WY_group))+
  geom_smooth(na.rm=TRUE)+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","black"))+
  
  labs (y = expression(bold("Ca"^+2 * "+ Mg"^+2 *" ("*mu*"eq L"^-1*")" )),
        x = expression(bold("(H"^+{}*")"^2*" ("*mu*"eq L"^-1*")"^2 )))+
  
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.position = c(0.85,0.15))+
  theme (axis.text=element_text(size=12,face="bold",color="black"),
         axis.title=element_text(size=14,face="bold",color="black"));CaMg_group
#

## 2d. SLIDE 8. CaMg vs H2 GROUP 2 ----
# East Bear
CaMg_group2_EB=ggplot (BBWM_streams_SFS_conc[Watershed=="EB",],
                       aes (x=H2,y=CaMg,
                            shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
#   geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_linetype_manual(values = c("solid","dashed","longdash","dotdash","dotted"))+
  
  labs (y = expression(bold("Ca"^+2 * "+ Mg"^+2 *" ("*mu*"eq L"^-1*")" )),
        x = expression(bold("(H"^+{}*")"^2*" ("*mu*"eq L"^-1*")")))+
  scale_x_continuous(labels = function(CaMg_group2_EB) format(CaMg_group2_EB, big.mark = ",",
                                                              scientific = FALSE))+
  
  ylim(40,175)+
  ggtitle("East Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.position = "none")+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'),
         legend.key.width = unit(3.5,'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"));CaMg_group2_EB

# West Bear
CaMg_group2_WB=ggplot (BBWM_streams_SFS_conc[Watershed=="WB",],
                       aes (x=H2,y=CaMg,
                            shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_linetype_manual(values = c("solid","dashed","longdash","dotdash","dotted"))+
  
  labs (y = expression(bold("Ca"^+2 * "+ Mg"^+2 *" ("*mu*"eq L"^-1*")" )),
        x = expression(bold("(H"^+{}*")"^2*" ("*mu*"eq L"^-1*")")))+
  
  ylim(40,175)+
  scale_x_continuous(labels = function(CaMg_group2_WB) format(CaMg_group2_WB, big.mark = ",",
                                                 scientific = FALSE))+
  ggtitle("West Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.position = c(0.8,0.3))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'),
         legend.key.width = unit(3.5,'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.title.y = element_blank(),
         axis.text.y = element_blank())+
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"));CaMg_group2_WB


plot_grid(CaMg_group2_EB,CaMg_group2_WB,align="hv")


## 2e. SLIDE 9. Al vs. H3 GROUP 2----
# East Bear
Al_group2_EB=ggplot (BBWM_streams_SFS_conc[Watershed=="EB",],
                     aes (x=H3,y=Al,
                          shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_linetype_manual(values = c("solid","dashed","longdash","dotdash","dotted"))+
  
  labs (y = expression(bold("Al ("*mu*"g L"^-1*")")),
        x = expression(bold("(H"^+{}*")"^3*" ("*mu*"eq L"^-1*")")))+
 
   scale_x_continuous(labels = function(BBWM_streams_SFS_conc) format(BBWM_streams_SFS_conc, 
           big.mark = ",",
           scientific = FALSE))+
 
  scale_y_continuous(limits = c(0,1500),
                     labels = function(BBWM_streams_SFS_conc) format(BBWM_streams_SFS_conc, 
                     big.mark = ","))+

  ggtitle("East Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=14))+
  theme (legend.position = c(0.2,0.7))+
  theme (legend.key = element_rect(size = 5),
         legend.key.size = unit(2, 'lines'),
         legend.key.width = unit(3.5,'lines'))+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"));Al_group2_EB

# West Bear
Al_group2_WB=ggplot (BBWM_streams_SFS_conc[Watershed=="WB",],
                     aes (x=H3,y=Al,
                          shape=WY_group2,color=WY_group2,linetype=WY_group2))+
  geom_smooth(na.rm=TRUE)+
  # geom_point(size=1)+
  scale_color_manual(values=c("green4","royalblue3","darkorange3","violetred4","black"))+
  scale_linetype_manual(values = c("solid","dashed","longdash","dotdash","dotted"))+
  
  labs (y = expression(bold("Al ("*mu*"g L"^-1*")")),
        x = expression(bold("(H"^+{}*")"^3*" ("*mu*"eq L"^-1*")")))+
  
  scale_x_continuous(labels = function(BBWM_streams_SFS_conc) format(BBWM_streams_SFS_conc, 
                                                                     big.mark = ",",
                                                                     scientific = FALSE))+
  
  scale_y_continuous(limits = c(0,1500),
                     labels = function(BBWM_streams_SFS_conc) format(BBWM_streams_SFS_conc, 
                                                                     big.mark = ","))+
  ggtitle("West Bear")+
  theme_bw()+
  theme (legend.title = element_blank())+
  theme (legend.text=element_text(size=12))+
  theme (legend.position = "none")+
  theme (plot.title = element_text(hjust = 0.05,size = 14))+
  theme (axis.title.y = element_blank(),
         axis.text.y = element_blank())+
  theme (axis.text=element_text(size=16,face="bold",color="black"),
         axis.title=element_text(size=16,face="bold",color="black"));Al_group2_WB

plot_grid(Al_group2_EB,Al_group2_WB,align="hv")


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
