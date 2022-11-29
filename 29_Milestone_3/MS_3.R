library("dplyr")
library("lme4")
library("forcats")
library("olsrr")
library("logistf")
library("ciTools")
library("jtools")
library("ggplot2")
library("scales")
library("ggpubr")
library("cowplot")
library("ggplotify")
library("patchwork")
setwd('C:/Users/mpran/OneDrive/Desktop/UH_Sem2/STAT/Project/')
Original_data_key<- read.csv("KeyData.csv")
Original_data_key$NP<-ifelse(Original_data_key$NP==1 , "NP1",
                ifelse(Original_data_key$NP==2, "NP2","NP3"))

Original_data_key$FA<-ifelse(Original_data_key$FA==1,"NSF",
                ifelse(Original_data_key$FA==2,"NIH",
                       ifelse(Original_data_key$FA==3,"DOE",
                              ifelse(Original_data_key$FA==4,"DOD",
                                     ifelse(Original_data_key$FA==5,"NASA",
                                            "OT")))))
Original_data_key$BF<-ifelse(Original_data_key$BF==1,"BF1","BF2")

Original_data_key$AR<-ifelse(Original_data_key$AR==1,"PR1",
                             ifelse(Original_data_key$AR==2,"PR2",
                                    ifelse(Original_data_key$AR==3,"PR3",
                                           ifelse(Original_data_key$AR==4,"PR4","PR5"))))
Original_data_key$DS<-ifelse(Original_data_key$DS==1,"DS1","DS2")
Original_data_key$T<-ifelse(Original_data_key$T==1,"TS1","TS2")
Original_data_key$RS<-ifelse(Original_data_key$RS==1,"RS1","RS2")


Original_model<-Original_data_key[,c("WH","BF","NP","FA","AP","AR","DWH","T","TWR","DWR","Rank",
                                     "RS","H","DS","NASA","TA","E","A","C","N","O","AC","EC","TC","FC","SR")]
Sig_stars <- function(mod_summary_sign) {
  mod_summary_stars <- NA                            
  mod_summary_stars[mod_summary_sign < 0.1] <- "."
  mod_summary_stars[mod_summary_sign < 0.05] <- "*"
  mod_summary_stars[mod_summary_sign < 0.01] <- "**"
  mod_summary_stars[mod_summary_sign < 0.001] <- "***"
  mod_summary_stars[is.na(mod_summary_stars)] <- " "
  return(mod_summary_stars)
}

new<-data.frame(type=c("*","**","***"),
                x=c("3","4","6"),
                y=c("5","4","4"))
leg_plot=ggplot(data = new,aes(x = x, y = y, color = type, linetype = type)) + geom_line()+ 
  theme(legend.title=element_blank(),
        legend.key.height = unit(3, 'cm'),
        legend.key.width = unit(3, 'cm'),
        legend.key=element_blank())+
  scale_linetype_manual(values = rep("solid", 3)) +
  scale_color_manual(values = c("blue", "Orange", "Red"))+
  theme(legend.position = "bottom") +
  theme(legend.box = "vertical") 
 


leg=get_legend(leg_plot)

#SG30------------------------------------------------------
SG30<-Original_model
SG30$SR<-ifelse(SG30$SR==1|SG30$SR==2|SG30$SR==3,0,1)
SG30_O_model<-glm(SR~WH+BF+NP+FA+AP+AR+DWH+T+TWR+DWR+Rank+RS+H+DS+
                      NASA+TA+E+A+C+N+O+AC+EC+TC,family="binomial",data = SG30)
SG30_O_model_F<-glm(SR~1,family="binomial",data = SG30)
backward=step(SG30_O_model)
summary(backward)
forword=step(SG30_O_model_F,scope=list(lower=formula(SG30_O_model_F),upper=formula(SG30_O_model)),direction="forward")
summary(forword)
mixed=step(SG30_O_model_F,scope=list(lower=formula(SG30_O_model_F),upper=formula(SG30_O_model)),direction="both")
summary(mixed)
#SR ~ NP + DS + H + FA + TA + T + RS + DWH (from b,f,m,AIC=424.93)
#SR ~ NP + FA + H + TA(model in manuscript)
SG30<-SG30[,c("NP","FA","T","RS","DWH",
              "H","DS","TA","SR")]
SG30$NP<-fct_relevel(as.factor(SG30$NP),"NP1")
SG30$FA<-fct_relevel(as.factor(SG30$FA),"NSF")
SG30$DS<-fct_relevel(as.factor(SG30$DS),"DS1")
SG30$RS<-fct_relevel(as.factor(SG30$RS),"RS1")
SG30$T<-fct_relevel(as.factor(SG30$T),"TS1")
SG30$DWH<-fct_relevel(as.factor(SG30$DWH),"1")
#model after removing DWS
model_SG30_1<-glm(SR ~ NP + DS + H + FA + TA + T + RS ,family="binomial",data = SG30)
summary(model_SG30_1)#(AIC=425.24)
#model after removing T
model_SG30_2<-glm(SR ~ NP + DS + H + FA + TA + RS ,family="binomial",data = SG30)
summary(model_SG30_2)#(AIC+425.79)
#model after removing RS
model_SG30_3<-glm(SR ~ NP + DS + H + FA + TA  ,family="binomial",data = SG30)
summary(model_SG30_3)#(AIC=426.38)
#no more insig to remove
#final model is model_3
summary(model_SG30_3)
#new_data<-data.frame(
#  NP=c("NP1","NP2","NP3","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1"),
#  DS=c("DS1","DS1","DS1","DS1","DS1","DS1","DS1","DS1","DS1","DS1","DS2","DS1","DS1","DS1","DS1","DS1","DS1"),
#  FA=c("NSF","NSF","NSF","NSF","NIH","DOE","DOD","NASA","OT","NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF"),
#  H=c(mean(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H)
#      ,mean(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H)-sd(SG30$H),
#      mean(SG30$H)+sd(SG30$H),mean(SG30$H),mean(SG30$H),mean(SG30$H)),
#  TA=c(mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),
#       mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),mean(SG30$TA),
#       mean(SG30$TA),mean(SG30$TA)-sd(SG30$TA),mean(SG30$TA)+sd(SG30$TA))
#)

#add_ci(new_data, model_3, names = c("lwr", "upr"), alpha = 0.05) 


list(summary(model_SG30_3)$coefficients[, 4])
odds_ratio <- exp(model_SG30_3$coefficients)
prob_ratio <- round(odds_ratio / (1 + odds_ratio), digits=3)
table_SG30<-data.frame(
  predictor=summary(model_SG30_3)$coefficients[, 0],
  Prob_wise=prob_ratio,
  Odds_ratio=summary(model_SG30_3)$coefficients[, 1],
  std_error=summary(model_SG30_3)$coefficients[, 2],
  Z_val=summary(model_SG30_3)$coefficients[, 3],
  p_val=paste(round(summary(model_SG30_3)$coefficients[, 4],digit=5),Sig_stars(summary(model_SG30_3)$coefficients[, 4]))
  
)


plot_SG30_NP<-effect_plot(model_SG30_3, pred = NP, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG30,color.class = c("Grey","red","Red"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1,),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="SG30",title = "Number of Proposals")+
  scale_x_discrete(limits=c("NP1","NP2","NP3"))

plot_SG30_FA<-effect_plot(model_SG30_3, pred = FA, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG30,color.class = c("Grey","Orange","Black","black","black","Orange"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Funding Agency")+
  scale_x_discrete(limits=c("NSF","NIH","DOE","DOD","NASA","OT"))

plot_SG30_BF<-ggplot(SG30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Break Frequency")+theme(plot.margin = unit(c(1,1,1,1),"cm"))


plot_SG30_PR<-ggplot(SG30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Plot Research")+theme(plot.margin = unit(c(1,1,1,1),"cm"))


plot_SG30_H<-effect_plot(model_SG30_3, pred = H, interval = TRUE,color.class = "lightblue", int.type = "confidence",data = SG30)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG30$H) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="SG30",title = "H-index")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


plot_SG30_DS<-effect_plot(model_SG30_3, pred = DS, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG30,color.class = c("Grey","Orange"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Deadline Stress")+
  scale_x_discrete(limits=c("DS1","DS2"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



plot_SG30_TA<-effect_plot(model_SG30_3, pred = TA, interval = TRUE,color.class = "lightblue", int.type = "confidence",data = SG30)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG30$TA) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Trait Anxiety")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


plot_SG30_E<-ggplot(SG30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Extraversion")+theme(plot.margin = unit(c(1,1,1,1),"cm"))


plot_SG30_AC<-ggplot(SG30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Avoidance Coping")+theme(plot.margin = unit(c(1,1,1,1),"cm"))


#-----------------------------------------
SG50<-Original_data_key
SG50$SR<-ifelse(SG50$SR==1|SG50$SR==2|SG50$SR==3|SG50$SR==4,0,1)
SG50_O_model<-glm(SR~WH+BF+NP+FA+AP+AR+DWH+T+TWR+DWR+Rank+RS+H+DS+
                    NASA+TA+E+A+C+N+O+AC+EC+TC,family="binomial",data = SG50)
SG50_O_model_F<-glm(SR~1,family="binomial",data = SG50)
backward=step(SG50_O_model)
summary(backward)
forword=step(SG50_O_model_F,scope=list(lower=formula(SG50_O_model_F),upper=formula(SG50_O_model)),direction="forward")
summary(forword)

mixed=step(SG50_O_model_F,scope=list(lower=formula(SG50_O_model_F),upper=formula(SG50_O_model)),direction="both")
summary(mixed)
#SR ~ NP + FA + H + E + TC + BF + DS + AC + AR
SG50<-SG50[,c("NP","FA","BF","AR","H","E","AC","TC","DS","SR")]
SG50$NP<-fct_relevel(as.factor(SG50$NP),"NP1")
SG50$FA<-fct_relevel(as.factor(SG50$FA),"NSF")
SG50$BF<-fct_relevel(as.factor(SG50$BF),"BF1")
SG50$AR<-fct_relevel(as.factor(SG50$AR),"PR1")
SG50$DS<-fct_relevel(as.factor(SG50$DS),"DS1")
#model after b,f,m AIC=304.31 SR ~ NP + FA + H + E + TC + BF + DS + AC + AR
#model after removing tc (AIC: 305.07)
model_SG50_1<-glm(SR ~ NP + FA + H + E + BF + DS + AC + AR,family="binomial",data = SG50)
summary(model_SG50_1)
#model after removing DS (AIC: 306.58)
model_SG50_2<-glm(SR ~ NP + FA + H + E + BF  + AC + AR,family="binomial",data = SG50)
summary(model_SG50_2)
#model after removing AC (AIC: 308.41)
model_SG50_3<-glm(SR ~ NP + FA + H + E + BF  + AR,family="binomial",data = SG50)
summary(model_SG50_3)
#Ac is significant
#final model
summary(model_SG50_2)

#new<-data.frame(
#  NP=c("NP1","NP2","NP3","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1","NP1"),
#  FA=c("NSF","NSF","NSF","NSF","NIH","DOE","DOD","NASA","OT","NSF","NSF","NSF",
#       "NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF",
#       "NSF","NSF","NSF","NSF","NSF"),
#  BF=c("BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF2","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1","BF1"
#       ,"BF1","BF1","BF1"),
#  AR=c("PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR1","PR2","PR3","PR4","PR5","PR1","PR1","PR1"
#       ,"PR1","PR1","PR1","PR1","PR1","PR1"),
#  H=c(mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H)
#      ,mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),
#      (mean(SG50$H)-sd(SG50$H)),(mean(SG50$H)+sd(SG50$H)),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H),mean(SG50$H)),
#  E=c(mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),
#      mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E)
#      ,mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E),mean(SG50$E)
#      ,(mean(SG50$E)-sd(SG50$E)),(mean(SG50$E)+sd(SG50$E)),mean(SG50$E),mean(SG50$E),mean(SG50$E)),
#  AC=c(mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC)
#       ,mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC)
#       ,mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC)
#       ,mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC),mean(SG50$AC)
#       ,mean(SG50$AC),(mean(SG50$AC)+sd(SG50$AC)),(mean(SG50$AC)-sd(SG50$AC)))
#  )
#add_ci(new, model_2, names = c("lwr", "upr"), alpha = 0.05) 


list(summary(model_SG50_2)$coefficients[, 4])
odds_ratio <- exp(model_SG50_2$coefficients)
prob_ratio <- round(odds_ratio / (1 + odds_ratio), digits=3)
table_SG50<-data.frame(
  predictor=summary(model_SG50_2)$coefficients[, 0],
  Prob_wise=prob_ratio,
  Odds_ratio=summary(model_SG50_2)$coefficients[, 1],
  std_error=summary(model_SG50_2)$coefficients[, 2],
  Z_val=summary(model_SG50_2)$coefficients[, 3],
  p_val=paste(round(summary(model_SG50_2)$coefficients[, 4],digit=5),Sig_stars(summary(model_SG50_2)$coefficients[, 4]))
  
)


plot_SG50_NP<-effect_plot(model_SG50_2, pred = NP, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG50,color.class = c("Grey","Orange","Red"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="SG30",title = "Number of Proposals")+
  scale_x_discrete(limits=c("NP1","NP2","NP3"))

plot_SG50_FA<-effect_plot(model_SG50_2, pred = FA, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG50,color.class = c("Grey","lightblue","Black","black","black","Orange"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Funding Agency")+
  scale_x_discrete(limits=c("NSF","NIH","DOE","DOD","NASA","OT"))


plot_SG50_BF<-effect_plot(model_SG50_2, pred = BF, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG50,color.class = c("Grey","lightblue"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Break Frequency")+
  scale_x_discrete(limits=c("BF1","BF2"))

plot_SG50_AR<-effect_plot(model_SG50_2, pred = AR, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG50,color.class = c("Grey","lightblue","Orange","lightblue","lightblue"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Pilot Research")+
  scale_x_discrete(limits=c("PR1","PR2","PR3","PR4","PR5"))

plot_SG50_H<-effect_plot(model_SG50_2, pred = H, interval = TRUE,color.class = "Orange", int.type = "confidence",data = SG50)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG50$H) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="SG50",title = "H-index")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_SG50_DS<-ggplot(SG50)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Deadline Stress")+theme(plot.margin = unit(c(1,1,1,1),"cm"))


plot_SG50_TA<-ggplot(SG50)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Trait Anxiety")+theme(plot.margin = unit(c(1,1,1,1),"cm"))

plot_SG50_E<-effect_plot(model_SG50_2, pred = E, interval = TRUE,color.class = "Orange", int.type = "confidence",data = SG50)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG50$E) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Extraversion")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


plot_SG50_AC<-effect_plot(model_SG50_2, pred = AC, interval = TRUE,color.class = "Lightblue", int.type = "confidence",data = SG50)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG50$AC) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Avoidance coping")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


#------------compile Figure-2

#A<-ggarrange(plot_SG30_NP,plot_SG30_FA,plot_SG30_BF,plot_SG30_PR,
#             plot_SG50_NP,plot_SG50_FA,plot_SG50_BF,plot_SG50_AR,
#             nrow = 2,
#             ncol = 4)
#B<-ggarrange(plot_SG30_H,plot_SG50_H,
#             nrow=2,
#             ncol = 1
#            )
#c<-ggarrange(plot_SG30_DS,plot_SG30_TA,plot_SG30_E,plot_SG30_AC,
#             plot_SG50_DS,plot_SG50_TA,plot_SG50_BF,plot_SG50_AC,
#             nrow = 2,
#             ncol = 4)

#((plot_SG30_NP+plot_SG30_FA+plot_SG30_BF+plot_SG30_PR+
#plot_SG50_NP+plot_SG50_FA+plot_SG50_BF+plot_SG50_AR+plot_layout(ncol=4,nrow=2,heights = c(0.5,0.5))))/
# ((as_ggplot(leg)/(((plot_SG30_H/plot_SG50_H)|(
#  (plot_SG30_DS|plot_SG30_TA|plot_SG30_E|plot_SG30_AC)/
#  (plot_SG50_DS|plot_SG50_TA|plot_SG50_BF|plot_SG50_AC))
#)+plot_layout(widths = c(0.5, 2))))+plot_layout(heights  = c(0.5,2)))


p1<-((plot_SG30_NP+plot_SG30_FA+plot_SG30_BF+plot_SG30_PR+
        plot_SG50_NP+plot_SG50_FA+plot_SG50_BF+plot_SG50_AR+plot_layout(ncol=4,nrow=2,heights = c(0.5,0.5))))

p2<-(plot_SG30_H/plot_SG50_H)

p3<-((plot_SG30_DS|plot_SG30_TA|plot_SG30_E|plot_SG30_AC)/
    (plot_SG50_DS|plot_SG50_TA|plot_SG50_E|plot_SG50_AC))

bottom<-plot_grid(p2, p3, labels = c('b', 'c'), label_size = 15,label_fontface="bold", ncol = 2,nrow = 1,rel_widths=c(0.5,2))

p5<- ((as_ggplot(leg)/bottom)+plot_layout(heights  = c(0.5,2)))

Figuer_2<-plot_grid(p1, p5, labels = c('a', ''), label_size = 15,label_fontface="bold", ncol = 1,nrow = 2,
                    theme(plot.background = element_rect(color = "black"))




#----------------------------------------F75``
F75<-Original_data_key
F75$FC<-ifelse(F75$FC==5|F75$FC==6,1,0)

F75_O_model<-glm(FC~WH+BF+NP+FA+AP+AR+DWH+T+TWR+DWR+Rank+RS+H+DS+
                    NASA+TA+E+A+C+N+O+AC+EC+TC,family="binomial",data = F75)
F75_O_model_F<-glm(FC~1,family="binomial",data = F75)
backward=step(F75_O_model)
summary(backward)
forword=step(F75_O_model_F,scope=list(lower=formula(F75_O_model_F),upper=formula(F75_O_model)),direction="forward")
summary(forword)
mixed=step(F75_O_model_F,scope=list(lower=formula(F75_O_model_F),upper=formula(F75_O_model)),direction="both")
summary(mixed)
# model after b.f.m FC ~ FA + T + TWR + RS + H + TA + E + O (AIC: 530.29)
F75<-F75[,c("FC","FA", "T" , "TWR"  ,"RS" , "H" ,"TA" , "E" , "O")]
F75$FA<-fct_relevel(as.factor(F75$FA),"NSF")
F75$T<-fct_relevel(as.factor(F75$T),"TS1")
F75$RS<-fct_relevel(as.factor(F75$RS),"RS1")
#model after removing TA  (AIC: 530.56)
model_F75_1<-glm(FC ~ FA + T + TWR + RS + H  + E + O,family="binomial",data = F75)
summary(model_F75_1)
#model after removing E (AIC: 531.61)
model_F75_2<-glm(FC ~ FA + T + TWR + RS + H  + O ,family="binomial",data = F75)
summary(model_F75_2)
#final model 
summary(model_F75_2)

#new_data<-data.frame(
#  FA=c("NSF","NIH","DOE","DOD","NASA","OT","NSF","NSF",
#       "NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF","NSF"),
#  T=c("TS1","TS1","TS1","TS1","TS1","TS1",
#      "TS1","TS2","TS1","TS1","TS1","TS1","TS1",
#      "TS1","TS1","TS1","TS1","TS1","TS1"),
#  RS=c("RS1","RS1","RS1","RS1","RS1","RS1","RS1","RS1",
#       "RS1","RS2","RS1","RS1","RS1","RS1","RS1","RS1","RS1","RS1","RS1"),
# TWR=c(mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR)
#        ,mean(F75$TWR),mean(F75$TWR)-sd(F75$TWR),mean(F75$TWR)+sd(F75$TWR),mean(F75$TWR),mean(F75$TWR)
#        ,mean(F75$TWR),mean(F75$TWR),mean(F75$TWR),mean(F75$TWR)),
#  H=c(mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H)
#      ,mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H),mean(F75$H)-sd(F75$H),
#      mean(F75$H)+sd(F75$H),mean(F75$H),mean(F75$H),mean(F75$H)),
#  O=c(mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),
#      mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),
#      mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O),mean(F75$O)-sd(F75$O),
#      mean(F75$O)+sd(F75$O))
  
#)
#add_ci(new_data, model_2, names = c("lwr", "upr"), alpha = 0.05) 


plot_F75_FA<-effect_plot(model_F75_2, pred = FA, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = F75,color.class = c("Grey","Orange","lightblue","Black","black","black"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="F75",title = "Funding Agency")+
  scale_x_discrete(limits=c("NSF","NIH","DOE","DOD","NASA","OT"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_F75_TS<-effect_plot(model_F75_2, pred = T, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = F75,color.class = c("Grey","lightblue"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Time of Submission")+
  scale_x_discrete(limits=c("TS1","TS2"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_F75_TWR<-effect_plot(model_F75_2, pred = TWR, interval = TRUE,color.class = "Orange", int.type = "confidence",data = F75)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(F75$TWR) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Typical Week Research")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


plot_F75_H<-effect_plot(model_F75_2, pred = H, interval = TRUE,color.class = "lightblue", int.type = "confidence",data = F75)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(F75$H) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="F75",title = "H-index")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_F75_RS<-effect_plot(model_F75_2, pred = RS, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = F75,color.class = c("Grey","lightblue"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Research Style")+
  scale_x_discrete(limits=c("RS1","RS2"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_F75_O<-effect_plot(model_F75_2, pred = O, interval = TRUE,color.class = "lightblue", int.type = "confidence",data = F75)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(F75$O) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Openness")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



#-----------------------------full
full<-Original_data_key
full$FC<-ifelse(full$FC==6,1,0)
full_O_model<-glm(FC~WH+BF+NP+FA+AP+AR+DWH+T+TWR+DWR+Rank+RS+H+DS+
                   NASA+TA+E+A+C+N+O+AC+EC+TC,family="binomial",data = full)
full_O_model_F<-glm(FC~1,family="binomial",data = full)
backward=step(full_O_model)
summary(backward)
forword=step(full_O_model_F,scope=list(lower=formula(full_O_model_F),upper=formula(full_O_model)),direction="forward")
summary(forword)
mixed=step(full_O_model_F,scope=list(lower=formula(full_O_model_F),upper=formula(full_O_model)),direction="both")
summary(mixed)
#model after b,f,m FC ~ TWR + FA + O + TC + RS + T (AIC: 404.88)
full<-full[,c("FC","FA","TWR","O","TC","RS","T")]
full$FA<-fct_relevel(as.factor(full$FA),"NSF")
full$RS<-fct_relevel(as.factor(full$RS),"RS1")
full$T<-fct_relevel(as.factor(full$T),"TS1")
#model after removing RS (AIC: 405.39)
model_Full_1<-glm(FC~TWR + FA + O + TC + T,family="binomial",data = full)
summary(model_Full_1)
#model after removing T (AIC: 405.61)
model_Full_2<-glm(FC~TWR + FA + O + TC ,family="binomial",data = full)
summary(model_Full_2)
#model after removing TC (AIC: 406.73)
model_Full_3<-glm(FC~TWR + FA + O  ,family="binomial",data = full)
summary(model_Full_3)
#summary(model_3)

#new<-data.frame(FA=c("NSF","NIH","DOE","NASA","DOD","OT","NSF","NSF","NSF","NSF"),
#                 TWR=c(mean(full$TWR),mean(full$TWR),mean(full$TWR),mean(full$TWR),mean(full$TWR),mean(full$TWR),(mean(full$TWR)+sd(full$TWR)),
#                       (mean(full$TWR)-sd(full$TWR)),mean(full$TWR),mean(full$TWR))
#                ,O=c(mean(full$O),mean(full$O),mean(full$O),mean(full$O),mean(full$O),mean(full$O),mean(full$O),mean(full$O),(mean(full$O)+sd(full$O)),
#                     (mean(full$O)-sd(full$O))))

#add_ci(new, model_3, names = c("lwr", "upr"), alpha = 0.05) 




plot_Full_FA<-effect_plot(model_Full_3, pred = FA, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = full,color.class = c("Grey","red","black","Black","black","black"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="Full",title = "Funding Agency")+
  scale_x_discrete(limits=c("NSF","NIH","DOE","DOD","NASA","OT"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_Full_TS<-ggplot(full)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Time of submission")+theme(plot.margin = unit(c(1,1,1,1),"cm"))


plot_Full_TWR<-effect_plot(model_Full_3, pred = TWR, interval = TRUE,color.class = "red", int.type = "confidence",data = full)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(full$TWR) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Typical Week Research")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_Full_h<-ggplot(full)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "h-index")+theme(plot.margin = unit(c(1,1,1,1),"cm"))

plot_Full_RS<-ggplot(full)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Research style")+theme(plot.margin = unit(c(1,1,1,1),"cm"))


plot_Full_O<-effect_plot(model_Full_3, pred = O, interval = TRUE,color.class = "lightblue", int.type = "confidence",data = full)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(full$O) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Openness")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            panel.background = element_blank())



###-------------compile Figure-3


p1<-((plot_F75_FA+plot_F75_TS+plot_F75_TWR+plot_Full_FA+
        plot_Full_TS+plot_Full_TWR+
        plot_layout(ncol=3,nrow=2,heights = c(0.5,0.5))))

p2<-(plot_F75_O/plot_Full_O)

p3<-((plot_F75_H|plot_F75_RS)/
       (plot_Full_h|plot_Full_RS))

bottom<-plot_grid(p3, p2, labels = c('b', 'c'), label_size = 12, ncol = 2,nrow = 1,rel_widths=c(1,0.5))

p5<- ((as_ggplot(leg)/bottom)+plot_layout(heights  = c(0.5,2)))

Figuer_3<-plot_grid(p1, p5, labels = c('a', ''), label_size = 12, ncol = 1,nrow = 2)



#-----------------------------SG50$$
SG50f<-Original_data_key
SG50f$jj<-ifelse(SG50f$FC==6 & (SG50f$SR==5|SG50f$SR==6|SG50f$SR==7),1,0)
SG50f_O_model<-glm(jj~WH+BF+NP+FA+AP+AR+DWH+T+TWR+DWR+Rank+RS+H+DS+
                    NASA+TA+E+A+C+N+O+AC+EC+TC,family="binomial",data = SG50f)
SG50f_O_model_F<-glm(jj~1,family="binomial",data = SG50f)
backward=step(SG50f_O_model)
summary(backward)
forword=step(SG50f_O_model_F,scope=list(lower=formula(SG50f_O_model_F),upper=formula(SG50f_O_model)),direction="forward")
summary(forword)
mixed=step(SG50f_O_model_F,scope=list(lower=formula(SG50f_O_model_F),upper=formula(SG50f_O_model)),direction="both")
summary(mixed)
#model after b,f,m jj ~ NP + TWR + E + A + RS + O, (AIC=159.83)
SG50f<-SG50f[,c("jj","NP" ,"TWR" ,"E" ,"A" , "RS" , "O")]
SG50f$NP<-fct_relevel(as.factor(SG50f$NP),"NP1")
SG50f$RS<-fct_relevel(as.factor(SG50f$RS),"RS1")
#model after removing RS (AIC: 160.52)
model_1<-glm(jj ~ NP + TWR + E + A + O,data=SG50f,family="binomial")
summary(model_1)
#model after removing O (AIC: 160.95)
model_2<-glm(jj ~ NP + TWR + E + A ,data=SG50f,family="binomial")
summary(model_2)#final model



list(summary(model_2)$coefficients[, 4])
odds_ratio <- exp(model_2$coefficients)
prob_ratio <- round(odds_ratio / (1 + odds_ratio), digits=3)

table_5<-data.frame(
  predictor=summary(model_2)$coefficients[, 0],
  Prob_wise=prob_ratio,
  Odds_ratio=summary(model_2)$coefficients[, 1],
  std_error=summary(model_2)$coefficients[, 2],
  Z_val=summary(model_2)$coefficients[, 3],
  p_val=paste(round(summary(model_2)$coefficients[, 4],digit=5),Sig_stars(summary(model_2)$coefficients[, 4]))
  
)


table_5 <- cbind(Predictor = rownames(table_5), table_5)
rownames(table_5) <- 1:nrow(table_5)



plot_1<-effect_plot(model_2, pred = NP, interval = TRUE, cat.interval.geom = "linerange",int.type = "confidence",data = SG50f,color.class = c("Grey","Orange","Orange"))+
  scale_y_continuous( labels = percent ,breaks = seq(0, 1, by = 0.25),limits = c(0,1))+
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14))+
  labs(x="" , y="SG50$$",title = "Number of Proposals")+
  scale_x_discrete(limits=c("NP1","NP2","NP3"))


plot_2<-effect_plot(model_2, pred = TWR, interval = TRUE,color.class = "Orange", int.type = "confidence",data = SG50f)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG50f$TWR) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14) ,axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Typical Week Research")
  

plot_3<-effect_plot(model_2, pred = E, interval = TRUE,color.class = "Blue", int.type = "confidence",data = SG50f)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG50f$E) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14) ,axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Extraversion")

plot_4<-effect_plot(model_2, pred = A, interval = TRUE,color.class = "Blue", int.type = "confidence",data = SG50f)+
  scale_y_continuous( labels = percent ,breaks = seq(0, 2, by = 0.25),limits = c(0,1))+
  geom_vline(xintercept =mean(SG50f$A) , linetype="dotted", color = "grey", size=1)+ 
  theme(panel.border = element_rect(color = "#000000",fill = NA,size = 1),plot.title = element_text(hjust = 0.5, size = 14) ,axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  labs(x="" , y="",title = "Agreeableness")






B<-ggarrange(as_ggplot(leg))
A<-ggarrange(plot_1,plot_2,plot_3,plot_4,
          ncol=4,
          nrow=1)
ggarrange(A,B,nrow = 2,ncol = 1,heights = c(0.98,0.03))









Figuer_4<-((plot_1+plot_2+plot_3+plot_4+
              plot_layout(ncol=4,nrow=1)))/as_ggplot(leg)

Figuer_4
