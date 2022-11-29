library("dplyr")
library("ggplot2")
library("tidyr")
library("scales")
library("ggpubr")
library("cowplot")
setwd('C:/Users/mpran/OneDrive/Desktop/UH_Sem2/STAT/Project/')
Original_data<- read.csv("AllData.csv")


##graphs


data_table75 <- read.csv("Milestone-II/Table_$75.csv",fileEncoding = 'UTF-8-BOM')
S_Full<- read.csv("Milestone-II/S_Full.csv",fileEncoding = 'UTF-8-BOM')
G_30<- read.csv("Milestone-II/SG_30.csv",fileEncoding = 'UTF-8-BOM')
G_50_ff<- read.csv("Milestone-II/G50_ff.csv",fileEncoding = 'UTF-8-BOM')
G_50<- read.csv("Milestone-II/G_50_new.csv",fileEncoding = 'UTF-8-BOM')

total=as.integer(count(filter(Original_data,Gender=="Male")))+as.integer(count(filter(Original_data,Gender=="Female")))
Gender_dis<-data.frame(
  type=c("male","female"),
  sample=c(as.integer(count(filter(Original_data,Gender=="Male")))*100/total,as.integer(count(filter(Original_data,Gender=="Female")))*100/total),
  US_UNI=c(64.8,35.2))

chisq.test(Gender_dis$sample,Gender_dis$US_UNI,correct = FALSE)

##figure-2##
##plot_1##
NP_g30<-distinct(G_30,NP,.keep_all=TRUE)
plot_1<-ggplot() +
  geom_linerange(data=NP_g30, mapping=aes(x=NP, ymin=Lower_limit, ymax=Upper_limit ),colour = c("grey","red","red")) + 
  geom_point(data=NP_g30, mapping=aes(x=NP, y=Prediction), size=4,  colour = c("grey","red","red")) + 
  labs(x="" , y="SG30",title = "Number of Proposals") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("NP1","NP2","NP3"))
plot_1
##plot_5##
#Number of Proposals - G50
NP_g50<-distinct(G_50,NP,.keep_all=TRUE)
plot_5<-ggplot() +
  geom_linerange(data=NP_g50, mapping=aes(x=NP, ymin=Lower_Limit, ymax=Upper_Limit),colour = c("grey","orange","red"),size=1) + 
  geom_point(data=NP_g50, mapping=aes(x=NP, y=Prediction), size=4,  colour = c("grey","orange","red")) + 
  labs(x="" , y="SG50",title = "Number of Proposals") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("NP1","NP2","NP3"))
plot_5

##Plot_2##
#Funding Agency - G30
FA_g30<-distinct(G_30,FA,.keep_all=TRUE)
plot_2<-ggplot()+
  geom_linerange(data=FA_g30, mapping=aes(x=FA, ymin=Lower_limit, ymax=Upper_limit),colour = c("grey","black","black","orange","black","orange"),size=1) + 
  geom_point(data=FA_g30, mapping=aes(x=FA, y=Prediction), size=4,  colour = c("grey","black","black","orange","black","orange")) + 
  labs(x="" , y="SG30",title = "Funding Agency") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("NSF","NIH","DoE","DoD","NASA","Other"))
plot_2

##Plot_2##
#Funding Agency - G30
FA_g50<-distinct(G_50,FA,.keep_all=TRUE)
plot_6<-ggplot()+
  geom_linerange(data=FA_g50, mapping=aes(x=FA, ymin=Lower_Limit, ymax=Upper_Limit),colour = c("grey","black","black","lightskyblue1","black","orange"),size=1) + 
  geom_point(data=FA_g50, mapping=aes(x=FA, y=Prediction), size=4,  colour = c("grey","black","black","lightskyblue1","black","orange")) + 
  labs(x="" , y="SG50",title = "Funding Agency") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("NSF","NIH","DoE","DoD","NASA","Other"))
plot_6

plot_bf3<-ggplot(G_30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Break Frequency")

summary_BF_50 <- distinct(G_50,BR,.keep_all=TRUE)
summary_BF_50$BR_new<-with(summary_BF_50,ifelse(summary_BF_50$BR=="Every1or2h","BF1",                                       ifelse(summary_BF_50$BR=="Every3or4h","BF2","NA")))
PLot_7<-
  ggplot()+
  geom_linerange(data=summary_BF_50, mapping=aes(x=BR_new, ymin=Lower_Limit, ymax=Upper_Limit),colour = c("grey","lightskyblue1"),size=1) + 
  geom_point(data=summary_BF_50, mapping=aes(x=BR_new, y=Prediction), size=4,  colour = c("grey","lightskyblue1")) + 
  labs(x="" , y="SG50",title = "Break Frequency") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("BF1","BF2"))
PLot_7
plot_pr4<-ggplot(G_30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Pilot Research")
##PLot-8##
#Pilot Research - G50
PR_g50<-distinct(G_50,AR,.keep_all=TRUE)
plot_8<-ggplot() +
  geom_linerange(data=PR_g50, mapping=aes(x=AR, ymin=Lower_Limit, ymax=Upper_Limit),colour = c("grey","lightskyblue1","orange","lightskyblue1","lightskyblue1"),size=1) + 
  geom_point(data=PR_g50, mapping=aes(x=AR, y=Prediction), size=4,  colour = c("grey","lightskyblue1","orange","lightskyblue1","lightskyblue1")) + 
  labs(x="" , y="SG50",title = "Pilot Research") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("PR1","PR2","PR3","PR4","PR5"))
plot_8
summary_h_30<-distinct(G_30,H,.keep_all=TRUE)
plot_S9<- ggplot(data = summary_h_30, aes(x = H,y=prediction))+scale_x_continuous(limits = c(0,200))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="lightskyblue1" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  geom_vline(xintercept =29.56824 , linetype="dotted", 
             color = "grey", size=1)+
  labs(x="" , y="SG30",title = "h-index")+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_S9

summary_h_50<-distinct(G_50,H,.keep_all=TRUE)
plot_S14<- ggplot(data = summary_h_50, aes(x = H,y=prediction))+scale_x_continuous(limits = c(0,200))+
  geom_smooth(aes( y = Prediction, ymin = Lower_Limit, ymax = Upper_Limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="yellow" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="SG50",title = "h-index")+
  geom_vline(xintercept =29.56824 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_S14

##plot_10##
#Deadline Stress - G30
DS_g30<-distinct(G_30,DS,.keep_all=TRUE)
plot_s10<-ggplot() +
  geom_linerange(data=DS_g30, mapping=aes(x=DS, ymin=Lower_limit, ymax=Upper_limit),colour = c("grey","orange")) + 
  geom_point(data=DS_g30, mapping=aes(x=DS, y=Prediction), size=4,  colour = c("grey","orange")) + 
  labs(x="" , y="SG30",title = "Deadline stress") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("DS1","DS2")) 
plot_s10

summary_S_TA<-distinct(G_30,TA,.keep_all=TRUE)
plot_S11<- ggplot(data = summary_S_TA, aes(x = TA,y=prediction))+scale_x_continuous(limits = c(20,70))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="blue" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="SG30",title = "Trait Anxiety")+
  geom_vline(xintercept =41.33747 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14))

plot_S11

summary_EX_50<-distinct(G_50,EX,.keep_all=TRUE)
plot_S17<- ggplot(data = summary_EX_50, aes(x = EX,y=prediction))+scale_x_continuous(limits = c(2,10))+
  geom_smooth(aes( y = Prediction, ymin = Lower_Limit, ymax = Upper_Limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="orange" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="SG50",title = "Extraversion")+
  geom_vline(xintercept =5.957816, linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_S17

summary_AV_50<-distinct(G_50,AV,.keep_all=TRUE)
plot_S18<- ggplot(data = summary_AV_50, aes(x = AV,y=prediction))+scale_x_continuous(limits = c(10,30))+
  geom_smooth(aes( y = Prediction, ymin = Lower_Limit, ymax = Upper_Limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="blue" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="SG50",title = "Avoidance coping")+
  geom_vline(xintercept =18.81638, linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_S18


plot_ev<-ggplot(DS_g30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Extraversion")

plot_ac<-ggplot(DS_g30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Avoidance Coping")
plot_ds2<-ggplot(DS_g30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Deadline Stress")

plot_TA<-ggplot(DS_g30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  labs(title = "Trait Anxiety")

f=ggarrange(plot_1,plot_2,plot_bf3,plot_pr4,plot_5,plot_6,PLot_7,plot_8,
            ncol=4,
            nrow=2)

b=ggarrange(plot_S9,plot_S14,ncol=1,nrow=2)

c=ggarrange(plot_s10,plot_S11,plot_ev,plot_ac,plot_ds2,plot_TA,plot_S17,plot_S18,ncol=4,nrow=2)


bottom_row <- plot_grid(b, c, labels = c('B', 'C'), label_size = 12)+theme(plot.background = element_rect(color = "black"))



plot_grid(f, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)
FA_75<-distinct(data_table75,FA,.keep_all=TRUE)
plot_21<-ggplot()+
  geom_linerange(data=FA_75, mapping=aes(x=FA, ymin=Lower_limit, ymax=Upper_limit),colour = c("grey","black","lightskyblue1","orange","black","black"),size=1) + 
  geom_point(data=FA_75, mapping=aes(x=FA, y=Prediction), size=4,  colour = c("grey","black","lightskyblue1","orange","black","black")) + 
  labs(x="" , y="S$75",title = "Funding Agency") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("NSF","NIH","DoE","DoD","NASA","Other"))
plot_21

summary_s_full_FA<-distinct(S_Full,FA,.keep_all=TRUE)
plot_24<-ggplot() +
  geom_linerange(data=summary_s_full_FA, mapping=aes(x=FA, ymin=Lower_limit, ymax=Upper_limit),colour = c("grey","black","black","red","black","black"),size=1) + 
  geom_point(data=summary_s_full_FA, mapping=aes(x=FA, y=Prediction), size=4,  colour = c("grey","black","black","red","black","black")) +
  labs(x="" , y="S$$",title = "Funding Agency") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("NSF","NIH","DoE","DoD","NASA","Other")) 
plot_24

TS_75<-distinct(data_table75,T,.keep_all=TRUE)
plot_22<-ggplot() +
  geom_linerange(data=TS_75, mapping=aes(x=T, ymin=Lower_limit, ymax=Upper_limit),colour = c("grey","lightskyblue1"),size=1) + 
  geom_point(data=TS_75, mapping=aes(x=T, y=Prediction), size=4,  colour = c("grey","lightskyblue1")) + 
  labs(x="" , y="S$75",title = "Time of Submission") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("TS1","TS2")) 
plot_22

summary_TWR <-distinct(data_table75,TWR,.keep_all=TRUE)
plot_23<-   ggplot(data = summary_TWR,aes(x = TWR,y=prediction))+scale_x_continuous(limits = c(0,100))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="orange" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="S$75",title = "Typical Week Research")+
  geom_vline(xintercept =42.08437 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_23
Summary_TWR_F<-distinct(S_Full,TWR,.keep_all=TRUE)
plot_26<-   ggplot(data = Summary_TWR_F, aes(x = TWR,y=prediction))+scale_x_continuous(limits = c(0,100))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="red" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="S$$",title = "Typical Week Research")+
  geom_vline(xintercept =42.08437 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_26
summary_h <-distinct(data_table75,H,.keep_all=TRUE)
plot_27<- ggplot(data = summary_h, aes(x = H,y=prediction))+scale_x_continuous(limits = c(0,100))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="blue" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="S$75",title = "h-index")+
  geom_vline(xintercept =29.56824 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_27

RS_75<-distinct(data_table75,RS,.keep_all=TRUE)
RS_75$Score<-0
RS_75$Score<-ifelse(RS_75$RS=="HandsOff",1,2)
plot_28<-ggplot()+
  geom_linerange(data=RS_75, mapping=aes(x=Score, ymin=Lower_limit, ymax=Upper_limit),colour = c("grey","lightskyblue1"),size=1) + 
  geom_point(data=RS_75, mapping=aes(x=Score, y=Prediction), size=4,  colour = c("grey","lightskyblue1")) + 
  labs(x="" , y="",title = "Research Style") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("RS1","RS2")) 
plot_28

summary_o<-distinct(data_table75,OP,.keep_all=TRUE)
plot_29<- ggplot(data = summary_o, aes(x = OP,y=prediction))+scale_x_continuous(limits = c(2,10))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="green" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="",title = "Openness")+
  geom_vline(xintercept =7.513648 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_29

summary_o_F<-distinct(S_Full,OP,.keep_all=TRUE)
plot_212<- ggplot(data = summary_o_F, aes(x = OP,y=prediction))+scale_x_continuous(limits = c(2,10))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="green" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="",title = "Openness")+
  geom_vline(xintercept =7.513648 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_212
plot_h<-ggplot(DS_g30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  labs(y="S$$")

plot_blank<-ggplot(DS_g30)+
  theme(panel.border = element_rect(colour = "black",fill ="white",size=1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14) )
a1=ggarrange(plot_21,plot_22,plot_23,plot_24,plot_blank,plot_26,
             ncol=3,
             nrow=2)
b1=ggarrange(plot_27,plot_28,plot_h,plot_blank,
             ncol=2,
             nrow=2)
c1=ggarrange(plot_29,plot_212,
             ncol=1,
             nrow=2)
bottom_row <- plot_grid(b1, c1, labels = c('B', 'C'), label_size = 12)+theme(plot.background = element_rect(color = "black"))



plot_grid(a1, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)


NP_Sg50<-distinct(G_50_ff,NP,.keep_all=TRUE)
plot_1_Sg50<-ggplot() +
  geom_linerange(data=NP_Sg50, mapping=aes(x=NP, ymin=Lower_limit, ymax=Upper_limit),colour = c("grey","orange","orange"),size=1) + 
  geom_point(data=NP_Sg50, mapping=aes(x=NP, y=Prediction), size=4,  colour = c("grey","orange","orange")) + 
  ylab(bquote(Y-Axis^superscript))+
  labs(x="" , y="S50$$",title = "Number of Proposals") + 
  scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 14) )+ 
  scale_x_discrete(limits=c("NP1","NP2","NP3")) 

plot_1_Sg50


summary_GFF_TWR<-distinct(G_50_ff,TWR,.keep_all=TRUE)
plot_GFF_2<- ggplot(data = summary_GFF_TWR, aes(x = TWR,y=prediction))+scale_x_continuous(limits = c(20,70))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="blue" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+theme(plot.title = element_text(hjust = 0.5, size = 14) )+ scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="SG50$$",title = "Typical Week Research")+
  geom_vline(xintercept =42.08437 , linetype="dotted", 
             color = "Orange", size=1)

plot_GFF_2

summary_GFF_EX<-distinct(G_50_ff,EX,.keep_all=TRUE)
plot_GFF_3<- ggplot(data = summary_GFF_EX, aes(x = EX,y=Prediction))+scale_x_continuous(limits = c(2,10))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="blue" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="SG50$$",title = "Extraversion")+
  geom_vline(xintercept =5.957816 , linetype="dotted", 
             color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_GFF_3

summary_GFF_AGR<-distinct(G_50_ff,AGR,.keep_all=TRUE)
plot_GFF_4<- ggplot(data = summary_GFF_AGR, aes(x = AGR,y=Prediction))+scale_x_continuous(limits = c(4,10))+
  geom_smooth(aes( y = Prediction, ymin = Lower_limit, ymax = Upper_limit), method = "lm" ,formula =  y ~ ns(x, 2) ,color="blue" ,stat="identity", fill="gray", alpha=0.2) + 
  theme_classic()+scale_y_continuous(breaks = seq(0, 1, .25) , labels = percent , limits = c(0, 1))+labs(x="" , y="SG50$$",title = "Agreeableness")+
  geom_vline(xintercept =7.528536 , linetype="dotted", color = "grey", size=1)+theme(plot.title = element_text(hjust = 0.5, size = 14) )
plot_GFF_4

test6=ggarrange(plot_1_Sg50,plot_GFF_2,plot_GFF_3,plot_GFF_4,
                ncol=4,
                nrow=1)
test6
