library("dplyr")
library("ggplot2")
library("tidyr")
library("scales")
library("ggpubr")
setwd('C:/Users/Rhys/Desktop/stats/final project/29_Milestone_1/29_Milestone')
Original_data <- read.csv("AllData.csv")

#figure s1
New_data<-data.frame(
  type=c("Regular Research","Sleep","Diet","Physical Activity","Interpersonal Relationships"),
  No_disruption=c(as.integer(count(filter(Original_data,P_Disrupted_Research==1)))
                             ,as.integer(count(filter(Original_data,P_Disrupted_Sleep==1)))
                             ,as.integer(count(filter(Original_data,P_Disrupted_Diet==1)))
                             ,as.integer(count(filter(Original_data,P_Disrupted_PA==1)))
                             ,as.integer(count(filter(Original_data,P_Disrupted_IR==1)))),
  Little=c(as.integer(count(filter(Original_data,P_Disrupted_Research==2)))
           ,as.integer(count(filter(Original_data,P_Disrupted_Sleep==2)))
           ,as.integer(count(filter(Original_data,P_Disrupted_Diet==2)))
           ,as.integer(count(filter(Original_data,P_Disrupted_PA==2)))
           ,as.integer(count(filter(Original_data,P_Disrupted_IR==2)))),
  Moderate=c(as.integer(count(filter(Original_data,P_Disrupted_Research==3)))
             ,as.integer(count(filter(Original_data,P_Disrupted_Sleep==3)))
             ,as.integer(count(filter(Original_data,P_Disrupted_Diet==3)))
             ,as.integer(count(filter(Original_data,P_Disrupted_PA==3)))
             ,as.integer(count(filter(Original_data,P_Disrupted_IR==3)))),
  Significant=c(as.integer(count(filter(Original_data,P_Disrupted_Research==4)))
                ,as.integer(count(filter(Original_data,P_Disrupted_Sleep==4)))
                ,as.integer(count(filter(Original_data,P_Disrupted_Diet==4)))
                ,as.integer(count(filter(Original_data,P_Disrupted_PA==4)))
                ,as.integer(count(filter(Original_data,P_Disrupted_IR==4)))),
  extremely_disruption=c(as.integer(count(filter(Original_data,P_Disrupted_Research==5)))
                         ,as.integer(count(filter(Original_data,P_Disrupted_Sleep==5)))
                         ,as.integer(count(filter(Original_data,P_Disrupted_Diet==5)))
                         ,as.integer(count(filter(Original_data,P_Disrupted_PA==5)))
                         ,as.integer(count(filter(Original_data,P_Disrupted_IR==5))))
  
)

data_long <- gather(New_data, condition, measurement, No_disruption:extremely_disruption, factor_key=TRUE)
plot_s1<-ggplot(data_long,aes(fill=forcats::fct_rev(condition),x=type,y=measurement))+scale_fill_manual(values = c("red3","orchid1","white","lightblue2","blue"))+scale_x_discrete(limits=c("Diet","Interpersonal Relationships","Physical Activity","Sleep","Regular Research"),guide = guide_axis(n.dodge=2))+ theme(legend.position="top",legend.title = element_blank())+
geom_bar(stat="identity",position="fill")+scale_y_continuous(labels = scales::percent)+labs(y = "Respondents[%]")



#Weekly workload
Data_wna<-Original_data[!is.na(Original_data$WH),]
total_wna=nrow(Data_wna)
plot1<-ggplot(Data_wna, aes(x=WH))+ggtitle("Weekly Workload[hrs]")+scale_x_discrete(limits=c("< 30","30-40","40-50","> 50"))+
  geom_bar( fill="steelblue")+labs( y= "# of Respondants")+ylim(0, 250)+theme(panel.grid.major = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_text(stat='count', aes(label=after_stat(percent((count/total_wna),accuracy = 1)),vjust=-1 ))+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))




#research Load
Data_wna_rl<-Original_data[!is.na(Original_data$TWR),]
total_wna_rl<-nrow(Data_wna_rl)
Data_wna_rl$Research_load<-ifelse(Data_wna_rl$TWR<= 20,"<20",
                                  ifelse(Data_wna_rl$TWR== 30,"30",
                                         ifelse(Data_wna_rl$TWR== 40,"40",
                                                ifelse(Data_wna_rl$TWR== 50,"50",
                                                       ifelse(Data_wna_rl$TWR== 60,"60",
                                                              ifelse(Data_wna_rl$TWR>=70,">70","N/A"))))))
                


plot2<-ggplot(Data_wna_rl, aes(x=Research_load))+ggtitle("Research Load[%]")+scale_x_discrete(limits=c("<20","30","40","50","60",">70"))+
  geom_bar( fill="steelblue")+ylim(0, 100)+theme(panel.grid.major = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_text(stat='count', aes(label=after_stat(percent((count/total_wna),accuracy = 1)),vjust=-1 ))+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))

plot2



#Funding Coverage
Data_wna_FF<-Original_data[!is.na(Original_data$Research_Funded_By_External_Grants),]
total_wna_FF<-nrow(Data_wna_rl)

Data_wna_FF$Funds<-ifelse(Data_wna_FF$Research_Funded_By_External_Grants=="Fully funded","FF",
                          ifelse(Data_wna_FF$Research_Funded_By_External_Grants=="100-75%","75-100",
                            ifelse(Data_wna_FF$Research_Funded_By_External_Grants=="25-1%","1-25",
                                  ifelse (Data_wna_FF$Research_Funded_By_External_Grants=="50-25%","25-50",
                                          ifelse (Data_wna_FF$Research_Funded_By_External_Grants=="75-50%","50-75",
                                                  ifelse (Data_wna_FF$Research_Funded_By_External_Grants=="Not funded","NF","NA"))))))

plot3<-ggplot(Data_wna_rl, aes(x=Data_wna_FF$Funds))+ggtitle("Funding Coverage[%]")+scale_x_discrete(limits=c("NF","1-25","25-50","50-75","75-100","FF"))+
  geom_bar( fill="steelblue")+theme(panel.grid.major = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_text(stat='count', aes(label=after_stat(percent((count/total_wna_FF),accuracy = 1)),vjust=-1 ))+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))




#Gender Distribution
Data_wna_Gender<-Original_data[!is.na(Original_data$Gender),]
total_wna_FF<-nrow(Data_wna_Gender)

plot4<-ggplot(Data_wna_Gender, aes(x=Gender))+ggtitle("Gender Distribution")+scale_x_discrete(limits=c("Female","Male"))+
  geom_bar( fill="steelblue",width=0.7)+labs( y= "# of Respondants")+theme(panel.grid.major = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_text(stat='count', aes(label=after_stat(percent((count/total_wna_FF),accuracy = 0.5)),vjust=-1 ))+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))




#Disciplinary Distribution
Data_wna_DD<-Original_data

Data_wna_DD$Disciplinary_Distribution <- with(Data_wna_DD, if_else((Data_wna_DD$Department %in% c("Applied Math","Environmental Studies", "Oceanography", "Chemistry", "Chemistry and Biochemistry","Mathematics","Physics", "Geosciences" )) | (Data_wna_DD$Department_Other %in% c("Applied Math","Environmental Studies", "Chemistry and Biochemistry", "Oceanography",  "Chemistry", "Physics", "Geosciences", "Mathematics") ) , "NAT" ,
                                                             if_else((Data_wna_DD$Department %in% c("Informatics", "Information", "Information and Logistics Technology", "Information Science", "Computer Science", "Scientific Computing")) | (Data_wna_DD$Department_Other %in% c("Informatics", "Information", "Information and Logistics Technology", "Information Science", "Computer Science", "Scientific Computing")) , "CIS" ,
                                                                     ifelse((Data_wna_DD$Department  %in% c("Engineering" ,"Learning Sciences"," Management","Operations Research", "Technology", "Statistical Sciences", "Engineering"))| (Data_wna_DD$Department_Other %in% c("Engineering" ,"Learning Sciences"," Management", "Statistical Sciences","Operations Research", "Technology", "Engineering")),"ENG",                                                                         
                                                                            if_else((Data_wna_DD$Department %in% c("Health", "Health and Human Performance", "HHP", "Neurobiology", "Optometry",  "Neuroscience","Pharmacological and Pharmaceutical Sciences", "Pharmacy", "Vision Science", "Biology", "Medicine", "Pharmacy" ))| (Data_wna_DD$Department_Other %in% c("Health", "Health and Human Performance", "HHP", "Neurobiology", "Neuroscience", "Optometry", "Pharmacological and Pharmaceutical Sciences", "Pharmacy", "Vision Science", "Biology", "Medicine", "Pharmacy")) , "BIO" ,"BEHAV" ))) ))


total_co<-nrow(Data_wna_DD$Disciplinary_Distribution)
plot5<-ggplot(Data_wna_DD, aes(x=Disciplinary_Distribution ))+ggtitle("Disciplinary Distribution")+scale_x_discrete(limits=c("BIO","BEHAV","ENG","CIS","NAT"))+
  geom_bar( fill="steelblue",width=0.4)+theme(panel.grid.major = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_text(stat='count', aes(label=after_stat(percent((count/total_wna_FF),accuracy = 1)),vjust=-1 ))+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))





#Geographical Distribution
Data_wna_gep<-Original_data[!is.na(Original_data$State),]
total_wna_gep=nrow(Data_wna_gep)
Data_wna_gep$geo <- with(Data_wna_gep,ifelse(Data_wna_gep$State %in% c("Connecticut", "Maryland", "Massachusetts", "New Jersey", "New York", "Pennsylvania","Virginia","North Carolina") ,"East",
                                     ifelse(Data_wna_gep$State %in% c("Illinois", "Michigan", "Minnesota", "Ohio"," Wisconsin"),"Midwest",
                                            ifelse(Data_wna_gep$State %in% c("Arizona", "California", "Colorado", "Nevada", "New Mexico", "Oregon", "Utah"),"West",
                                                   "South"))))

plot6<-ggplot(Data_wna_gep, aes(x=geo))+ggtitle("Geographical Distribution")+scale_x_discrete(limits=c("East","West","Midwest","South"))+
  geom_bar( fill="steelblue",width=0.4)+theme(panel.grid.major = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_text(stat='count', aes(label=after_stat(percent((count/total_wna_gep),accuracy = 1)),vjust=-1 ))+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))


test=ggarrange(plot4,plot5,plot6,plot1,plot2,plot3,
               ncol=3,
               nrow=2)
test

