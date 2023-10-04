library(ggplot2)
library(viridis)
library(tidyverse)

##using Sheet 1 data
all_cost <- Zambia_Cost_Model
ggplot(Zambia_Cost_Model, aes(fill=cost_cat, y=cost, x=period)) + 
  geom_bar(position="fill", stat="identity")+
  camre_style()+
  scale_fill_camre(discrete = "TRUE")+
  ggtitle("Proportion Cost by Intervention Period") +
  xlab("") +
  ylab("Proportion")+
  labs(fill= "Category")
  
  

ggplot(Zambia_Cost_Model, aes(fill=cost_cat, y=cost, x='Before Deployment')) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Cost Category Pre-Deployment") +
  xlab("") +
  ylab("Cost")+
  scale_y_continuous(labels = scales::dollar_format())+
  labs(fill= "Category")

ggplot(Zambia_Cost_Model, aes(fill=cost_cat, y=cost, x='Deployment')) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Cost Category Deployment") +
  xlab("") +
  ylab("Cost")+
  scale_y_continuous(labels = scales::dollar_format())+
  labs(fill= "Category")

ggplot(Zambia_Cost_Model, aes(fill=cost_cat, y=cost, x='Monitoring')) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Cost Category Monitoring") +
  xlab("") +
  ylab("Cost")+
  scale_y_continuous(labels = scales::dollar_format())+
  labs(fill= "Category")

####Box and Whisker for Cluster Level Costs
#Import cluster cost sheet from Zambia Cost Model

cluster_cost<-Zambia_Cost_Model

#Per Person
ggplot(Zambia_Cost_Model, aes(x=group, y=cost_pp)) +
  geom_boxplot()+
  camre_style()+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  expand_limits(x=1, y=c(0,35))+
  xlab("Per Person")+
  ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Person")

#Per ATSB
ggplot(Zambia_Cost_Model, aes(x=group, y=cost_atsb))+
           geom_boxplot()+
  camre_style()+
  expand_limits(x=1, y=c(0,35))+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
           xlab("Per ATSB")+
           ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per ATSB")

#Per Structure
ggplot(Zambia_Cost_Model, aes(x=group, y=cost_struc))+
  geom_boxplot()+
  camre_style()+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  expand_limits(x=1, y=c(0,35))+
  xlab("Per Structure")+
  ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Structure")  

#Per Household
ggplot(Zambia_Cost_Model, aes(x=group, y=cost_hh))+
geom_boxplot()+
  camre_style()+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  xlab("Per Household")+
  ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Household")


ggplot(Zambia_Cost_Model, aes(cost_pp, cost_atsb))



# install.packages("devtools")
devtools::install_github("Bruke08/camrePalettes")


#Create Violin Plot for Cost per Household
#import 'cluster cost' tab from excel sheet
ggplot(Zambia_Cost_Model, aes(x=group, y=cost_hh, fill="group")) + 
  geom_violin(fill="#1F6633")+
  geom_boxplot(fill="white")+
  camre_style()+
    theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  xlab("Per Household")+
  ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Household")

ggplot(Zambia_Cost_Model, aes(x=group, y=cost_struc, fill="group")) + 
  geom_violin(fill="#D4E676")+
  geom_boxplot(fill="white")+
  camre_style()+
  expand_limits(x=1, y=c(0,35))+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  xlab("Per Household")+
  ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Structure")


ggplot(Zambia_Cost_Model, aes(x=group, y=cost_atsb, fill="group")) + 
  geom_violin(fill="#58AE95")+
  geom_boxplot(fill="white")+
  camre_style()+
  expand_limits(x=1, y=c(0,35))+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  xlab("Per Household")+
  ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per ATSB")

ggplot(Zambia_Cost_Model, aes(x=group, y=cost_pp, fill="group")) + 
  geom_violin(fill="#9BBC21")+
  geom_boxplot(fill="white")+
  camre_style()+
  expand_limits(x=1, y=c(0,35))+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  xlab("Per Household")+
  ylab("Cost USD")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Person")






rm(list=ls())

####Cluster level costs using cluster cost sheet
#Total Cluster Costs
ggplot(data=Zambia_Cost_Model, aes(x=Cluster, y=Cost)) +
  geom_bar(stat="identity", fill="steelblue")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Total Cost by Cluster Year 1")

#Cost per person per cluster
ggplot(data=Zambia_Cost_Model, aes(x=Cluster, y=cost_pp)) +
  geom_bar(stat="identity", fill="steelblue")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Person by Cluster Year 1")+
  ylab("Cost")

#Cost per atsb per cluster
ggplot(data=Zambia_Cost_Model, aes(x=Cluster, y=cost_atsb)) +
  geom_bar(stat="identity", fill="seagreen")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per ATSB by Cluster Year 1")+
  ylab("Cost")

#Cost per Structure
ggplot(data=Zambia_Cost_Model, aes(x=Cluster, y=cost_struc)) +
  geom_bar(stat="identity", fill="lightslateblue")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Structure by Cluster Year 1")+
  ylab("Cost")

ggplot(data=Zambia_Cost_Model, aes(x=Cluster, y=cost_hh)) +
  geom_bar(stat="identity", fill="yellow4")+
  scale_y_continuous(labels = scales::dollar_format())+
  ggtitle("Cost per Household by Cluster Year 1")+
  ylab("Cost")

