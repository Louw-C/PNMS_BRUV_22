#Load edited plankton data
Plankton_all <- read.csv(file.choose()) 
Plankton_all %>% head(5)

#Remove exponential annotation!!!!
options(scipen = 100, digits = 4)

#Aggregate to site level

Plankton_Site<-Plankton_all %>% 
  dplyr::group_by(Site, Zone) %>%
  summarize(Total_Count=mean(Total_Count))

#Make sure that the Zones are ordered correctly
Plankton_Site$Zone <- factor(Plankton_Site$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

#1 - First do a boxplot of all plankton across zones

Plankton_1<-ggplot(Plankton_Site, aes(x=factor(Zone), y=Total_Count))+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  labs(y = "Average plankton abundance")+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Plankton_1

#Try without outliers - copepods at two sites and other gelatinous at one site
#Remove outliers Total_Count larger than 1000

Plankton_new<-Plankton_Site%>% 
  filter(Total_Count < 1000)

Plankton_2<-ggplot(Plankton_new, aes(x=factor(Zone), y=Total_Count))+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  labs(y = "Average plankton abundance")+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Plankton_2


#2 - Second do a boxplot of plankton categories (holo vs mero) across zones

Plankton_2<-ggplot(Plankton_new, aes(x=factor(Zone), y=Total_Count,fill=Plankton_Category))+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  scale_fill_manual(values = wes_palette("FantasticFox1", 3))+
  labs(y = "Average plankton abundance")+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Plankton_2

#3 - Third do a boxplot of plankton groups across zones

Plankton_3<-ggplot(Plankton_new, aes(x=factor(Plankton_Group), y=Total_Count))+
  facet_grid(Zone~.)+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  scale_fill_manual(values = wes_palette("Zissou1", 1))+
  labs(y = "Average plankton abundance")+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, ), strip.text = element_text(
        size = 11))

Plankton_3
#Combining fish and plankton
#Load all data sheets - fish MaxN; Zooplankton, Meta = common factor is Site
BRUV_2022 <- read.csv(file.choose()) 
Plankton_all <- read.csv(file.choose()) 
BRUV_meta <- read.csv(file.choose()) 

Plankton_String<-Plankton_all %>% 
  dplyr::group_by(String, Zone, Plankton_Category,Plankton_Group) %>%
  summarize(Total_Count=mean(Total_Count))
BRUV_String<-BRUV_2022%>% 
  dplyr::group_by(String, Zone, Site) %>%
  summarize(t.Biomass=mean(Biomass),
            t.MaxN=mean(MaxN))

#Merge all data sheets
Fish_Plankton <- merge(BRUV_2022,Plankton_all,by = "String")
Fish_Plankton %>% head(5)

plot(Total_Count~MaxN, Fish_Plankton)
M1<-glm(Total_Count~MaxN, Fish_Plankton)
abline(M1)
summary(M1)



#Plot plankton and fish numbers - NEED TO FIGURE OUT HOW TO COMBINE ABUNDANCES
Fish_Plankton1<-ggplot(Fish_Plankton, aes(x=factor(Plankton_Group), y=Total_Count))+
  facet_grid(Zone~.)+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  scale_fill_manual(values = wes_palette("Zissou1", 1))+
  labs(y = "Average plankton abundance")+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11, ), strip.text = element_text(
          size = 11))

Plankton_3




#Figures looking at fish and plankton

#Load Fish Max N data
Fish<- read.csv(file.choose()) 
Fish %>% head(5)

#Aggregate to site level
Fish_site<-Fish %>% 
  dplyr::group_by(Site, Zone,Taxa, Binomial) %>%
  summarize(Biomass=mean(Biomass),
            MaxN=mean(MaxN))
Fish_site %>% head(5)

####Do the same plots for fish as were done for plankton

#Make sure that the Zones are ordered correctly
Fish_site$Zone <- factor(Fish_site$Zone, levels = c("North", "West", "South"))

#1 - First do a boxplot of all fish across zones

Fish_summary<-summarySE(Fish_site, measurevar="MaxN", groupvars=c("Zone"))
Fish_1<-ggplot(Fish_summary, aes(x=factor(Zone), y=MaxN))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average MaxN")+
  geom_errorbar(aes(ymin=MaxN-se, ymax=MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Fish_1

#2 - Second do a boxplot of fish taxa across zones
#First remove NONE
Fish_site<-subset(Fish_site, Taxa!="None")

Fish_summary2<-summarySE(Fish_site, measurevar="MaxN", groupvars=c("Zone", "Taxa"))
Fish_2<-ggplot(Fish_summary2, aes(x=factor(Taxa), y=MaxN,fill=Zone))+
  geom_col(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Zissou1", 3))+
  labs(y = "Average MaxN")+
  geom_errorbar(aes(ymin=MaxN-se, ymax=MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90, vjust=0.3),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

Fish_2

#3 - Third do a boxplot of fish species across zones
#First remove NONE
Fish_site<-subset(Fish_site, Taxa!="None")

Fish_summary3<-summarySE(Fish_site, measurevar="MaxN", groupvars=c("Zone", "Binomial"))
Fish_3<-ggplot(Fish_summary3, aes(x=factor(Binomial), y=MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9))+
  scale_fill_manual(values = wes_palette("Zissou1", 3))+
  labs(y = "Average plankton abundance")+
  geom_errorbar(aes(ymin=MaxN-se, ymax=MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90, vjust=0.3,face='italic'),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11), strip.text = element_text(
          size = 11))

Fish_3
