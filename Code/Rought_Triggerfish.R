#Load BRUV MaxN and Biomass data
BRUV_all <- read.csv(file.choose()) 
BRUV_all %>% head(5)

#Subset to only look at triggerfish

#Subset for triggerfish
BRUV_trigger<-BRUV_all%>% 
  subset(Common.name=="rought triggerfish")
names(BRUV_trigger)

#Aggregate to string
BRUV_trigger<-BRUV_trigger %>% 
  dplyr::group_by(String, Zone, Site, Binomial,Has.fed) %>%
  summarize(t.Biomass=mean(Biomass),
            t.MaxN=mean(MaxN))

#Plot MaxN
Trigger<-summarySE(BRUV_trigger, measurevar="t.MaxN", groupvars=c("Site","Zone"))
Trigger.Plot1<-ggplot(Trigger, aes(x=factor(Site), y=t.MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9), color="grey")+
  labs(y = "Mean MaxN", x="Site", title="Mean MaxN of rought triggerfish across sites")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=t.MaxN-se, ymax=t.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Trigger.Plot1


#Plot Biomass
Trigger2<-summarySE(BRUV_trigger, measurevar="t.Biomass", groupvars=c("Site","Zone"))
Trigger.Plot2<-ggplot(Trigger2, aes(x=factor(Site), y=t.Biomass, fill=Zone))+
  geom_col(position=position_dodge(0.9), color="grey")+
  labs(y = "Mean MaxN", x="Site", title="Mean Biomass of rought triggerfish across sites")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=t.Biomass-se, ymax=t.Biomass+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Trigger.Plot2


#Look at feeding

Trigger.Plot3<-ggplot(BRUV_trigger, aes(x=factor(Site), y=t.MaxN, fill=Has.fed))+
  geom_bar(position="stack", stat="identity")+
  labs(y = "MaxN", x="Site", title="Feeding behaviour of rought triggerfish across sites")+
  scale_fill_manual(values= wes_palette("Zissou1", n = 2))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Trigger.Plot3

#Load FL data
BRUV_FL <- read.csv(file.choose()) 
BRUV_FL %>% head(5)

#Subset for triggerfish
Trigger_FL<-BRUV_FL%>% 
  subset(Binomial=="Canthidermis maculata")
Trigger_FL %>%head(5) 

ggplot(Trigger_FL, aes(Length_mm)) +
  geom_histogram(binwidth = 5,fill="lightblue4")+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())


#Plot sizes
Trigger2<-summarySE(BRUV_trigger, measurevar="t.Biomass", groupvars=c("Site","Zone"))
Trigger.Plot2<-ggplot(Trigger2, aes(x=factor(Site), y=t.Biomass, fill=Zone))+
  geom_col(position=position_dodge(0.9), color="grey")+
  labs(y = "Mean MaxN", x="Site", title="Mean Biomass of rought triggerfish across sites")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=t.Biomass-se, ymax=t.Biomass+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Trigger.Plot2

