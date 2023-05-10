#PAL22 BRUVs - data exploration

#Load BRUV MaxN and Biomass data
BRUV_all <- read.csv(file.choose()) 
BRUV_all %>% head(5)

#Load BRUV meta data with depth data
BRUV_meta <- read.csv(file.choose()) 
#Look at the first 5 rows of data
BRUV_meta %>% head(5)

#This merges two data frames with a common ID - here we used Sample
BRUV_database <- merge(BRUV_all,BRUV_meta,by = "Sample")
BRUV_database %>% head(5)

######Look at general patterns of MaxN and biomass
#Group the mean of MaxN and Biomass to String level (mean of 5 rigs to get the string value)
BRUV_String<-BRUV_database %>% 
  dplyr::group_by(String, Zone, Site, Taxa, Binomial, Common.name) %>%
  summarize(t.Biomass=mean(Biomass),
            t.MaxN=mean(MaxN))

#Plot MaxN across zones and sites (Site can be considered similar to string)
BRUV_database %>% 
  ggplot(aes(x = Site, y=MaxN, color=Zone)) +
  geom_jitter()+ ggtitle("MaxN") + theme(axis.text.x=element_text(angle=90,vjust=0.3),strip.text.y = element_text(angle = 0))

#Boxplot
BRUV_String %>% 
  ggplot(aes(x = Site, y=t.MaxN, color=Zone)) +
  geom_boxplot()+ ggtitle("MaxN") + theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0))

#Develop a boxplot with SE of Mean MaxN across Zones
require(wesanderson)
#Not using aggregate data - as this would double average it?
#Using sample level data

BRUV2<-summarySE(BRUV_database, measurevar="MaxN", groupvars=c("Site","Zone"))
BRUV2.Plot1<-ggplot(BRUV2, aes(x=factor(Site), y=MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average MaxN", x="Site")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=MaxN-se, ymax=MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
BRUV2.Plot1

BRUV_database$Site <- factor(BRUV_database$Site, levels = c("N1.1","N1.2","N1.3",
                                                            "N3.1","N3.2","N3.3",
                                                            "N4.1","N4.2",
                                                            "W1.1","W1.2","WA.1","WA.2",
                                                            "W3.1","W4.1","W4.2","W4.3",
                                                            "S1.1","S1.2","S1.3",
                                                            "S2.1","S2.2","S2.3",
                                                            "S3.1","S3.2","S3.3",
                                                            "S4.1","S4.2","S4.3"))
BRUV_database$Zone <- factor(BRUV_database$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

#Look at MaxN of different taxa (subsample to remove the zero's)
No_Zero<-BRUV_database%>% 
  subset(Common.name!="None")
BRUV3<-summarySE(No_Zero, measurevar="MaxN", groupvars=c("Binomial","Zone"))
BRUV3.Plot1<-ggplot(BRUV3, aes(x=factor(Binomial), y=MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average MaxN", x="Fish taxa")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=MaxN-se, ymax=MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
BRUV3.Plot1


#Plot MaxN across depths - just to see what it looks like
BRUV_database %>% 
  ggplot(aes(x = Depth, y=MaxN,color=Zone)) +
  geom_jitter()+ ggtitle("MaxN across depths")+
  scale_color_manual(values= wes_palette("FantasticFox1", n = 3))+
  ylab("MaxN/string")+
  theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0),
  axis.line = element_line(color='grey'),
              legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
              panel.grid.minor = element_blank())

#Group together to look at MaxN across zones - use String data set
BRUV4<-summarySE(BRUV_String, measurevar="t.MaxN", groupvars=c("Zone"))
BRUV4.Plot1<-ggplot(BRUV4, aes(x=factor(Zone), y=t.MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average MaxN", x="Management Zone")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=t.MaxN-se, ymax=t.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),legend.position="none",
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
BRUV4.Plot1



#Plot Biomass across zones and sites
BRUV_database %>% 
  ggplot(aes(x = Site, y=Biomass, color=Zone)) +
  geom_jitter()+ ggtitle("MaxN") + theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0))


BRUV_String %>% 
  ggplot(aes(x = Site, y=t.MaxN, color=Zone)) +
  geom_boxplot()+ ggtitle("MaxN") + theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0))



#Plot NMax for different taxa
BRUV_database %>% 
  ggplot(aes(x = Binomial, y=Biomass, color=Zone)) +
  geom_boxplot()+ ggtitle("MaxN") + theme(axis.text.x=element_text(angle=90,vjust=0.3),strip.text.y = element_text(angle = 0))
