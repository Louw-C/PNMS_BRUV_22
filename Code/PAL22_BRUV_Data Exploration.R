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

######Look at general patterns of MaxN###########

#Group the mean of MaxN and Biomass to String level (mean of 5 rigs to get the string value)
BRUV_String<-BRUV_database %>% 
  dplyr::group_by(String, Zone, Site, Depth) %>%
  summarize(t.Biomass=mean(Biomass),
            t.MaxN=mean(MaxN))

#Set the order of variables in graphs
BRUV_String$Zone <- factor(BRUV_String$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))
BRUV_String$Site <- factor(BRUV_String$Site, levels = c("N1",
                                                        "N3",
                                                        "N4",
                                                        "W1",
                                                        "W2",
                                                        "W3",
                                                        "W4",
                                                        "S1",
                                                        "S2",
                                                        "S3",
                                                        "S4"))

#Plot MaxN across zones and sites - just basic plots to check data
BRUV_String %>% 
  ggplot(aes(x = Site, y=t.MaxN, color=Zone)) +
  scale_color_manual(values= wes_palette("FantasticFox1", n = 3))+
  hrbrthemes::theme_ipsum_rc(axis_title_just="center", axis_title_size=12)+
  geom_jitter()+ ggtitle("MaxN") + theme(axis.text.x=element_text(angle=90,vjust=0.3),strip.text.y = element_text(angle = 0))


#Boxplot
BRUV_String %>% 
  ggplot(aes(x = Site, y=t.MaxN, color=Zone)) +
  geom_boxplot()+ ggtitle("MaxN") + theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0))


require(wesanderson)
#Develop a boxplot with SE of Mean MaxN across Zones
#Using string level data - mean of number of strings deployed in each site
MaxN1<-summarySE(BRUV_String, measurevar="t.MaxN", groupvars=c("Site","Zone"))
MaxN1.Plot1<-ggplot(BRUV2, aes(x=factor(Site), y=t.MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9), color="grey")+
  labs(y = "Mean MaxN", x="Site", title="Mean MaxN across sites")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=t.MaxN-se, ymax=t.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
MaxN1.Plot1

#Look at mean MaxN across depths
#Plot MaxN across depths - just to see what it looks like
BRUV_String %>% 
  ggplot(aes(x = Depth, y=t.MaxN,color=Zone)) +
  geom_jitter()+ ggtitle("MaxN across depths")+
  hrbrthemes::theme_ipsum_rc(axis_title_just="center", axis_title_size=12) + 
  scale_color_manual(values= wes_palette("FantasticFox1", n = 3))+
  labs(y="Mean MaxN",x="Depth", title ="Mean MaxN across depths") +
  theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0),
        axis.line = element_line(color='grey'),
        legend.title = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major=element_line(0.5, colour="Gray80"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=11))

#Group together to look at MaxN across zones - use String data set
MaxN2<-summarySE(BRUV_String, measurevar="t.MaxN", groupvars=c("Zone"))
MaxN2.Plot1<-ggplot(MaxN2, aes(x=factor(Zone), y=t.MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9), color="grey")+
  labs(y = "Mean MaxN", x="Zone", title="Mean MaxN across different management zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=t.MaxN-se, ymax=t.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),legend.position="none",
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
MaxN2.Plot1

#Look at MaxN of different taxa - need to aggregate data again
#Group the mean of MaxN and Biomass to String level - for different taxa
BRUV_taxa<-BRUV_database %>% 
  dplyr::group_by(String, Zone, Site, Binomial, Common.name) %>%
  summarize(Taxa.Biomass=mean(Biomass),
            Taxa.MaxN=mean(MaxN))

#For the plot - take out zero observations
BRUV_taxa<-BRUV_taxa%>% 
  subset(Common.name!="None")

#Set the order of variables in graphs
BRUV_taxa$Zone <- factor(BRUV_taxa$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

#Look mean MaxN for each taxa across sites
TaxaMaxN<-summarySE(BRUV_taxa, measurevar="Taxa.MaxN", groupvars=c("Binomial","Zone"))
TaxaMaxN.Plot1<-ggplot(TaxaMaxN, aes(x=factor(Binomial), y=Taxa.MaxN, fill=Zone))+
  geom_col(position=position_dodge(0.9),color="grey")+
  labs(y = "Mean MaxN", x="Fish taxa", title="Mean MaxN of different fish taxa across zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=Taxa.MaxN-se, ymax=Taxa.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TaxaMaxN.Plot1


#####Move onto biomass

#Biomass has major outliers - tiger shark.
#Subset data to remove tiger shark - massive outlier

Biomass_data<-BRUV_database%>% 
  subset(Common.name!="tiger shark")

#Aggregate to string level for biomass data
Biomass_String<-Biomass_data %>% 
  dplyr::group_by(String, Zone, Site, Depth) %>%
  summarize(B.Biomass=mean(Biomass),
            B.MaxN=mean(MaxN))

Biomass1<-summarySE(Biomass_String, measurevar="B.Biomass", groupvars=c("Site","Zone"))
Biomass1.Plot1<-ggplot(Biomass1, aes(x=factor(Site), y=B.Biomass, fill=Zone))+
  geom_col(position=position_dodge(0.9), color="grey")+
  labs(y = "Mean Biomass (g)", x="Site", title="Mean fish biomass across sites")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=B.Biomass-se, ymax=B.Biomass+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass1.Plot1

#Look at biomass across zones only - grouped
Biomass2<-summarySE(Biomass_String, measurevar="B.Biomass", groupvars=c("Zone"))
Biomass2.Plot1<-ggplot(Biomass2, aes(x=factor(Zone), y=B.Biomass, fill=Zone))+
  geom_col(position=position_dodge(0.9), color="grey")+
  labs(y = "Average Biomass (g)", x = "Zones", title= "Mean biomass across different management zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_errorbar(aes(ymin=B.Biomass-se, ymax=B.Biomass+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass2.Plot1





