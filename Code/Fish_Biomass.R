#Code to focus on BIOMASS

#Biomass has major outliers - tiger shark and mola
#Subset data to remove tiger shark and Mola.
####Decide if you want to keep in Marlins and silky shark?####
Biomass_data<-BRUV_database%>% 
  subset(Common.name!="tiger shark")
Biomass_data<-Biomass_data%>% 
  subset(Common.name!="sharptail mola")


#Aggregate to string level for biomass data
#This averages the five BRUVs for a single string - so you get the biomass for each string
Biomass_String<-Biomass_data %>% 
  dplyr::group_by(String, Zone, Site, Depth) %>%
  summarize(B.Biomass=mean(Biomass),
            B.MaxN=mean(MaxN))

#Set the order of variables in graphs
Biomass_String$Zone <- factor(Biomass_String$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))
Biomass_String$Site <- factor(Biomass_String$Site, levels = c("N1",
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

#Plot biomass across sites and zones (same as for the abundance plot above)
Biomass1.Plot1<-ggplot(BRUV_String, aes(x=factor(Site), y=B.Biomass, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkgrey",shape=5)+
  labs(y = "Biomass (g)")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass1.Plot1

#Look at biomass across zones only - grouped
Biomass2.Plot1<-ggplot(Biomass_String, aes(x=factor(Zone), y=B.Biomass, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkgrey", shape=5)+
  labs(y = "Biomass (g)")+
  scale_fill_manual(values= wes_palette("Zissou1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),legend.position="none",panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass2.Plot1

#Look at Biomass for each taxa across sites
#The taxa are averaged across a string.
TaxaBiomass.Plot1<-ggplot(BRUV_taxa, aes(x=factor(Binomial), y=Taxa.Biomass))+
  facet_grid(Zone~.)+
  geom_boxplot()+geom_jitter(colour="darkgrey", shape=5)+
  labs(y = "Relative fish abundance (MaxN)")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TaxaBiomass.Plot1