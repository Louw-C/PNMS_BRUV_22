#Code to focus on BIOMASS

#Upload data similar MaxN code (BRUV_2022)

#Load BRUV MaxN and Biomass data - BRUV_2022 is your master data file.
BRUV_2022 <- read.csv(file.choose()) 
BRUV_2022 %>% head(5)

#Group the mean of MaxN and Biomass to String level (mean of 5 rigs to get the string value)
#This averages across strings, and shows 3 data points (strings) per site.
Biomass_String<-BRUV_2022 %>% 
  dplyr::group_by(String,Zone,Site) %>%
  summarize(string.Biomass_g=mean(Biomass_g),
            string.Biomass_kg=mean(Biomass_kg),
            string.MaxN=mean(MaxN))

#Remove exponential annotation!!!!
options(scipen = 100, digits = 4)

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


#Plot Biomass across zones and sites - just basic plots to check data
#Data are aggregated (averaged) to string level - so, 3 strings per site (e.g. 3 data points per site)
#This gives average biomass per string (not total!!!!!!!)
Biomass_String %>% 
  ggplot(aes(x = Site, y=string.Biomass_kg, color=Zone)) +
  scale_color_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_jitter()+
  labs(y = "Biomass (g)")+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

###Biomass has major outliers - check data

#Tiger shark (Masturus lanceolatus) = 334934.3100
#Sharptail mola (Masturus lanceolatus) = 134051.6000
#Blue Marlin (makaira nigricans) = 36646.2900 (Two of them)
#Silky shark (Carcharhinus falciformis) = 16176.3200

#String biomass of > 19 kg shows these outliers. 

#Subset data to remove tiger shark and Mola.

#Remove from BRUV_2022 - and then aggregate again and link with meta data

Biomass_sub<-BRUV_2022%>% 
  subset(Common.name!="tiger shark")
Biomass_sub<-Biomass_data%>% 
  subset(Common.name!="sharptail mola")

#Merge with meta data again - using the subset data.
BRUV_biomass <- merge(Biomass_sub,BRUV_meta,by = "Sample")
BRUV_biomass %>% head(5)

#Aggregate again - SUBSET!
BRUV_String_Sub1<-BRUV_biomass %>% 
  dplyr::group_by(String.x, Zone,Site, Depth) %>%
  summarize(string.Biomass_g=mean(Biomass_g),
            string.Biomass_kg=mean(Biomass_kg),
            string.MaxN=mean(MaxN))

#Plot the data again with the subset data - removal of two outliers.
BRUV_String_Sub1 %>% 
  ggplot(aes(x = Site, y=string.Biomass_g, color=Zone)) +
  scale_color_manual(values= wes_palette("FantasticFox1", n = 3))+
  geom_jitter()+
  labs(y = "Average biomass (g)")+
  theme(legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))

#Look at biomass across depths
Depth_biomass<-BRUV_String_Sub1 %>% 
  ggplot(aes(x = Depth, y=string.Biomass_g,color=Zone)) +
  geom_jitter()+
  scale_color_manual(values= wes_palette("FantasticFox1", n = 3))+
  labs(y="Average fish biomass (g)",x="Depth") +
  theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0),
        axis.line = element_line(color='grey'),
        legend.title = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major=element_line(0.5, colour="Gray80"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=11))
Depth_biomass

#Set the order of variables in graphs
BRUV_String_Sub1 $Zone <- factor(BRUV_String_Sub1 $Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))
BRUV_String_Sub1 $Site <- factor(BRUV_String_Sub1 $Site, levels = c("N1",
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

#Boxplot of biomass across sites and zones (same as for the abundance plot above)
Biomass1.Plot1<-ggplot(BRUV_String_Sub1, aes(x=factor(Site), y=string.Biomass_g, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkgrey",shape=5)+
  labs(y = "Average biomass (g)")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass1.Plot1

#Look at biomass across zones only - grouped
Biomass2.Plot1<-ggplot(BRUV_String_Sub1, aes(x=factor(Zone), y=string.Biomass_g, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkgrey", shape=5)+
  labs(y = "Average biomass (g)")+
  scale_fill_manual(values= wes_palette("Zissou1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),legend.position="none",panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass2.Plot1

#Look at Biomass for each taxa across sites
#Aggregate again with Binomial
BRUV_String_Sub2<-BRUV_biomass %>% 
  dplyr::group_by(String.x, Zone,Site,Binomial,Depth) %>%
  summarize(string.Biomass_g=mean(Biomass_g),
            string.Biomass_kg=mean(Biomass_kg),
            string.MaxN=mean(MaxN))

#The taxa are averaged across a string.
TaxaBiomass.Plot1<-ggplot(BRUV_String_Sub2, aes(x=factor(Binomial), y=string.Biomass_g))+
  facet_grid(Zone~.)+
  geom_boxplot()+geom_jitter(colour="darkgrey", shape=5)+
  labs(y = "Relative fish abundance (MaxN)")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TaxaBiomass.Plot1
#Not really informative - so not saving a graph. 