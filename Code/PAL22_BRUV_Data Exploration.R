#PAL22 BRUVs - data exploration

#Load BRUV MaxN and Biomass data
BRUV_2022 <- read.csv(file.choose()) 
BRUV_2022 %>% head(5)

#Load BRUV meta data with depth data
BRUV_meta <- read.csv(file.choose()) 
#Look at the first 5 rows of data
BRUV_meta %>% head(5)

#This merges two data frames with a common ID - here we used Sample
BRUV_database <- merge(BRUV_2022,BRUV_meta,by = "Sample")
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
  geom_boxplot()+ geom_jitter()+
  ggtitle("MaxN") + theme(axis.text.x=element_text(),strip.text.y = element_text(angle = 0))


require(wesanderson)
#Develop a boxplot with SE of Mean MaxN across Zones
#Using string level data - mean of number of strings deployed in each site
MaxN1.Plot1<-ggplot(BRUV_String, aes(x=factor(Site), y=t.MaxN, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  labs(y = "MaxN", x="Site", title="MaxN across sites")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
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
MaxN2.Plot1<-ggplot(BRUV_String, aes(x=factor(Zone), y=t.MaxN, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  labs(y = "MaxN",title="MaxN across different management zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_blank(),legend.position="none",
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
TaxaMaxN.Plot1<-ggplot(BRUV_taxa, aes(x=factor(Binomial), y=Taxa.MaxN))+
  facet_grid(Zone~.)+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  labs(y = "MaxN", x="Fish taxa", title="MaxN of different fish taxa across zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TaxaMaxN.Plot1

#####Move onto biomass

#Biomass has major outliers - tiger shark.
#Subset data to remove tiger shark - massive outlier; also remove Mola.
#Decide if you want to keep in Marlins and silky shark?####
Biomass_data<-BRUV_database%>% 
  subset(Common.name!="tiger shark")
Biomass_data<-Biomass_data%>% 
  subset(Common.name!="sharptail mola")


#Aggregate to string level for biomass data
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

Biomass1.Plot1<-ggplot(Biomass_String, aes(x=factor(Site), y=B.Biomass, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkblue",shape=5)+
  labs(y = "Biomass (g)", x="Site", title="Fish biomass across sites")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass1.Plot1

#Look at biomass across zones only - grouped
Biomass2.Plot1<-ggplot(Biomass_String, aes(x=factor(Zone), y=B.Biomass, fill=Zone))+
  geom_boxplot()+geom_jitter(colour="darkblue", shape=5)+
  labs(y = "Biomass (g)", x = "Zones", title= "Biomass across different management zones")+
  scale_fill_manual(values= wes_palette("Zissou1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),legend.position="none",panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Biomass2.Plot1





