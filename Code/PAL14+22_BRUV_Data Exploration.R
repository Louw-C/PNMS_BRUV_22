#PAL22 BRUVs - data exploration

#Load BRUV MaxN and Biomass data
BRUV_2014 <- read.csv(file.choose()) 
BRUV_2014 %>% head(5)


######Look at general patterns of MaxN###########

#Look at MaxN of different taxa - need to aggregate data again
#Group the mean of MaxN and Biomass to String level - for different taxa
BRUV14_taxa<-BRUV_2014 %>% 
  dplyr::group_by(String, Binomial, Common.name) %>%
  summarize(Taxa.Biomass=mean(Biomass),
            Taxa.MaxN=mean(MaxN))

#For the plot - take out zero observations
BRUV14_taxa<-BRUV14_taxa%>% 
  subset(Common.name!="None")

#Look mean MaxN for each taxa across sites
TaxaMaxN<-summarySE(BRUV14_taxa, measurevar="Taxa.MaxN", groupvars=c("Binomial"))
TaxaMaxN.Plot1<-ggplot(TaxaMaxN, aes(x=factor(Binomial), y=Taxa.MaxN))+
  geom_col(position=position_dodge(0.9),color="grey")+
  labs(y = "Mean MaxN", x="Fish taxa", title="Mean MaxN of different fish taxa across zones")+
  geom_errorbar(aes(ymin=Taxa.MaxN-se, ymax=Taxa.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TaxaMaxN.Plot1


#####Look at Max N for species for both 2014 and 2023

#Load BRUV MaxN and Biomass data
MaxN_all <- read.csv(file.choose()) 
MaxN_all %>% head(5)



######Look at general patterns of MaxN###########

#Look at MaxN of different taxa - need to aggregate data again
#Group the mean of MaxN and Biomass to String level - for different taxa
MaxN_all_taxa<-MaxN_all %>% 
  dplyr::group_by(String,Year, Binomial, Common.name) %>%
  summarize(Taxa.Biomass=mean(Biomass),
            Taxa.MaxN=mean(MaxN))

MaxN_all_taxa$Year <- factor(MaxN_all_taxa$Year, levels = c("2014","2022"))

#For the plot - take out zero observations
MaxN_all_taxa<-MaxN_all_taxa%>% 
  subset(Common.name!="None")

#Look mean MaxN for each taxa for both 2014 and 2022

All_MaxN<-summarySE(MaxN_all_taxa, measurevar="Taxa.MaxN", groupvars=c("Binomial","Year"))
All_MaxN.Plot1<-ggplot(All_MaxN, aes(x=factor(Binomial), y=Taxa.MaxN, fill=Year))+
  geom_col(position=position_dodge(0.9),color="grey")+
  scale_fill_manual(values= wes_palette("Darjeeling2", n = 3))+
  labs(y = "Mean MaxN", x="Fish taxa", title="Mean MaxN of different fish taxa across surveys")+
  geom_errorbar(aes(ymin=Taxa.MaxN-se, ymax=Taxa.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
All_MaxN.Plot1


####Common name
#Look mean MaxN for each taxa for both 2014 and 2022

All_MaxN<-summarySE(MaxN_all_taxa, measurevar="Taxa.MaxN", groupvars=c("Common.name","Year"))
All_MaxN.Plot1<-ggplot(All_MaxN, aes(x=factor(Common.name), y=Taxa.MaxN, fill=Year))+
  geom_col(position=position_dodge(0.9),color="grey")+
  scale_fill_manual(values= wes_palette("Darjeeling2", n = 3))+
  labs(y = "Mean MaxN", x="Fish taxa", title="Mean MaxN of different fish taxa across surveys")+
  geom_errorbar(aes(ymin=Taxa.MaxN-se, ymax=Taxa.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3,face="italic"),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
All_MaxN.Plot1







