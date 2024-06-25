#Load Trophic Level data
TL<- read.csv(file.choose()) 
TL %>% head(5)

#Aggregate to string level
TL_String<-TL %>% 
  dplyr::group_by(String, Zone, Site, Binomial, TR) %>%
  summarize(Biomass=mean(Biomass),
            MaxN=mean(MaxN))
TL_String %>% head(5)


as_factor(TL$TR)

TL_1<-ggplot(TL_String, aes(x=Biomass, y=TR))+
  geom_jitter(color="darkblue")+
  labs(y = "Trophic Level Score", x="Biomass", title="Biomass for different trophic levels")+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TL_1

TL_2<-ggplot(TL_String, aes(x=MaxN, y=TR))+
  geom_jitter(color="darkblue")+
  labs(y = "Trophic Level Score", x="Relative abundance", title="Relative abundances for different trophic levels")+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TL_2


TL_3<-ggplot(TL_String, aes(x=MaxN, y=TR, colour=Zone))+
  geom_jitter()+
  labs(y = "Trophic Level Score", x="Relative abundance", title="Relative abundances for different trophic levels")+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TL_3

TL_4<-ggplot(TL_String, aes(x=Biomass, y=TR, colour=Zone))+
  geom_jitter()+
  labs(y = "Trophic Level Score", x="Biomass", title="Biomass for different trophic levels")+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
TL_4


#Load plankton data

Plankton_all <- read.csv(file.choose()) 
Plankton_all %>% head(5)

#Aggregate to site level
Plankton_Site<-Plankton_all %>% 
  dplyr::group_by(Site, Region, Type) %>%
  summarize(Total_Count=mean(Total_Count))

#Merge Plankton and Fish data sets - using Site as the common factor

#This merges two data frames with a common ID - here we used tag.ID
Fish_Plankton <- merge(TL,Plankton_all,by = "Site")
Fish_Plankton %>% head(5)

names(Fish_Plankton)


TL_5<-ggplot(Fish_Plankton, aes(x=MaxN, y=Total_Count, colour=TR))+
  geom_point()+
  labs(y = "Total Plankton Count", x="Relative abundance of pelagic fish")+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_text(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())

TL_5

