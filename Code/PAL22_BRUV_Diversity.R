#Load BRUV MaxN and Biomass data
BRUV_all <- read.csv(file.choose()) 
BRUV_all %>% head(5)

#Aggregate to string level
BRUV_taxa<-BRUV_all %>% 
  dplyr::group_by(String, Zone, Site, Binomial) %>%
  summarize(Taxa.Biomass=mean(Biomass),
            Taxa.MaxN=mean(MaxN))

names(BRUV_taxa)

#Change format to wide
BRUV.wide<-cast(BRUV_taxa, String + Zone + Site  ~ Binomial, value='Taxa.MaxN', FUN=sum)

# replace NA values with 0's because surveys were done and some fish families not seen
BRUV.wide[is.na(BRUV.wide)]<-0

names(BRUV.wide)

#Create a species data frame - with species as columns with MaxN
BRUV_species<-BRUV.wide[,-c(1:3)]
#Create meta data for the species data
BRUV_meta<-BRUV.wide[c(1:3)]

#Number of species
BRUV_sp <- specnumber(BRUV.wide)
boxplot(BRUV_sp~BRUV_meta$Zone)

#Make a dataframe with BRUV_sp to plot using ggplot
BRUV_sp1<-enframe(BRUV_sp, name = "name", value = "Diversity")

#Plot the number of species across zones
Div.Plot1<-ggplot(BRUV_sp1, aes(x=factor(BRUV_meta$Zone), y=Diversity, fill=BRUV_meta$Zone))+
  geom_boxplot(position=position_dodge(0.9),color="black", show.legend=FALSE)+
  labs(y = "Mean number of taxa", x="Zone", title="Mean number of fish taxa across management zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),axis.title.y=element_text(size=12),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Div.Plot1

#Plot the number of species across sites and zones
Div.Plot2<-ggplot(BRUV_sp1, aes(x=factor(BRUV_meta$Site), y=Diversity, fill=BRUV_meta$Zone))+
  geom_boxplot(position=position_dodge(0.9),color="black", show.legend=FALSE)+
  labs(y = "Mean number of taxa", x="Zone", title="Mean number of fish taxa across management zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),axis.title.y=element_text(size=12),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Div.Plot2


BRUV_Shannon <- diversity(BRUV_species, index = 'shannon')
#Make a dataframe with BRUV_sp to plot using ggplot
BRUV_sh<-enframe(BRUV_Shannon, name = "Name", value = "Diversity")

#Test for significance across zones
BRUV_aov <- aov(BRUV_Shannon ~ Zone, data = BRUV_meta)
summary(BRUV_aov)
TukeyHSD(BRUV_aov)

####DFZ west as sig higher diversity compared to North and South.

#Plot the Shannon diversity index across zones

Div.Plot3<-ggplot(BRUV_sh, aes(x=factor(BRUV_meta$Zone), y=Diversity, fill=BRUV_meta$Zone))+
  geom_boxplot(position=position_dodge(0.9),color="black", show.legend=FALSE)+
  labs(y = "Shannon diversity index", x="Zone", title="Shannon diversity index  across management zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),axis.title.y=element_text(size=12),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Div.Plot3

#Plot the Shannon diversity index across Sites and Zones

Div.Plot4<-ggplot(BRUV_sh, aes(x=factor(BRUV_meta$Site), y=Diversity, fill=BRUV_meta$Zone))+
  geom_boxplot(position=position_dodge(0.9),color="black", show.legend=FALSE)+
  labs(y = "Shannon diversity index", x="Zone", title="Shannon diversity index across management zones")+
  scale_fill_manual(values= wes_palette("FantasticFox1", n = 3))+
  theme(axis.line = element_line(color='grey'),
        legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),axis.title.y=element_text(size=12),
        axis.text.y= element_text(size=11),panel.grid.minor = element_blank())
Div.Plot4
