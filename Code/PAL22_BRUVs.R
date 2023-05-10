require(Rmisc)
library(readxl)
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)
library(tidyverse) # this includes ggplot necessary for plotting
library(EnvStats) # for adding sample size
library(vctrs)


#PAL22 BRUV data with MaxN and biomass across space
#Upload the base data


BRUV_2022<-read.csv(file.choose(),header=T,sep=",")
names(BRUV_2022)
glimpse(BRUV_2022)

#Summarise data to see what is there
BRUV_2022 %>% group_by(Site) %>% 
  summarise(mean = mean(MaxN))

#Start by looking at the data across space - both biomass and MaxN
#At the moment the data are for each species observation
#Need to aggregate it to the sample level - which is the string

#Aggregate data to String level
BRUV_String<-BRUV_2022 %>%
  dplyr::group_by(String,Site,Taxa,Binomial,Common.name) %>% 
  summarize(String_biomass=mean(Biomass),
            String_MaxN=mean(MaxN))

names(BRUV_String)
#BRUV_String gives total MaxN and total biomass for each species for the entire string (added all rig data for each string)

#Just look at MaxN across sites
ggplot(BRUV_2022, aes(x=String,
                        y=MaxN, color=Site)) + geom_jitter()+
                        theme(axis.text.x=element_text(angle=90,vjust=0.3))+
                        scale_fill_brewer(palette="Dark2")

#Just look at biomass across sites
ggplot(BRUV_2022, aes(x=String,
                        y=Biomass, fill=Site)) + geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,vjust=0.3))+
  scale_fill_brewer(palette="Dark2")

#For biomass, the tigershark biomass is massive - remove

No_tiger<-BRUV_2022%>% 
  subset(Common.name!="tiger shark")

#Plot biomass again - without tiger shark (No_tiger)
ggplot(No_tiger, aes(x=String,
                        y=Biomass, fill=Site)) + geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,vjust=0.3))+
  scale_fill_brewer(palette="Dark2")

#Biomass per string is too variable - graph is not helpful

#Move on to graph data with a limit
#MaxN limit - counts of 60 and less (to remove super large schools of scad)
BRUV_2022$MaxN<-ifelse(BRUV_2022$MaxN>60,60,BRUV_2022$MaxN)


#Plot capped (at 60) average MaxN across sites (north, south and west)
ggplot(BRUV_2022, aes(x=String,
      y=MaxN, fill=Site)) + geom_boxplot()+
      theme(axis.text.x = element_blank()) +
      scale_fill_brewer(palette="Dark2")+
      ylab("Mean MaxN")

#Get means and SE to make a better graph
BRUV1<-summarySE(BRUV_2022, measurevar="MaxN", groupvars=c("String","Site"))

BRUV1<-ggplot(BRUV1, aes(x=factor(String), y=MaxN, fill=Site))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average MaxN", x="String")+
  scale_fill_brewer(palette="Dark2")+
  geom_errorbar(aes(ymin=MaxN-se, ymax=MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(angle=90,vjust=0.3),axis.title.x=element_text(),
        axis.text.y= element_text(size=11))
    
BRUV1


###################STOPPED HERE ON 4/27/2023

#Look at MaxN/string across zones

Fish3<-summarySE(d.tr.sp, measurevar="t.MaxN", groupvars=c("Site"))
Fish3<-ggplot(Fish3, aes(x=factor(Site), y=t.MaxN))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average MaxN per string")+
  geom_errorbar(aes(ymin=t.MaxN-se, ymax=t.MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
Fish3

#Look at biomass/string across zones
require(ggplot2)
Fish4<-summarySE(d.tr.sp, measurevar="t.biomass", groupvars=c("Site"))
Fish4<-ggplot(Fish4, aes(x=factor(Site), y=t.biomass))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "Average biomass per string")+
  geom_errorbar(aes(ymin=t.biomass-se, ymax=t.biomass+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))+
  geom_text(aes(y = label_y, label = Count), stat = "count", vjust = 1.5, colour = "white")
Fish4

plot(d.tr.sp$t.biomass~d.tr.sp$t.MaxN)

BRUV_2014<-read.csv(file.choose(),header=T,sep=",")

names(BRUV_2014)

#Aggregate data to String level
d.tr.sp<-BRUV_2022 %>%
  group_by(String,Family, Binomial, MaxN,Biomass) %>%
  summarize(t.biomass=sum(Biomass),
            t.MaxN=sum(MaxN))

#Make basic figures
#MaxN

require(ggplot2)
Fish1<-summarySE(BRUV_2014, measurevar="MaxN", groupvars=c("Family"))
Fish1<-ggplot(Fish1, aes(x=factor(Family), y=MaxN))+
  geom_col(position=position_dodge(0.9))+
  labs(y = "MaxN")+
  geom_errorbar(aes(ymin=MaxN-se, ymax=MaxN+se),position=position_dodge(0.9), width=0.4)+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11, angle=90),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
Fish1


#Mixed Effects model
library(lme4)
require(lmerTest)

#Model with effect of interaction protection, year and habitat - for data with snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish_all1<-lmer(Total_Biomass~Protection*Year*Habitat+(1|Station),data=Ngerumekaol_Fish_All)
drop1(fish_all1, test="Chi")

fish_all2<-lmer(Abundance_Total~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all2, test="Chi")

fish_all3<-lmer(Abundance_Total~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all3, test="Chi")

AIC(fish_all1,fish_all2,fish_all3)
library(effects)
plot(allEffects(fish_all1))
plot(allEffects(fish_all2))
plot(allEffects(fish_all3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish_all1, pairwise ~ Year*Protection*Habitat)

fit3.res = resid(fish_all2)
plot(fit3.res)
abline(0, 0)

#Check collinearity of parameters
require(performance)
check_collinearity(fish_all2)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish_all2)
plot(x)

#Model with effect of interaction protection, year and habitat - for data without snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish1<-lmer(Total_Sans~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish1, test="Chi")

fish2<-lmer(Total_Sans~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish2, test="Chi")

fish3<-lmer(Total_Sans~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish3, test="Chi")

AIC(fish1,fish2,fish3)
library(effects)
plot(allEffects(fish1))
plot(allEffects(fish2))
plot(allEffects(fish3))

#check normality of residuals
fit3.res = resid(fish1)
plot(fit3.res)
abline(0, 0)    

Plot.fit3.Linearity<-plot(resid(fish1))
plot(fish1)
qqmath(fit3)

#Check collinearity of parameters
require(performance)
check_collinearity(fish1)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish1)
plot(x)


#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish1, pairwise ~ Year*Protection*Habitat)


##############BIOMASS##############
#Make basic figures
#All fish data
require(Rmisc)
require(ggplot2)
Fish_BIOMASS<-summarySE(Ngerumekaol_Fish_All, measurevar="Grams_square_meter", groupvars=c("Year", "Habitat", "Protection"))
FishBIO<-ggplot(Fish_BIOMASS, aes(x=factor(Year), y=Grams_square_meter,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=Grams_square_meter-se,max=Grams_square_meter+se),position=position_dodge(0.9), width=0.4)+
  labs(y = "Fish biomass (Grams/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
FishBIO

#Figure wihtout snapper biomass
Fish_BIOMASS_SANS<-summarySE(Ngerumekaol_Fish_All, measurevar="Biomass_Sans_g_m2", groupvars=c("Year", "Habitat", "Protection"))
FishBIO_SANS<-ggplot(Fish_BIOMASS_SANS, aes(x=factor(Year), y=Biomass_Sans_g_m2,fill=factor(Protection)))+
  facet_grid(Habitat~.)+
  geom_col(position=position_dodge(0.9))+
  geom_errorbar(aes(ymin=Biomass_Sans_g_m2-se,max=Biomass_Sans_g_m2+se),position=position_dodge(0.9), width=0.4)+
  labs(y = "Fish biomass without Lutjanus gibbus (Grams/m2)")+
  scale_fill_manual(values=c("darkorange3","steelblue4"))+
  theme(legend.title = element_blank(),panel.background = element_blank(),panel.grid.major=element_line(0.5, colour="Gray80"),
        axis.text.x = element_text(size=11),axis.title.x=element_blank(),
        axis.text.y= element_text(size=11))
FishBIO_SANS


#Mixed Effects model for biomass
library(lme4)
require(lmerTest)

#Model with effect of interaction protection, year and habitat - for data with snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
fish_all1<-lmer(Total_Biomass~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all1, test="Chi")

fish_all2<-lmer(Total_Biomass~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all2, test="Chi")

fish_all3<-lmer(Total_Biomass~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish_all3, test="Chi")

AIC(fish_all1,fish_all2,fish_all3)
library(effects)
plot(allEffects(fish_all1))
plot(allEffects(fish_all2))
plot(allEffects(fish_all3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(fish_all1, pairwise ~ Year*Protection*Habitat)

#Check your model
#Plot residuals

fit3.res = resid(fish_all1)
plot(fit3.res)
abline(0, 0)

#Check collinearity of parameters
require(performance)
check_collinearity(fish_all1)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(fish_all1)
plot(x)


#Model with effect of interaction protection, year and habitat - for data without snapper
Ngerumekaol_Fish_All<-transform(Ngerumekaol_Fish_All,Year=as.factor(Year))
Biomass_Sans1<-lmer(Biomass_Sans~Protection*Year*Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish1, test="Chi")

Biomass_Sans2<-lmer(Biomass_Sans~Protection+Year+Habitat+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish2, test="Chi")

Biomass_Sans3<-lmer(Biomass_Sans~Protection+Year+(1|Station), data=Ngerumekaol_Fish_All)
drop1(fish3, test="Chi")

AIC(Biomass_Sans1,Biomass_Sans2,Biomass_Sans3)
library(effects)
plot(allEffects(Biomass_Sans1))
plot(allEffects(Biomass_Sans2))
plot(allEffects(Biomass_Sans3))

#Then do pairwise comparisons to seen which years and status were significant 
library(emmeans)
lsmeans(Biomass_Sans1.res, pairwise ~ Year*Protection*Habitat)

#check normality of residuals
Biomass_Sans1.res = resid(Biomass_Sans1)
plot(Biomass_Sans.res)
abline(0, 0)    

#Check collinearity of parameters
require(performance)
check_collinearity(Biomass_Sans1.res)
#VIF less than 5 indicates a low correlation of that predictor with other predictors

#Plot
require(see)
x<-check_collinearity(Biomass_Sans1.res)
plot(x)

##########################################DIVERSITY###############################################

#Fish diversity_All fish species
require(vegan)
Ngerumekaol_Fish_diversity<-read.csv(file.choose(),header=T, row.names = 1,sep=",")
Diversity_Fish_Sites<-read.csv(file.choose(),header=T, row.names = 1,sep=",")

Fish_sr <- specnumber(Ngerumekaol_Fish_diversity)
boxplot(Fish_sr~Diversity_Fish_Sites$Protection)
Fish_shannon <- diversity(Ngerumekaol_Fish_diversity, index = 'shannon')
par(mfrow=c(1,2))
boxplot(Fish_shannon~Diversity_Fish_Sites$Protection, xlab="Protection status", ylab="Shannon Wiener Diversity Index")
boxplot(Fish_shannon~Diversity_Fish_Sites$Habitat, xlab="Habitat", ylab="Shannon Wiener Diversity Index")
