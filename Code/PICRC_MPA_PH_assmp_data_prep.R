# install any of these libraries if you don't have them installed already
library(readxl)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(car)


PICRC_data<-read.csv("https://www.dropbox.com/s/960o15j2n0fnzsg/All%20fish%20species-PH12-4-csv.csv?dl=1")

d<-PICRC_data
d

# remove NA values for transect associated with rainbow runner reef pelagic rare sighting
d<-subset(d, Transect>0)

# remove all NA values for biomass because no data exists (not too many)
d<-subset(d, Biomass.g.>0)

# look for any obvious fish families/species to remove that are rare but huge - sharks, pelagics, etc

# quick summary of species biomass and density across all sites-habitats-etc

d.outlier<-d %>%
  group_by(Species_Code) %>%
  summarise(biomass=sum(Biomass.g.),
            density=sum(Number),
            inflated.species=(biomass/1000)/density)

# this graph will show species with high biomass contribution but rare
ggplot(subset(d.outlier, inflated.species>1), aes(x=reorder(Species_Code, -inflated.species), y=inflated.species)) +
  geom_bar(stat="identity") +
  theme_bw(base_size=22) +
  ylab("inflation kg/density") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

## conclusion made ##
# remove sharks and barricuda and katsuwo because only very few sightings, high biomass, and reef pelagic
# before removing, look at one more potential data bias we need to overcome
# Now we understand inflated species, we need to look at biomass contribution for large schools of fish
ggplot(subset(d.outlier, (biomass/1000)>5), aes(x=reorder(Species_Code, -biomass/1000), y=biomass/1000)) +
  geom_bar(stat="identity") +
  theme_bw(base_size=22) +
  ylab("biomass kg") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

# we need to deal with Lutjanus gibbus but any others?
ggplot(subset(d.outlier, (biomass/1000)>5 & Species_Code!="Lutjanus gibbus"), aes(x=reorder(Species_Code, -biomass/1000), y=biomass/1000)) +
  geom_bar(stat="identity") +
  theme_bw(base_size=22) +
  ylab("biomass kg") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

# others look good, just Lutjanus gibbus or keremlau

## ready to make all the adjustments we discovered above ##

# we revert back to our "d" dataframe to do this, as we will work from that
d<-subset(d, Family!="Carcharhinidae")
d<-subset(d, Family!="Sphyraenidae")
d<-subset(d, Family!="Scombridae")

# Now for the gibbus problem
# My plan is to set a cap on # of Keremlau observed on any transect (gibbus)
# To do this, we need to aggregate our data to the transect level
d.tr.sp<-d %>%
  group_by(Year, Station, Habitat, Protection, Transect, Family, Genus, Species_Code) %>%
  summarize(t.biomass=sum(Biomass.g.),
            t.abundance=sum(Number))

# create histogram to see large schools in terms of density
ggplot(d.tr.sp, aes(x=t.abundance, fill=Family)) +
  geom_histogram() +
  theme_bw(base_size=22)

# zoom into unusual abundances only
ggplot(subset(d.tr.sp, t.abundance>5), aes(x=t.abundance, fill=Family)) +
  geom_histogram() +
  theme_bw(base_size=22)

# large schools are all snapper and Lutjanus gibbus or keremlau

## decision made here ##
#### cut all large fish schools of keremlau to 20 max to avoid rare large schools ####

# rescale large schools to max of 20 of fish using an ifelse statement
d.tr.sp$t.abundance2<-ifelse(d.tr.sp$t.abundance>20,20,d.tr.sp$t.abundance)

# now we need to rescale, or recalculate, biomass based upon this max cut process
# create scaling factor for biomass
d.tr.sp$scaling<-d.tr.sp$t.abundance/d.tr.sp$t.abundance2
# finally, our rescaled biomass
d.tr.sp$biomass.scaled<-d.tr.sp$t.biomass/d.tr.sp$scaling

# create new histogram of our data that accouned for these large schools
ggplot(d.tr.sp, aes(x=t.abundance2, fill=Family)) +
  geom_histogram() +
  theme_bw(base_size=22)

# data looking great now, very tractable decisions and ready for some views

#### next section looking at some basic graphs of biomass ####

# quick look  at biomass across major genera, years, habitat, and year
ggplot(d.tr.sp, aes(x=Station, y=biomass.scaled/1000, fill=Genus)) +
  geom_bar(stat="identity") +
  facet_grid(Habitat+as.factor(Year)~Protection, scales="free") +
  theme_bw(base_size=22) +
  ylab("biomass (kg)")

# first quick look suggests only forereef in 2020 MPA may be working, other habitat-years not
# however, we need to take a formal, deeper dive like in our exercise with Pohnpei


#### isolate upon total fish biomass as our dependent variable for formal investigation ####
# aggregate total fish biomass to transect level
d2<-d.tr.sp %>%
  group_by(Year, Station, Habitat, Protection, Transect) %>%
  summarize(biomass=sum(biomass.scaled),
            abundance=sum(t.abundance2))


# Now I noticed that there were some transects with nothing observed, like no fist
# need to account for these and put 0 values in the data where this was observed
# The trick is to do a cast and melt to account for those, you can follow
d.cast<-dcast(d2, Year + Station + Habitat + Protection ~ Transect, value.var="biomass", FUN=sum)
# now we can see where our 0 values for transects occurred
# fill all NA with 0
d.cast[is.na(d.cast)] <-0
# solved that, now we melt our data back into original shape
d.melt<-melt(d.cast, id=c("Year","Station","Habitat","Protection"))
# finally rename the column headers in the data
names(d.melt)[5:6]<-c("Transect","biomass")

# rename our data frame for use
d3<-d.melt

# geom_box to show transect level data spread and show our hypothesis of MPA
ggplot(d3, aes(x=as.factor(Station), y=biomass/1000)) +
  geom_boxplot(aes(fill=Protection)) +
  facet_grid(Habitat + as.factor(Year)~Protection, scales="free") +
  theme_bw(base_size=20) +
  ylab("biomass (kg)")

# same graph while appreciating data distributions in a violin/jitter plot
ggplot(d3, aes(x=as.factor(Station), y=biomass/1000)) +
  geom_violin(aes(fill=Protection)) +
  geom_jitter(width=0.1) +
  facet_grid(Habitat + as.factor(Year)~Protection, scales="free") +
  theme_bw(base_size=22) +
  ylab("biomass (kg)")

#### end - we are now ready to do the stats ####