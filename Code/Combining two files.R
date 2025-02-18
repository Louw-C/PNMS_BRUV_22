#creating a data file by combining BRUV meta data with BRUV data to add coordinates.
#Use Sample Number as the common parameter

#load data formatted for publication and shared with Joel
Plankton_Publication <- read.csv(file.choose()) 
BRUV_Publication <- read.csv(file.choose())
BRUV_Meta <- read.csv(file.choose())

#Want to combine coordinate data to BRUV data - combine BRUV_Meta and BRUV_Publication

BRUV_Combined <- merge(BRUV_Publication,BRUV_Meta,by = "Sample")
BRUV_Combined  %>% head(5)

#Save file as a CSV.
write.csv(BRUV_Combined, file = "BRUV_Combined.csv")
