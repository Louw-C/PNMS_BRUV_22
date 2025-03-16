# PNMS BRUV Analysis with Outlier checks and removal
# -------------------------------------------
# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices

#Avoid scientific anotations
options(scipen = 999)

#Load data
bruv_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PNMS_BRUV_2022.csv", stringsAsFactors = FALSE)

# Set a consistent color palette
zone_colors <- c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")

# Set factor levels to ensure consistent zone ordering
bruv_data$Zone <- factor(bruv_data$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

# 1. Identify Outliers in Raw Data
# -------------------------------

# Function to find outliers using Tukey's rule
find_outlier_thresholds <- function(x, multiplier = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr
  return(list(lower = lower_bound, upper = upper_bound, q1 = q1, q3 = q3, iqr = iqr))
}

# Find outliers in biomass data
biomass_thresholds <- find_outlier_thresholds(bruv_data$Biomass_kg)
cat("Biomass quartiles (kg):", biomass_thresholds$q1, biomass_thresholds$q3, "\n")
cat("Biomass IQR (kg):", biomass_thresholds$iqr, "\n")
cat("Biomass outlier thresholds (kg):", biomass_thresholds$lower, biomass_thresholds$upper, "\n")

# Flag biomass outliers in the data - creates an additional column in the data to show outliers (TRUE/FALSE)
bruv_data$biomass_outlier <- bruv_data$Biomass_kg < biomass_thresholds$lower | 
  bruv_data$Biomass_kg > biomass_thresholds$upper

# Find extreme biomass outliers (3 * IQR)
biomass_extreme_thresholds <- find_outlier_thresholds(bruv_data$Biomass_kg, multiplier = 3)
cat("Extreme biomass outlier thresholds (kg):", biomass_extreme_thresholds$lower, biomass_extreme_thresholds$upper, "\n")

bruv_data$biomass_extreme_outlier <- bruv_data$Biomass_kg < biomass_extreme_thresholds$lower | 
  bruv_data$Biomass_kg > biomass_extreme_thresholds$upper

# Identify biomass outliers
biomass_outliers <- bruv_data %>% 
  filter(biomass_outlier) %>% 
  arrange(desc(Biomass_kg)) %>%
  select(Sample, String, Site, Zone, Binomial, Biomass_kg)

biomass_extreme_outliers <- bruv_data %>% 
  filter(biomass_extreme_outlier) %>% 
  arrange(desc(Biomass_kg)) %>%
  select(Sample, String, Site, Zone, Binomial, Biomass_kg)

# Print outlier summary
cat("Number of biomass outliers:", nrow(biomass_outliers), "\n")
cat("Number of extreme biomass outliers:", nrow(biomass_extreme_outliers), "\n")

print("Top biomass outliers:")
print(head(biomass_outliers, 5))

print("Extreme biomass outliers:")
print(biomass_extreme_outliers)

##-----##
#Looking at outliers for abundance/MaxN

maxN_thresholds <- find_outlier_thresholds(bruv_data$MaxN)
cat("MaxN quartiles:", maxN_thresholds$q1, maxN_thresholds$q3, "\n")
cat("MaxN IQR:", maxN_thresholds$iqr, "\n")
cat("MaxN outlier thresholds:", maxN_thresholds$lower, maxN_thresholds$upper, "\n")

# Flag MaxN outliers in the data - creates an additional column in the data to show outliers (TRUE/FALSE)
bruv_data$maxN_outlier <- bruv_data$MaxN < maxN_thresholds$lower | 
  bruv_data$MaxN > maxN_thresholds$upper

# Find extreme MaxN outliers (3 * IQR)
maxN_extreme_thresholds <- find_outlier_thresholds(bruv_data$MaxN, multiplier = 3)
cat("Extreme MaxN outlier thresholds:", maxN_extreme_thresholds$lower, maxN_extreme_thresholds$upper, "\n")

# Flag MaxN extremel outliers in the data - creates an additional column in the data to show extreme outliers (TRUE/FALSE)
bruv_data$maxN_extreme_outlier <- bruv_data$MaxN< maxN_extreme_thresholds$lower | 
  bruv_data$MaxN > maxN_extreme_thresholds$upper

# Identify outliers
maxN_outliers <- bruv_data %>% 
  filter(maxN_outlier) %>% 
  arrange(desc(MaxN)) %>%
  select(Sample, String, Site, Zone, Binomial, MaxN)

maxN_extreme_outliers <- bruv_data %>% 
  filter(maxN_extreme_outlier) %>% 
  arrange(desc(MaxN)) %>%
  select(Sample, String, Site, Zone, Binomial, MaxN)

# Print outlier summary
cat("Number of MaxN outliers:", nrow(maxN_outliers), "\n")
cat("Number of extreme MaxN outliers:", nrow(maxN_extreme_outliers), "\n")

print("Top MaxN outliers:")
print(head(maxN_outliers, 5))

print("Extreme MaxN outliers:")
print(extreme_outliers)

##So - most of the outliers identified above are ecological important
#e.g. large schools of mahi-mahi and decapterus
#e.g. large billfish
#There are also more than one observations of the above across strings
#Only the large tiger shark and the large mola mola (one observation of each only) can be considered as true outliers, significantly skewing data
#Create a datasheet with these two observations removed.

# Create a dataset with extreme outliers removed
extreme_outlier_samples <- extreme_outliers$Sample
bruv_data_no_extreme <- bruv_data %>%
  filter(!(Sample %in% extreme_outlier_samples))

cat("Original data rows:", nrow(bruv_data), "\n")
cat("Rows after removing extreme outliers:", nrow(bruv_data_no_extreme), "\n")
cat("Number of items removed:", nrow(bruv_data) - nrow(bruv_data_no_extreme), "\n")

cat("\nDetailed list of extreme outlier items removed:\n")
print(extreme_outliers %>% 
        select(Sample, String, Site, Zone, Binomial, Biomass_kg) %>%
        arrange(String, Sample))

# Summarize what was removed by species
cat("\nSummary of species removed as extreme outliers:\n")
print(extreme_outliers %>%
        group_by(Binomial) %>%
        summarize(
          Count = n(),
          Total_Biomass_kg = sum(Biomass_kg),
          Mean_Biomass_kg = mean(Biomass_kg),
          Zones = paste(unique(Zone), collapse = ", ")
        ) %>%
        arrange(desc(Total_Biomass_kg)))

# Summarize what was removed by zone
cat("\nSummary of biomass removed by zone:\n")
print(extreme_outliers %>%
        group_by(Zone) %>%
        summarize(
          Items_Removed = n(),
          Total_Biomass_Removed_kg = sum(Biomass_kg),
          Percent_of_Outlier_Biomass = Total_Biomass_Removed_kg / sum(extreme_outliers$Biomass_kg) * 100
        ) %>%
        arrange(desc(Total_Biomass_Removed_kg)))