
# R Script to Combine Plankton and BRUV Data
# This script merges the two datasets using the BRUV String as the linking variable

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(ggplot2)    # For visualization

# Set working directory to where your files are located
# setwd("/path/to/your/files")  # Uncomment and modify this line as needed

# Read the data files
plankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Data_with_BRUV.csv")
bruv_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PNMS_BRUV_2022_ouliers removed.csv")

# Display information about the datasets
cat("BRUV data dimensions:", dim(bruv_data), "\n")
cat("Plankton data dimensions:", dim(plankton_data), "\n")

# Check for missing values in the key joining columns
cat("\nMissing values in BRUV 'String' column:", sum(is.na(bruv_data$String)), "\n")
cat("Missing values in Plankton 'BRUVString' column:", sum(is.na(plankton_data$BRUVString)), "\n")

# Display sample of both datasets
cat("\nSample of BRUV data:\n")
print(head(bruv_data))

cat("\nSample of Plankton data:\n")
print(head(plankton_data))

# Method 1: Join datasets by BRUVString/String
# First, rename the column in BRUV data to match the Plankton data
bruv_data_renamed <- bruv_data %>%
  rename(BRUVString = String)

# Perform a left join to keep all Plankton data and match BRUV data where possible
combined_data_method1 <- plankton_data %>%
  left_join(bruv_data_renamed, by = "BRUVString",relationship = "many-to-many")

# Check for any rows with no match
cat("\nNumber of plankton records with no matching BRUV data:", 
    sum(is.na(combined_data_method1$Sample.y)), "\n")

# Method 2: Summarize Plankton data by BRUVString before joining
# This creates aggregate counts per plankton category for each BRUV string
# Modified Method 2 to include both Family and species information
combined_data_method2_with_species <- plankton_summary %>%
  left_join(bruv_data_renamed, by = "BRUVString", relationship = "many-to-many")

# Check a sample with both taxonomic levels
print(combined_data_method2_with_species %>% 
        select(BRUVString, Site.x, Specimen_Type, Family, Binomial, `Common name`, MaxN) %>% 
        head(10))

# Method 3: Combine at the site level
# This is useful if you want to analyze relationships between plankton and fish at the site level
site_plankton <- plankton_data %>%
  group_by(Site, Zone) %>%
  summarize(
    Total_Plankton_Count = sum(Total_Count, na.rm = TRUE),
    Unique_Plankton_Types = n_distinct(Spec_Morph, na.rm = TRUE),
    .groups = 'drop'
  )

site_bruv <- bruv_data %>%
  group_by(Site, Zone) %>%
  summarize(
    Total_Fish_MaxN = sum(MaxN, na.rm = TRUE),
    Total_Fish_Biomass_kg = sum(Biomass_kg, na.rm = TRUE),
    Fish_Species_Richness = n_distinct(Binomial, na.rm = TRUE),
    .groups = 'drop'
  )


# Join the site-level summaries
combined_site_data <- site_plankton %>%
  left_join(site_bruv, by = c("Site", "Zone"))

# Save the merged datasets
write_csv(combined_data_method1, "Combined_Plankton_BRUV_All_Records.csv")
write_csv(combined_data_method2, "Combined_Plankton_BRUV_Summarized.csv")
write_csv(combined_site_data, "Combined_Plankton_BRUV_Site_Level.csv")

# After each join, let's examine the results more carefully

# After Method 1 (row-by-row join)
cat("\n--- Method 1 Diagnostics (All Records) ---\n")
cat("Dimensions of combined dataset:", dim(combined_data_method1), "\n")
cat("Number of unique BRUVStrings in combined dataset:", 
    length(unique(combined_data_method1$BRUVString)), "\n")
cat("Number of unique BRUVStrings in plankton data:", 
    length(unique(plankton_data$BRUVString)), "\n")
cat("Number of unique BRUVStrings in BRUV data:", 
    length(unique(bruv_data_renamed$BRUVString)), "\n")

# Check for specific examples
cat("\nSample data for one BRUVString (e.g., PAL22_1):\n")
sample_string <- "PAL22_1"
cat("Number of plankton records for", sample_string, ":", 
    sum(plankton_data$BRUVString == sample_string), "\n")
cat("Number of BRUV records for", sample_string, ":", 
    sum(bruv_data_renamed$BRUVString == sample_string), "\n")
cat("Number of combined records for", sample_string, ":", 
    sum(combined_data_method1$BRUVString == sample_string), "\n")

# Check a few columns to ensure they're correctly joined
cat("\nSample of combined dataset (first 5 rows):\n")
print(combined_data_method1 %>% 
        select(BRUVString, Sample.x, Sample.y, Specimen_Type, Family, MaxN) %>% 
        head(5))

# Similar checks for Method 2
cat("\n--- Method 2 Diagnostics (Summarized Plankton) ---\n")
cat("Dimensions of combined dataset:", dim(combined_data_method2), "\n")
cat("Sample of combined dataset (first 5 rows):\n")
print(combined_data_method2 %>% 
        select(BRUVString, Site.x, Site.y, Specimen_Type, Family, MaxN) %>% 
        head(5))

# And Method 3
cat("\n--- Method 3 Diagnostics (Site Level) ---\n")
cat("Dimensions of combined dataset:", dim(combined_site_data), "\n")
cat("Sample of combined dataset (all rows):\n")
print(combined_site_data)

# Check for any sites in plankton data that don't match BRUV data
cat("\nSites in plankton data not found in BRUV data:\n")
setdiff(unique(site_plankton$Site), unique(site_bruv$Site))

# Check for any sites in BRUV data that don't match plankton data
cat("\nSites in BRUV data not found in plankton data:\n")
setdiff(unique(site_bruv$Site), unique(site_plankton$Site))
