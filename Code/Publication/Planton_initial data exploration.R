# PNMS Zooplankton Analysis - 2022 Dataset
# ---------------------------------------------

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices
library(readxl)    # For reading Excel files
library(viridis)   # For color palettes

# Avoid scientific notation
options(scipen = 999)

# Set a consistent color palette similar to BRUV analysis
zone_colors <- c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")

# ---------------------------------------------
# Step 1: Data Loading and Initial Exploration
# ---------------------------------------------

# Load the zooplankton data
# Load the zooplankton data
zooplankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Data_with_BRUV.csv")

# Basic structure and summary
str(zooplankton_data)
summary(zooplankton_data)

# View the first few rows
head(zooplankton_data)

# Check dimensions
dim(zooplankton_data)

# Check for missing values in main columns
colSums(is.na(zooplankton_data[,1:8]))

# Look at unique values in categorical columns
unique_values <- list(
  Sample = length(unique(zooplankton_data$Sample)),
  Type = length(unique(zooplankton_data$Type)),
  Zone = length(unique(zooplankton_data$Zone)),
  Site = length(unique(zooplankton_data$Site)),
  Plankton_Category = length(unique(zooplankton_data$Plankton_Category)),
  Specimen_Type = length(unique(zooplankton_data$Specimen_Type)),
  Plankton_Group = length(unique(zooplankton_data$Plankton_Group))
)
print(unique_values)

# Display unique values for key categorical variables
cat("\nUnique Zones:\n")
unique(zooplankton_data$Zone)

cat("\nUnique Sites:\n")
unique(zooplankton_data$Site)

cat("\nUnique Plankton Categories:\n")
unique(zooplankton_data$Plankton_Category)

cat("\nUnique Plankton Groups:\n")
unique(zooplankton_data$Plankton_Group)

cat("\nUnique Specimen Types:\n")
unique(zooplankton_data$Specimen_Type)

# Set factor levels to ensure consistent zone ordering
zooplankton_data$Zone <- factor(zooplankton_data$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

