# PNMS BRUV Analysis - Raw data
# -------------------------------------------

# Load required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(vegan)     # For diversity indices

#Avoid scientific annotations
options(scipen = 999)

#Load data - loaded data where the two outliers were removed!!!!
bruv_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PNMS_BRUV_2022_ouliers removed.csv", stringsAsFactors = FALSE)

# Set a consistent color palette
zone_colors <- c("PNMS North" = "#0088FE", "DFZ West" = "#00C49F", "PNMS South" = "#FFBB28")

# Set factor levels to ensure consistent zone ordering
bruv_data$Zone <- factor(bruv_data$Zone, levels = c("PNMS North", "DFZ West", "PNMS South"))

# Load required packages
library(tidyverse)
library(ggplot2)
library(vegan)  # For diversity indices

# ================================================================
# Data structure and general checks
# ================================================================

# Basic structure and summary
str(bruv_data)
summary(bruv_data)

# View the first few rows
head(bruv_data)

# Check dimensions
dim(bruv_data)

# Check for missing values
colSums(is.na(bruv_data))

# Look at unique values in categorical columns
unique_values <- list(
  Sample = length(unique(bruv_data$Sample)),
  String = length(unique(bruv_data$String)),
  Site = length(unique(bruv_data$Site)),
  Zone = length(unique(bruv_data$Zone)),
  Type = length(unique(bruv_data$Type)),
  Family = length(unique(bruv_data$Family)),
  Binomial = length(unique(bruv_data$Binomial))
)
print(unique_values)

# Display unique values for key categorical variables
cat("\nUnique Sites:\n")
unique(bruv_data$Site)

cat("\nUnique Zones:\n")
unique(bruv_data$Zone)

cat("\nUnique Families:\n")
unique(bruv_data$Family)

# Understanding the sampling design
# Number of strings per site
strings_per_site <- bruv_data %>%
  group_by(Site) %>%
  summarize(
    Zone = first(Zone),
    UniqueSamples = n_distinct(Sample),
    UniqueStrings = n_distinct(String),
    TotalObservations = n()
  )
print(strings_per_site)

# Check the relationship between Sample and String
sample_string_relation <- bruv_data %>%
  select(Sample, String) %>%
  distinct() %>%
  group_by(String) %>%
  summarize(NumSamples = n())

print(sample_string_relation)

# Check how many samples per string
samples_per_string <- bruv_data %>%
  select(Sample, String) %>%
  distinct() %>%
  group_by(String) %>%
  summarize(NumSamples = n()) %>%
  group_by(NumSamples) %>%
  summarize(Count = n())

print(samples_per_string)

# ================================================================
# Check for samples with zero fish observations
# ================================================================

# First, identify all unique samples in the dataset
all_samples <- bruv_data %>%
  select(Sample, String, Site, Zone) %>%
  distinct() %>%
  arrange(Zone, Site, String, Sample)

# Count number of species observations per sample
species_per_sample <- bruv_data %>%
  group_by(Sample, String, Site, Zone) %>%
  summarize(
    NumSpeciesObserved = n_distinct(Binomial[Binomial != "None" & !is.na(Binomial)]),
    TotalMaxN = sum(MaxN, na.rm = TRUE),
    HasFish = NumSpeciesObserved > 0 | TotalMaxN > 0,
    .groups = 'drop'
  ) %>%
  arrange(Zone, Site, String, Sample)

# Identify samples with zero fish
samples_with_zero_fish <- species_per_sample %>%
  filter(!HasFish)

# Summarize results
cat("=== SAMPLES WITH ZERO FISH OBSERVATIONS ===\n")
cat("Total number of samples:", nrow(all_samples), "\n")
cat("Number of samples with zero fish:", nrow(samples_with_zero_fish), "\n")
cat("Percentage of samples with zero fish:", 
    round(nrow(samples_with_zero_fish) / nrow(all_samples) * 100, 2), "%\n\n")

if (nrow(samples_with_zero_fish) > 0) {
  cat("List of samples with zero fish observations:\n")
  print(samples_with_zero_fish)
} else {
  cat("No samples with zero fish were found.\n")
}

# Visualize the hierarchical structure

# Create a summary data frame for plotting
hierarchy_data <- bruv_data %>%
  select(Zone, Site, String, Sample) %>%
  distinct() %>%
  group_by(Zone, Site, String) %>%
  summarize(NumSamples = n(), .groups = 'drop')

# Plot the structure
ggplot(hierarchy_data, aes(x = Site, y = NumSamples, fill = Zone)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Samples per Site",
       x = "Site", y = "Number of Samples") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ================================================================
# Check for strings with zero fish observations
# ================================================================

# First, identify all unique strings in the dataset
all_strings <- bruv_data %>%
  select(String, Site, Zone) %>%
  distinct() %>%
  arrange(Zone, Site, String)

# Aggregate at the string level
# For each species within each string, take the maximum MaxN
string_level_data <- bruv_data %>%
  # Group by string and species
  group_by(String, Site, Zone, Binomial) %>%
  summarize(
    MaxN = max(MaxN, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Filter out "None" species
  filter(Binomial != "None" & !is.na(Binomial))

# Count fish observations per string
fish_per_string <- string_level_data %>%
  group_by(String, Site, Zone) %>%
  summarize(
    NumSpeciesObserved = n_distinct(Binomial),
    TotalMaxN = sum(MaxN, na.rm = TRUE),
    HasFish = NumSpeciesObserved > 0 | TotalMaxN > 0,
    .groups = 'drop'
  ) %>%
  arrange(Zone, Site, String)

# Find strings that may not appear in string_level_data (zero observations)
strings_with_zero_fish <- all_strings %>%
  anti_join(fish_per_string, by = "String") %>%
  mutate(
    NumSpeciesObserved = 0,
    TotalMaxN = 0,
    HasFish = FALSE
  )

# Combine with fish_per_string for complete dataset
complete_string_data <- bind_rows(fish_per_string, strings_with_zero_fish) %>%
  arrange(Zone, Site, String)

# Identify strings with zero fish
zero_fish_strings <- complete_string_data %>%
  filter(!HasFish)

# Summarize results
cat("\n=== STRINGS WITH ZERO FISH OBSERVATIONS ===\n")
cat("Total number of strings:", nrow(all_strings), "\n")
cat("Number of strings with zero fish:", nrow(zero_fish_strings), "\n")
cat("Percentage of strings with zero fish:", 
    round(nrow(zero_fish_strings) / nrow(all_strings) * 100, 2), "%\n\n")

if (nrow(zero_fish_strings) > 0) {
  cat("List of strings with zero fish observations:\n")
  print(zero_fish_strings)
} else {
  cat("No strings with zero fish were found.\n")
}
