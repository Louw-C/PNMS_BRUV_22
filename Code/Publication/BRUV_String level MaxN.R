# PNMS BRUV - data processing
#MaxN determined for each rig - but we want MaxN per string. 
#Need to process the data to have MaxN for each group per string - not rig
#Using BRUV data with outlier removed - tiger shark and mola
# -------------------------------------------

# R code to calculate MaxN at the string level rather than rig level
# This addresses potential double-counting across rigs in the same string

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Read the data
# Replace with actual file path if different
bruv_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PNMS_BRUV_2022_ouliers removed.csv")

# Check the data structure
str(bruv_data)

# Function to calculate string-level MaxN
calculate_string_maxn <- function(data) {
  # Group by String and Species (Binomial), then find the maximum MaxN value
  string_maxn <- data %>%
    group_by(String, Family, Binomial, `Common name`, Type, Zone, Site, Year) %>%
    summarize(
      MaxN_string = max(MaxN), # Take the maximum MaxN across all rigs in the string
      Total_MaxN_sum = sum(MaxN), # Sum of MaxN across all rigs (for comparison)
      N_rigs_observed = n_distinct(Sample), # Number of rigs where the species was observed
      Mean_biomass_g = mean(Biomass_g),
      Total_biomass_g = sum(Biomass_g),
      Mean_biomass_kg = mean(Biomass_kg),
      Total_biomass_kg = sum(Biomass_kg),
      Samples = paste(Sample, collapse = ", "), # List of samples where species was observed
      .groups = "drop"
    )
  
  return(string_maxn)
}

# Apply function to calculate string-level MaxN
string_level_maxn <- calculate_string_maxn(bruv_data)

# Summary statistics for original rig-level vs new string-level MaxN
summary_stats <- data.frame(
  Original_total_MaxN = sum(bruv_data$MaxN),
  New_total_MaxN = sum(string_level_maxn$MaxN_string),
  Original_records = nrow(bruv_data),
  New_records = nrow(string_level_maxn),
  MaxN_reduction_percent = (1 - sum(string_level_maxn$MaxN_string) / sum(bruv_data$MaxN)) * 100
)

print("Comparison of original rig-level vs new string-level MaxN:")
print(summary_stats)

# Save the results
write_csv(string_level_maxn, "PNMS_BRUV_2022_string_level_MaxN.csv")

# Create summary by String
string_summary <- string_level_maxn %>%
  group_by(String) %>%
  summarize(
    N_species = n_distinct(Binomial),
    N_families = n_distinct(Family),
    Total_MaxN = sum(MaxN_string),
    Total_biomass_kg = sum(Total_biomass_kg),
    .groups = "drop"
  )

# Visualization: Compare MaxN sum vs MaxN max by String
comparison_plot <- bruv_data %>%
  group_by(String, Binomial) %>%
  summarize(
    MaxN_sum = sum(MaxN),
    MaxN_max = max(MaxN),
    .groups = "drop"
  ) %>%
  group_by(String) %>%
  summarize(
    Total_MaxN_sum = sum(MaxN_sum),
    Total_MaxN_max = sum(MaxN_max),
    .groups = "drop"
  )

# Plot to visualize difference between summing MaxN and taking the max MaxN
ggplot(comparison_plot, aes(x = reorder(String, Total_MaxN_sum))) +
  geom_bar(aes(y = Total_MaxN_sum, fill = "Sum of all rigs' MaxN"), stat = "identity") +
  geom_bar(aes(y = Total_MaxN_max, fill = "New string-level MaxN"), stat = "identity", alpha = 0.7) +
  labs(
    title = "Comparison of MaxN Calculation Methods by String",
    x = "String", 
    y = "MaxN Value",
    fill = "Calculation Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("Sum of all rigs' MaxN" = "red", "New string-level MaxN" = "blue"))

# Example detailed analysis for a specific string (e.g., String 1)
string1_detail <- string_level_maxn %>%
  filter(String == "PAL22_1") %>%
  select(Binomial, `Common name`, Family, MaxN_string, Total_MaxN_sum, N_rigs_observed, 
         Total_biomass_kg, Samples) %>%
  arrange(desc(MaxN_string))

print("Detailed analysis for String 1 (PAL22_1):")
print(string1_detail)

# Assess impact on biodiversity metrics
biodiversity_comparison <- data.frame(
  Metric = c("Total Species", "Total MaxN", "Species per String (mean)"),
  Original_data = c(
    n_distinct(bruv_data$Binomial),
    sum(bruv_data$MaxN),
    mean(bruv_data %>% group_by(String) %>% summarize(n_species = n_distinct(Binomial)) %>% pull(n_species))
  ),
  New_data = c(
    n_distinct(string_level_maxn$Binomial),
    sum(string_level_maxn$MaxN_string),
    mean(string_level_maxn %>% group_by(String) %>% summarize(n_species = n_distinct(Binomial)) %>% pull(n_species))
  )
)

print("Impact on biodiversity metrics:")
print(biodiversity_comparison)
