# R Script to Add Functional Groups to String-Level BRUV Data
# -----------------------------------------------------

# Load necessary libraries
library(tidyverse)

# 1. Load the datasets
# -------------------
# Species data with functional groups
species_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/PAL22_BRUV_Species.csv", stringsAsFactors = FALSE)

# String-level MaxN data
maxn_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/PNMS_BRUV_2022_string_level_MaxN.csv", stringsAsFactors = FALSE)

# 2. Create a lookup table for functional groups
# --------------------------------------------
# Standardize the functional group names (remove any trailing spaces)
species_data$Functional_Group <- trimws(species_data$Functional_Group)

# Create a lookup dataframe that maps species to functional groups
functional_group_lookup <- species_data %>%
  select(Binomial, Functional_Group, TR) %>%
  distinct()

# Print the lookup table to verify
print("Functional Group Lookup Table:")
print(functional_group_lookup)

# 3. Add functional groups to the MaxN data
# ---------------------------------------
# Merge the functional groups into the MaxN data
maxn_data_with_groups <- maxn_data %>%
  left_join(functional_group_lookup, by = "Binomial")

# Check for any species that didn't get a functional group assigned
missing_functional_groups <- maxn_data_with_groups %>%
  filter(is.na(Functional_Group)) %>%
  select(Binomial) %>%
  distinct()

if (nrow(missing_functional_groups) > 0) {
  warning("The following species are missing functional group assignments:")
  print(missing_functional_groups)
}

# 4. Save the updated dataset
# ------------------------
# Save the updated data with functional groups
write.csv(maxn_data_with_groups, "PNMS_BRUV_2022_string_level_MaxN_with_FuncGroups.csv", row.names = FALSE)

# 5. Create summary statistics by functional group
# ---------------------------------------------
# Summarize MaxN by functional group and zone
fg_summary_by_zone <- maxn_data_with_groups %>%
  filter(!is.na(Functional_Group)) %>%
  group_by(Zone, Functional_Group) %>%
  summarize(
    Total_MaxN = sum(MaxN_string, na.rm = TRUE),
    Percent_MaxN = round(100 * sum(MaxN_string, na.rm = TRUE) / sum(maxn_data_with_groups$MaxN_string, na.rm = TRUE), 1),
    Species_Count = n_distinct(Binomial),
    String_Count = n_distinct(String),
    Mean_TR = mean(TR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Zone, desc(Total_MaxN))

# Overall summary by functional group
fg_summary_overall <- maxn_data_with_groups %>%
  filter(!is.na(Functional_Group)) %>%
  group_by(Functional_Group) %>%
  summarize(
    Total_MaxN = sum(MaxN_string, na.rm = TRUE),
    Percent_MaxN = round(100 * sum(MaxN_string, na.rm = TRUE) / sum(maxn_data_with_groups$MaxN_string, na.rm = TRUE), 1),
    Species_Count = n_distinct(Binomial),
    String_Count = n_distinct(String),
    Mean_TR = mean(TR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_MaxN))

# Save the summary statistics
write.csv(fg_summary_by_zone, "FunctionalGroup_Summary_by_Zone.csv", row.names = FALSE)
write.csv(fg_summary_overall, "FunctionalGroup_Summary_Overall.csv", row.names = FALSE)

# 6. Optional: Create string-level functional group compositions
# -----------------------------------------------------------
# This creates a dataset showing the percentage of each functional group at each string
string_level_fg_composition <- maxn_data_with_groups %>%
  filter(!is.na(Functional_Group)) %>%
  group_by(String, Zone, Site) %>%
  mutate(String_Total_MaxN = sum(MaxN_string, na.rm = TRUE)) %>%
  group_by(String, Zone, Site, Functional_Group) %>%
  summarize(
    FG_MaxN = sum(MaxN_string, na.rm = TRUE),
    FG_Percent = round(100 * FG_MaxN / first(String_Total_MaxN), 1),
    String_Total = first(String_Total_MaxN),
    .groups = "drop"
  ) %>%
  arrange(Zone, Site, String, desc(FG_Percent))

# Save the string-level functional group composition
write.csv(string_level_fg_composition, "String_Level_FunctionalGroup_Composition.csv", row.names = FALSE)

# 7. Print completion message
# ------------------------
cat("\nAnalysis complete! The following files have been created:\n")
cat("1. PNMS_BRUV_2022_string_level_MaxN_with_FuncGroups.csv - Main dataset with functional groups added\n")
cat("2. FunctionalGroup_Summary_by_Zone.csv - Summary statistics by zone\n")
cat("3. FunctionalGroup_Summary_Overall.csv - Overall summary statistics\n")
cat("4. String_Level_FunctionalGroup_Composition.csv - Functional group composition at each string\n")