# R Script to Create a Single Combined Taxa Dataset
# =================================================
# This script creates a single dataset with:
# 1. One column for string ID
# 2. Columns for all fish species (each species gets its own column with MaxN values)
# 3. Columns for all plankton specimen types (each type gets its own column with counts)

# Load required packages
library(tidyverse)
library(stringr)
library(readr)

# ----------------------------------------
# Step 1: Load datasets
# ----------------------------------------

# Load BRUV string-level data
bruv_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/PNMS_BRUV_2022_string_level_MaxN.csv")

# Load plankton data
plankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Publication.csv")

# Define strings known to have no plankton data - these will be excluded
strings_to_exclude <- c("PAL22_19", "PAL22_22", "PAL22_25", "PAL22_28")
cat("Excluding BRUV strings with no plankton data:", paste(strings_to_exclude, collapse = ", "), "\n")

# Filter out the strings with no plankton data
bruv_data_filtered <- bruv_data %>%
  filter(!(String %in% strings_to_exclude))

cat("BRUV data rows before filtering:", nrow(bruv_data), "\n")
cat("BRUV data rows after filtering:", nrow(bruv_data_filtered), "\n")

# ----------------------------------------
# Step 2: Prepare datasets for matching
# ----------------------------------------

# Extract key components from BRUV strings
bruv_data_filtered <- bruv_data_filtered %>%
  mutate(
    StringNumber = as.numeric(str_replace(String, "PAL22_", "")),
    SiteZone = paste(Site, Zone, sep = "_")
  )

# Extract key components from plankton samples
plankton_data <- plankton_data %>%
  mutate(
    # Clean up zone names to match BRUV data
    ZoneClean = case_when(
      Zone == "North" ~ "PNMS North",
      Zone == "South" ~ "PNMS South",
      Zone == "West" ~ "DFZ West",
      TRUE ~ Zone
    ),
    SiteZone = paste(Site, ZoneClean, sep = "_")
  )

# ----------------------------------------
# Step 3: Create a mapping between datasets
# ----------------------------------------

# Create summary of BRUV strings 
bruv_summary <- bruv_data_filtered %>%
  group_by(String, StringNumber, Site, Zone, SiteZone) %>%
  summarize(
    TotalMaxN = sum(MaxN_string, na.rm = TRUE),
    TotalBiomass_kg = sum(Total_biomass_kg, na.rm = TRUE),
    SpeciesRichness = n_distinct(Binomial[Binomial != "None"]),
    .groups = "drop"
  )

# Create summary of plankton samples
plankton_summary <- plankton_data %>%
  group_by(Sample, Site, ZoneClean, SiteZone) %>%
  summarize(
    TotalPlanktonCount = sum(`Total Count`, na.rm = TRUE),
    .groups = "drop"
  )

# Create a function to match BRUV strings with plankton samples
match_string_to_sample <- function(bruv_string, bruv_site, bruv_zone, plankton_data) {
  # Extract string number
  string_num <- as.numeric(str_replace(bruv_string, "PAL22_", ""))
  
  # Find matching plankton samples based on site and zone
  matching_samples <- plankton_data %>%
    filter(
      Site == bruv_site,
      ZoneClean == bruv_zone
    )
  
  if (nrow(matching_samples) == 0) {
    return(NA)
  }
  
  # Try to find the best match
  for (sample in unique(matching_samples$Sample)) {
    # Check if the sample contains the string number
    if (str_detect(sample, paste0("_", string_num, "_"))) {
      return(sample)
    }
  }
  
  # If no clear match, return the first sample from that site/zone
  return(unique(matching_samples$Sample)[1])
}

# Apply the matching function to create a mapping
bruv_to_plankton_map <- bruv_summary %>%
  rowwise() %>%
  mutate(
    PlanktonSample = match_string_to_sample(String, Site, Zone, plankton_summary)
  ) %>%
  ungroup()

# Count how many BRUV strings could be matched with plankton samples
cat("BRUV strings with matched plankton samples:", sum(!is.na(bruv_to_plankton_map$PlanktonSample)), 
    "out of", nrow(bruv_to_plankton_map), "\n")

# Remove any strings that don't have a matching plankton sample
bruv_to_plankton_map <- bruv_to_plankton_map %>%
  filter(!is.na(PlanktonSample))

cat("Number of BRUV strings with valid plankton matches:", nrow(bruv_to_plankton_map), "\n")
valid_strings <- bruv_to_plankton_map$String

# ----------------------------------------
# Step 4: Prepare fish taxa data in wide format
# ----------------------------------------

# Get all fish taxa for strings with plankton matches
fish_taxa <- bruv_data_filtered %>%
  filter(String %in% valid_strings, Binomial != "None") %>%
  select(String, Family, Binomial, `Common name`, MaxN_string, Total_biomass_kg)

# Create wide format with each fish species as a column
fish_taxa_wide <- fish_taxa %>%
  # Create a clean species name for column headers
  mutate(Species_Column = str_replace_all(Binomial, " ", "_")) %>%
  # Pivot to wide format with species as columns and MaxN as values
  pivot_wider(
    id_cols = String,
    names_from = Species_Column,
    values_from = MaxN_string,
    values_fill = 0,
    names_prefix = "Fish_"
  )

# ----------------------------------------
# Step 5: Prepare plankton taxa data in wide format
# ----------------------------------------

# Get plankton data for the matched samples
plankton_taxa <- plankton_data %>%
  filter(Sample %in% bruv_to_plankton_map$PlanktonSample) %>%
  select(Sample, `Specimen Type`, `Plankton Category`, `Total Count`)

# Create wide format with each plankton specimen type as a column
plankton_taxa_wide <- plankton_taxa %>%
  # Group by sample and specimen type
  group_by(Sample, `Specimen Type`) %>%
  summarize(
    TypeCount = sum(`Total Count`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Create a clean specimen type name for column headers
  mutate(Type_Column = str_replace_all(`Specimen Type`, "[ -]", "_")) %>%
  # Pivot to wide format with specimen types as columns and counts as values
  pivot_wider(
    id_cols = Sample,
    names_from = Type_Column,
    values_from = TypeCount,
    values_fill = 0,
    names_prefix = "Plankton_"
  )

# ----------------------------------------
# Step 6: Create a combined dataset
# ----------------------------------------

# 1. Start with the string-level summary as base
combined_data <- bruv_to_plankton_map %>%
  select(String, Site, Zone, PlanktonSample, TotalMaxN, TotalBiomass_kg, SpeciesRichness)

# 2. Add total plankton count
combined_data <- combined_data %>%
  left_join(
    plankton_summary %>% select(Sample, TotalPlanktonCount),
    by = c("PlanktonSample" = "Sample")
  )

# 3. Add fish taxa columns
combined_data <- combined_data %>%
  left_join(fish_taxa_wide, by = "String")

# 4. Add plankton taxa columns
combined_data <- combined_data %>%
  left_join(plankton_taxa_wide, by = c("PlanktonSample" = "Sample"))

# Replace any NAs with 0 in the species/specimen columns
combined_data <- combined_data %>%
  mutate(across(starts_with(c("Fish_", "Plankton_")), ~replace_na(., 0)))

# ----------------------------------------
# Step 7: Add taxonomic information tables
# ----------------------------------------

# Create a fish taxonomy reference table
fish_taxonomy <- fish_taxa %>%
  select(Binomial, Family, `Common name`) %>%
  distinct() %>%
  mutate(Species_Column = str_c("Fish_", str_replace_all(Binomial, " ", "_")))

# Create a plankton taxonomy reference table
plankton_taxonomy <- plankton_taxa %>%
  select(`Specimen Type`, `Plankton Category`) %>%
  distinct() %>%
  mutate(Type_Column = str_c("Plankton_", str_replace_all(`Specimen Type`, "[ -]", "_")))

# ----------------------------------------
# Step 8: Save the dataset
# ----------------------------------------

# Save the combined dataset
write_csv(combined_data, "BRUV_Plankton_Combined_Taxa.csv")

# Save the taxonomy reference tables
write_csv(fish_taxonomy, "Fish_Taxonomy_Reference.csv")
write_csv(plankton_taxonomy, "Plankton_Taxonomy_Reference.csv")

# Print a completion message
cat("\nCombined taxa dataset created successfully!\n")
cat("Main dataset saved as: BRUV_Plankton_Combined_Taxa.csv\n")
cat("Fish taxonomy reference saved as: Fish_Taxonomy_Reference.csv\n")
cat("Plankton taxonomy reference saved as: Plankton_Taxonomy_Reference.csv\n")

# ----------------------------------------
# Step 9: Examine the dataset
# ----------------------------------------

# Print summary information
cat("\nSummary of the combined dataset:\n")
cat("Number of BRUV strings:", nrow(combined_data), "\n")
cat("Number of fish species columns:", sum(startsWith(names(combined_data), "Fish_")), "\n")
cat("Number of plankton type columns:", sum(startsWith(names(combined_data), "Plankton_")), "\n")
cat("Total number of columns:", ncol(combined_data), "\n")

# Print column names
cat("\nFirst 5 fish species columns:\n")
print(head(names(combined_data)[startsWith(names(combined_data), "Fish_")], 5))

cat("\nFirst 5 plankton type columns:\n")
print(head(names(combined_data)[startsWith(names(combined_data), "Plankton_")], 5))

# ----------------------------------------
# Step 10: Create example visualizations
# ----------------------------------------

# Plot 1: Correlation between total fish MaxN and plankton count
ggplot(combined_data, aes(x = TotalPlanktonCount, y = TotalMaxN, color = Zone)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(
    title = "Relationship Between Total Plankton Count and Fish MaxN",
    x = "Total Plankton Count",
    y = "Total Fish MaxN",
    color = "Zone"
  ) +
  theme_minimal() +
  scale_color_viridis_d()

# Save the plot
ggsave("total_plankton_vs_fish.png", width = 10, height = 7)

# Plot 2: Heatmap of fish species vs plankton types
# (This is just a sample visualization with a subset of the data)

# Get the top fish species (by total MaxN)
fish_cols <- names(combined_data)[startsWith(names(combined_data), "Fish_")]
top_fish <- combined_data %>%
  summarize(across(all_of(fish_cols), sum)) %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "TotalMaxN") %>%
  arrange(desc(TotalMaxN)) %>%
  head(5) %>%
  pull(Species)

# Get the top plankton types (by total count)
plankton_cols <- names(combined_data)[startsWith(names(combined_data), "Plankton_")]
top_plankton <- combined_data %>%
  summarize(across(all_of(plankton_cols), sum)) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "TotalCount") %>%
  arrange(desc(TotalCount)) %>%
  head(5) %>%
  pull(Type)

# Create correlation matrix between top fish and plankton
corr_data <- combined_data %>%
  select(all_of(c(top_fish, top_plankton))) %>%
  cor()

# Subset the correlation matrix to just fish-plankton correlations
corr_subset <- corr_data[startsWith(rownames(corr_data), "Fish_"), 
                         startsWith(colnames(corr_data), "Plankton_")]

# Convert to long format for plotting
corr_long <- as.data.frame(corr_subset) %>%
  rownames_to_column("Fish") %>%
  pivot_longer(
    cols = -Fish,
    names_to = "Plankton",
    values_to = "Correlation"
  ) %>%
  # Clean up names for the plot
  mutate(
    Fish = str_replace(Fish, "Fish_", ""),
    Fish = str_replace_all(Fish, "_", " "),
    Plankton = str_replace(Plankton, "Plankton_", ""),
    Plankton = str_replace_all(Plankton, "_", " ")
  )

# Create the heatmap
ggplot(corr_long, aes(x = Plankton, y = Fish, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1, 1)
  ) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  labs(
    title = "Correlation Between Top Fish Species and Plankton Types",
    x = "Plankton Type",
    y = "Fish Species",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face = "italic")
  )

# Save the plot
ggsave("fish_plankton_correlation_heatmap.png", width = 12, height = 8)