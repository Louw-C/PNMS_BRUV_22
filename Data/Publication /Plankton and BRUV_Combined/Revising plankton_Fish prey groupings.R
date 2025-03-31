# Plankton Functional Group Classification
# ----------------------------------------------
# This script adds ecological functional groups to the plankton dataset
# based on feeding relationships with pelagic fish

# Load required packages
library(tidyverse)
library(readr)


# Step 1: Load the plankton data
# ------------------------------
plankton_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /PAL_2022_Zooplankton_Classified.csv")

# Step 1: Verify balanced sampling across zones
# --------------------------------------------
samples_per_zone <- plankton_data %>%
  select(Zone, Sample) %>%
  distinct() %>%
  count(Zone, name = "NumSamples")

print("Samples per zone:")
print(samples_per_zone)

# Verify site-level sampling
samples_per_site <- plankton_data %>%
  select(Zone, Site, Sample) %>%
  distinct() %>%
  count(Zone, Site, name = "NumSamples") %>%
  arrange(Zone, Site)

print("Samples per site:")
print(samples_per_site)

# Step 2: Basic summary of the data
# --------------------------------
cat("Dataset summary:\n")
cat("Total records:", nrow(plankton_data), "\n")
cat("Total plankton count:", sum(plankton_data$`Total Count`), "\n")
cat("Unique samples:", length(unique(plankton_data$Sample)), "\n")

# Step 3: Analyze functional groups by zone
# ---------------------------------------

# Print column names to verify the exact spelling and format
cat("Column names in the dataset:\n")
print(colnames(plankton_data))

# Create a summary by functional group and zone using the correct column name
# (Using backticks to handle potential spaces in column names)
fg_by_zone <- plankton_data %>%
  group_by(Zone, `FunctionalGroup`) %>%  # Using backticks to be safe
  summarize(
    TotalCount = sum(`Total Count`, na.rm = TRUE),
    NumRecords = n(),
    NumSpecimenTypes = n_distinct(`Specimen Type`),
    .groups = "drop"
  ) %>%
  # Calculate percentage within each zone
  group_by(Zone) %>%
  mutate(
    ZoneTotal = sum(TotalCount),
    Percentage = TotalCount / ZoneTotal * 100
  ) %>%
  ungroup() %>%
  arrange(Zone, desc(Percentage))

# Display the summary to confirm it works
print(fg_by_zone)

# Step 4: Create visualizations
# ----------------------------

# 1. Stacked bar chart of functional group composition by zone (percentage)
plot1 <- ggplot(fg_by_zone, aes(x = Zone, y = Percentage, fill = FunctionalGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Plankton Functional Group Composition by Zone",
       subtitle = "Based on equal sampling effort (8 samples per zone)",
       x = "Zone", 
       y = "Percentage (%)",
       fill = "Functional Group") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
        legend.position = "right")
plot1

# 2. Bar chart of total abundance by zone (direct comparison due to equal sampling)
plot2 <- ggplot(fg_by_zone, aes(x = Zone, y = TotalCount, fill = FunctionalGroup)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Abundance of Functional Groups by Zone",
       subtitle = "Direct comparison with equal sampling effort (8 samples per zone)",
       x = "Zone", 
       y = "Total Count",
       fill = "Functional Group") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
        legend.position = "right")
plot2

# 3. Faceted bar chart to compare functional groups across zones
plot3 <- ggplot(fg_by_zone, aes(x = Zone, y = TotalCount, fill = Zone)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ FunctionalGroup, scales = "free_y") +
  labs(title = "Functional Group Abundance Across Zones",
       subtitle = "Direct comparison with equal sampling effort (8 samples per zone)",
       x = "Zone", 
       y = "Total Count",
       fill = "Zone") +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "lightgray"),
        strip.text = element_text(face = "bold"))
plot3

# Save the plots
ggsave("Plankton_Composition_by_Zone_Percentage.png", plot1, width = 10, height = 6)
ggsave("Plankton_Abundance_by_Zone.png", plot2, width = 10, height = 6)
ggsave("Functional_Groups_Across_Zones.png", plot3, width = 12, height = 8)