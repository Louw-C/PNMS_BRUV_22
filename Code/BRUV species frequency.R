# Visualizing Species Distribution Across Zones
# Using the species_metrics_by_site_with_se.csv file

# Load required packages
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# ---------------------------------------------------------
# Step 1: Read in the data
# ---------------------------------------------------------
# Read the species data
species_data <- read.csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /BRUV_Aggregated_No Outliers/species_metrics_by_site_with_se.csv", stringsAsFactors = FALSE)

# Remove any "None" observations if needed
species_data <- species_data %>%
  filter(Binomial != "None")

# Define zone order
zone_order <- c("PNMS North", "DFZ West", "PNMS South")

# Apply the ordering to the dataframe
species_data$Zone <- factor(species_data$Zone, levels = zone_order)

# ---------------------------------------------------------
# Step 2: Create presence/absence by zone
# ---------------------------------------------------------
# Determine which species occur in which zones
zone_presence <- species_data %>%
  # Select only the species and zone columns
  select(Binomial, Zone) %>%
  # Get unique species-zone combinations
  distinct() %>%
  # Add a presence indicator
  mutate(Present = TRUE) %>%
  # Convert to wide format for set analysis
  pivot_wider(
    id_cols = Binomial,
    names_from = Zone,
    values_from = Present,
    values_fill = FALSE
  )

# Fill in FALSE for NA values
zone_presence <- zone_presence %>%
  mutate(across(everything(), ~ if_is.na(., FALSE)))

# Calculate zone statistics
zone_stats <- species_data %>%
  group_by(Zone) %>%
  summarize(
    NumSites = n_distinct(Site),
    NumSpecies = n_distinct(Binomial),
    .groups = 'drop'
  )

# Calculate species that are unique to each zone or shared
species_zone_counts <- zone_presence %>%
  mutate(
    # Create a category for each possible pattern
    ZonePattern = case_when(
      `PNMS North` & `DFZ West` & `PNMS South` ~ "All Zones",
      `PNMS North` & `DFZ West` & !`PNMS South` ~ "North & West only",
      `PNMS North` & !`DFZ West` & `PNMS South` ~ "North & South only",
      !`PNMS North` & `DFZ West` & `PNMS South` ~ "West & South only",
      `PNMS North` & !`DFZ West` & !`PNMS South` ~ "North only",
      !`PNMS North` & `DFZ West` & !`PNMS South` ~ "West only",
      !`PNMS North` & !`DFZ West` & `PNMS South` ~ "South only"
    )
  )

# Count species in each pattern
pattern_counts <- species_zone_counts %>%
  count(ZonePattern) %>%
  arrange(desc(n))

# ---------------------------------------------------------
# Step 3: Create a horizontal bar chart of zone patterns
# ---------------------------------------------------------

# Create a data frame of zone patterns with counts
zone_patterns <- data.frame(
  Pattern = c("All Zones", "North & West only", "North & South only",
              "West & South only", "North only", "West only", "South only"),
  Count = c(
    sum(species_zone_counts$`PNMS North` & species_zone_counts$`DFZ West` & species_zone_counts$`PNMS South`),
    sum(species_zone_counts$`PNMS North` & species_zone_counts$`DFZ West` & !species_zone_counts$`PNMS South`),
    sum(species_zone_counts$`PNMS North` & !species_zone_counts$`DFZ West` & species_zone_counts$`PNMS South`),
    sum(!species_zone_counts$`PNMS North` & species_zone_counts$`DFZ West` & species_zone_counts$`PNMS South`),
    sum(species_zone_counts$`PNMS North` & !species_zone_counts$`DFZ West` & !species_zone_counts$`PNMS South`),
    sum(!species_zone_counts$`PNMS North` & species_zone_counts$`DFZ West` & !species_zone_counts$`PNMS South`),
    sum(!species_zone_counts$`PNMS North` & !species_zone_counts$`DFZ West` & species_zone_counts$`PNMS South`)
  )
)

# Order the patterns for the chart
zone_patterns$Pattern <- factor(zone_patterns$Pattern, 
                                levels = c("All Zones", 
                                           "North & West only", "North & South only", "West & South only",
                                           "North only", "West only", "South only"))

# Create a color palette for the patterns
pattern_colors <- c(
  "All Zones" = "#4DAF4A",          # Green for species in all zones
  "North & West only" = "#377EB8",  # Blue for North & West
  "North & South only" = "#984EA3", # Purple for North & South
  "West & South only" = "#FF7F00",  # Orange for West & South
  "North only" = "#A6CEE3",         # Light blue for North only
  "West only" = "#FDBF6F",          # Light orange for West only
  "South only" = "#CAB2D6"          # Light purple for South only
)

# Create the bar chart
p1 <- ggplot(zone_patterns, aes(x = reorder(Pattern, Count), y = Count, fill = Pattern)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), hjust = -0.2) +
  scale_fill_manual(values = pattern_colors) +
  coord_flip() +
  labs(title = "Species Distribution Patterns Across Zones",
       subtitle = "Number of species unique to each zone or shared between zones",
       x = NULL,
       y = "Number of Species") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))
p1

# Save the bar chart
ggsave("zone_pattern_counts.png", p1, width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------
# Step 4: Create a Venn diagram-like visualization
# ---------------------------------------------------------

# Get lists of species in each zone
north_species <- species_zone_counts %>% filter(`PNMS North` == TRUE) %>% pull(Binomial)
west_species <- species_zone_counts %>% filter(`DFZ West` == TRUE) %>% pull(Binomial)
south_species <- species_zone_counts %>% filter(`PNMS South` == TRUE) %>% pull(Binomial)

# Get species for each pattern
all_zones <- intersect(intersect(north_species, west_species), south_species)
north_west <- setdiff(intersect(north_species, west_species), south_species)
north_south <- setdiff(intersect(north_species, south_species), west_species)
west_south <- setdiff(intersect(west_species, south_species), north_species)
north_only <- setdiff(setdiff(north_species, west_species), south_species)
west_only <- setdiff(setdiff(west_species, north_species), south_species)
south_only <- setdiff(setdiff(south_species, north_species), west_species)

# Create a Venn-like diagram using ggplot2
p2 <- ggplot() +
  # Add labels for the zones
  annotate("text", x = 1, y = 3.3, label = paste("PNMS North\n", length(north_species), " species"), 
           size = 5, fontface = "bold", color = "#3B9AB2") +
  annotate("text", x = 3, y = 3.3, label = paste("DFZ West\n", length(west_species), " species"), 
           size = 5, fontface = "bold", color = "#E4B80E") +
  annotate("text", x = 2, y = 0.7, label = paste("PNMS South\n", length(south_species), " species"), 
           size = 5, fontface = "bold", color = "#1B8A6B") +
  
  # Add circles for the zones (semi-transparent)
  annotate("path", 
           x = 1 + 0.9 * cos(seq(0, 2*pi, length.out = 100)),
           y = 2 + 0.9 * sin(seq(0, 2*pi, length.out = 100)),
           color = "#3B9AB2", alpha = 0.3, size = 1) +
  annotate("path", 
           x = 3 + 0.9 * cos(seq(0, 2*pi, length.out = 100)),
           y = 2 + 0.9 * sin(seq(0, 2*pi, length.out = 100)),
           color = "#E4B80E", alpha = 0.3, size = 1) +
  annotate("path", 
           x = 2 + 0.9 * cos(seq(0, 2*pi, length.out = 100)),
           y = 1 + 0.9 * sin(seq(0, 2*pi, length.out = 100)),
           color = "#1B8A6B", alpha = 0.3, size = 1) +
  
  # Add text for each intersection
  annotate("text", x = 2, y = 2, label = paste("All Zones\n", length(all_zones)), 
           size = 4, fontface = "bold", color = pattern_colors["All Zones"]) +
  annotate("text", x = 2, y = 2.6, label = paste("North & West\n", length(north_west)), 
           size = 3.5, color = pattern_colors["North & West only"]) +
  annotate("text", x = 1.5, y = 1.5, label = paste("North & South\n", length(north_south)), 
           size = 3.5, color = pattern_colors["North & South only"]) +
  annotate("text", x = 2.5, y = 1.5, label = paste("West & South\n", length(west_south)), 
           size = 3.5, color = pattern_colors["West & South only"]) +
  annotate("text", x = 0.7, y = 2.6, label = paste("North only\n", length(north_only)), 
           size = 3.5, color = pattern_colors["North only"]) +
  annotate("text", x = 3.3, y = 2.6, label = paste("West only\n", length(west_only)), 
           size = 3.5, color = pattern_colors["West only"]) +
  annotate("text", x = 2, y = 0.3, label = paste("South only\n", length(south_only)), 
           size = 3.5, color = pattern_colors["South only"]) +
  
  # Set plot limits and remove axis details
  xlim(0, 4) + ylim(0, 4) +
  labs(title = "Species Distribution Across Zones",
       subtitle = "Number of species in each zone and shared between zones") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p2

# Save the Venn-like diagram
ggsave("zone_venn_diagram.png", p2, width = 8, height = 7, dpi = 300)

# ---------------------------------------------------------
# Step 5: Create species lists by pattern
# ---------------------------------------------------------

# Arrange the species by pattern
species_by_pattern <- rbind(
  data.frame(Binomial = all_zones, Pattern = "All Zones"),
  data.frame(Binomial = north_west, Pattern = "North & West only"),
  data.frame(Binomial = north_south, Pattern = "North & South only"),
  data.frame(Binomial = west_south, Pattern = "West & South only"),
  data.frame(Binomial = north_only, Pattern = "North only"),
  data.frame(Binomial = west_only, Pattern = "West only"),
  data.frame(Binomial = south_only, Pattern = "South only")
)

# Add family and common name information
species_info <- species_data %>%
  select(Binomial, Family, Common.name) %>%
  distinct()

species_by_pattern <- species_by_pattern %>%
  left_join(species_info, by = "Binomial") %>%
  arrange(Pattern, Binomial)

# Write to CSV
write.csv(species_by_pattern, "species_by_zone_pattern.csv", row.names = FALSE)

# ---------------------------------------------------------
# Step 6: Create a plot showing occurrence frequency by pattern
# ---------------------------------------------------------

# Calculate mean occurrence frequency by species and zone
species_occurrence <- species_data %>%
  group_by(Binomial, Zone) %>%
  summarize(
    MeanOccurrence = mean(OccurrenceFrequency),
    .groups = 'drop'
  )

# Add pattern information
species_occurrence <- species_occurrence %>%
  left_join(species_by_pattern %>% select(Binomial, Pattern), by = "Binomial")

# Create a plot showing occurrence by pattern
p3 <- ggplot(species_occurrence, aes(x = Zone, y = MeanOccurrence, group = Binomial, color = Pattern)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 3) +
  facet_wrap(~ Pattern) +
  scale_color_manual(values = pattern_colors) +
  labs(title = "Mean Occurrence Frequency by Zone and Distribution Pattern",
       x = "Zone", 
       y = "Mean Occurrence Frequency (%)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"),
        legend.position = "none")
p3

# Save the occurrence plot
ggsave("occurrence_by_pattern.png", p3, width = 12, height = 8, dpi = 300)

# ---------------------------------------------------------
# Step 7: Create a detailed species table
# ---------------------------------------------------------

# Get top species for each zone by occurrence
top_by_zone <- species_data %>%
  group_by(Zone, Binomial) %>%
  summarize(
    MeanOccurrence = mean(OccurrenceFrequency),
    MeanMaxN = mean(MeanMaxNPerString),
    .groups = 'drop'
  ) %>%
  group_by(Zone) %>%
  arrange(desc(MeanOccurrence)) %>%
  slice_head(n = 5) %>%
  left_join(species_info, by = "Binomial") %>%
  select(Zone, Binomial, Family, Common.name, MeanOccurrence, MeanMaxN)

# Create a plot of top species by zone
p4 <- ggplot(top_by_zone, aes(x = reorder(Binomial, MeanOccurrence), y = MeanOccurrence, fill = Zone)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Zone, scales = "free_y") +
  scale_fill_manual(values = c("PNMS North" = "#3B9AB2", "DFZ West" = "#E4B80E", "PNMS South" = "#1B8A6B")) +
  coord_flip() +
  labs(title = "Top 5 Species by Occurrence Frequency in Each Zone",
       x = NULL, 
       y = "Mean Occurrence Frequency (%)") +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"),
        legend.position = "none",
        axis.text.y = element_text(face = "italic"))
p4

# Save the top species plot
ggsave("top_species_by_zone.png", p4, width = 12, height = 8, dpi = 300)

# ---------------------------------------------------------
# Step 8: Summary Results
# ---------------------------------------------------------

# Print the key results to the console
cat("\n----- ZONE-LEVEL SPECIES DISTRIBUTION SUMMARY -----\n\n")
cat("Total number of species in dataset:", n_distinct(species_data$Binomial), "\n\n")

cat("Species counts by zone:\n")
print(zone_stats)

cat("\nSpecies distribution patterns:\n")
print(zone_patterns)

cat("\nZone-level species analysis complete.\n")
cat("Files created:\n")
cat("1. zone_pattern_counts.png - Bar chart of species by zone pattern\n")
cat("2. zone_venn_diagram.png - Venn-like diagram showing zone overlaps\n")
cat("3. species_by_zone_pattern.csv - List of species by zone pattern\n")
cat("4. occurrence_by_pattern.png - Species occurrence by pattern and zone\n")
cat("5. top_species_by_zone.png - Top species by occurrence in each zone\n")