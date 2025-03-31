##Combined data investigation - relationships between fish and zooplankton

# Load necessary libraries
library(tidyverse)

# Read the combined site-level dataset
site_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /Plankton and BRUV_Combined/Combined_Plankton_BRUV_Site_Level.csv")

# Display information about the dataset
dim(site_data)  # Shows number of rows and columns
head(site_data)  # Shows first few rows

# Create a correlation matrix for the main metrics
correlation_matrix <- site_data %>%
  select(Total_Plankton_Count, Unique_Plankton_Types, 
         Total_Fish_MaxN, Total_Fish_Biomass_kg, Fish_Species_Richness) %>%
  cor()

# Print the correlation matrix
print(correlation_matrix)

# If you have the corrplot package installed, visualize the correlations
library(corrplot)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45)

# Plot 1: Plankton Count vs. Fish MaxN
ggplot(site_data, aes(x = Total_Plankton_Count, y = Total_Fish_MaxN, color = Zone)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40") +
  geom_text(aes(label = Site), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(
    title = "Relationship Between Plankton Count and Fish Abundance",
    x = "Total Plankton Count",
    y = "Total Fish MaxN (Abundance)",
    color = "Zone"
  ) +
  theme_minimal()

# Plot 2: Plankton Count vs. Fish Species Richness
ggplot(site_data, aes(x = Total_Plankton_Count, y = Fish_Species_Richness, color = Zone)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40") +
  geom_text(aes(label = Site), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(
    title = "Relationship Between Plankton Count and Fish Species Richness",
    x = "Total Plankton Count",
    y = "Fish Species Richness",
    color = "Zone"
  ) +
  theme_minimal()

# Test the relationship between plankton count and fish MaxN
model_maxn <- lm(Total_Fish_MaxN ~ Total_Plankton_Count, data = site_data)
summary(model_maxn)

# Test the relationship between plankton count and fish species richness
model_richness <- lm(Fish_Species_Richness ~ Total_Plankton_Count, data = site_data)
summary(model_richness)

# Test with correlation tests (includes p-values)
cor.test(site_data$Total_Plankton_Count, site_data$Total_Fish_MaxN)
cor.test(site_data$Total_Plankton_Count, site_data$Fish_Species_Richness)