# Utility Functions for Embodied Cities Philadelphia Analysis
# Custom functions used across multiple scripts

# =============================================================================
# DATA PROCESSING UTILITIES
# =============================================================================

#' Clean and standardize neighborhood names
#' @param neighborhood_names Character vector of neighborhood names
#' @return Cleaned neighborhood names
clean_neighborhood_names <- function(neighborhood_names) {
  neighborhood_names %>%
    str_to_title() %>%
    str_trim() %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("[^A-Za-z0-9\\s]", "")
}

#' Convert coordinates to spatial points
#' @param data Data frame with lat/lon columns
#' @param lat_col Name of latitude column  
#' @param lon_col Name of longitude column
#' @param crs Coordinate reference system (default: WGS84)
#' @return sf spatial data frame
coords_to_spatial <- function(data, lat_col = "lat", lon_col = "lon", crs = 4326) {
  data %>%
    filter(!is.na(.data[[lat_col]]), !is.na(.data[[lon_col]])) %>%
    st_as_sf(coords = c(lon_col, lat_col), crs = crs)
}

#' Calculate distance to nearest feature
#' @param points Spatial points (sf object)
#' @param features Spatial features to calculate distance to (sf object)
#' @return Numeric vector of distances in meters
distance_to_nearest <- function(points, features) {
  # Transform to projected coordinate system for accurate distance calculation
  points_proj <- st_transform(points, crs = 3857)  # Web Mercator
  features_proj <- st_transform(features, crs = 3857)
  
  # Calculate distances
  distances <- st_distance(points_proj, features_proj)
  
  # Return minimum distance for each point
  apply(distances, 1, min)
}

# =============================================================================
# STATISTICAL UTILITIES  
# =============================================================================

#' Standardize variables to 0-1 scale
#' @param x Numeric vector
#' @return Normalized vector between 0 and 1
normalize_0_1 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#' Calculate composite index from multiple indicators
#' @param data Data frame with indicator columns
#' @param indicators Character vector of column names to include
#' @param weights Numeric vector of weights (must sum to 1)
#' @return Numeric vector of composite scores
calculate_composite_index <- function(data, indicators, weights = NULL) {
  
  if (is.null(weights)) {
    weights <- rep(1/length(indicators), length(indicators))
  }
  
  # Normalize all indicators to 0-1 scale
  normalized_data <- data %>%
    select(all_of(indicators)) %>%
    mutate(across(everything(), normalize_0_1))
  
  # Calculate weighted sum
  as.matrix(normalized_data) %*% weights
}

#' Assign quintiles to numeric variable
#' @param x Numeric vector
#' @param labels Character vector of labels (default: "Very Low" to "Very High")
#' @return Factor with quintile assignments
assign_quintiles <- function(x, labels = c("Very Low", "Low", "Medium", "High", "Very High")) {
  cut(x, 
      breaks = quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE),
      labels = labels,
      include.lowest = TRUE)
}

# =============================================================================
# VISUALIZATION UTILITIES
# =============================================================================

#' Create consistent color palette for maps
#' @param type Type of palette ("stress", "wellbeing", "change")
#' @return Character vector of hex colors
get_project_palette <- function(type = "stress") {
  palettes <- list(
    stress = c("#2166ac", "#67a9cf", "#f7f7f7", "#fdbf6f", "#d73027"),
    wellbeing = c("#d73027", "#fdbf6f", "#f7f7f7", "#67a9cf", "#2166ac"),
    change = c("#ca0020", "#f4a582", "#ffffff", "#92c5de", "#0571b0")
  )
  
  palettes[[type]]
}

#' Save plot with consistent dimensions and quality
#' @param plot ggplot object
#' @param filename Filename (without extension)
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution
save_project_plot <- function(plot, filename, width = 10, height = 8, dpi = 300) {
  
  # Save as PNG
  ggsave(
    filename = here("output", "figures", paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    type = "cairo"
  )
  
  # Also save as SVG for web use
  ggsave(
    filename = here("output", "figures", paste0(filename, ".svg")),
    plot = plot,
    width = width,
    height = height
  )
  
  cat("Saved:", filename, "in PNG and SVG formats\n")
}

# =============================================================================
# PROJECT-SPECIFIC CALCULATIONS
# =============================================================================

#' Calculate stress index based on multiple factors
#' @param crime_rate Crimes per 1000 residents
#' @param air_quality Air quality index (0-500, higher = worse)
#' @param walkability Walk score (0-100, higher = better)
#' @param vacant_rate Percent vacant properties
#' @return Stress index (0-10 scale)
calculate_stress_index <- function(crime_rate, air_quality, walkability, vacant_rate) {
  
  # Normalize inputs to 0-1 scale (higher = more stress)
  crime_norm <- normalize_0_1(crime_rate)
  air_norm <- normalize_0_1(air_quality)  # Already higher = worse
  walk_norm <- 1 - normalize_0_1(walkability)  # Flip so higher = worse
  vacant_norm <- normalize_0_1(vacant_rate)
  
  # Weighted composite (adjust weights based on research)
  stress_score <- (crime_norm * 0.35) + 
    (air_norm * 0.25) + 
    (walk_norm * 0.25) + 
    (vacant_norm * 0.15)
  
  # Scale to 0-10
  stress_score * 10
}

#' Calculate potential ROI from stress reduction interventions
#' @param baseline_stress Current stress index (0-10)
#' @param target_stress Target stress after intervention (0-10) 
#' @param population Neighborhood population
#' @param healthcare_cost_per_capita Annual healthcare costs per person
#' @param intervention_cost Total cost of intervention
#' @return List with ROI calculations
calculate_intervention_roi <- function(baseline_stress, target_stress, population, 
                                       healthcare_cost_per_capita, intervention_cost) {
  
  # Estimate healthcare cost reduction (based on research literature)
  # Assume 8% reduction in healthcare costs per 1-point stress reduction
  stress_reduction <- baseline_stress - target_stress
  healthcare_reduction_rate <- 0.08 * stress_reduction
  
  # Annual savings
  annual_savings <- population * healthcare_cost_per_capita * healthcare_reduction_rate
  
  # Calculate ROI metrics
  payback_period <- intervention_cost / annual_savings
  roi_5_year <- ((annual_savings * 5) - intervention_cost) / intervention_cost * 100
  
  list(
    stress_reduction = stress_reduction,
    annual_savings = annual_savings,
    payback_period_years = payback_period,
    roi_5_year_percent = roi_5_year,
    total_5_year_savings = annual_savings * 5
  )
}

# =============================================================================
# LOGGING AND PROGRESS TRACKING
# =============================================================================

#' Print progress message with timestamp
#' @param message Message to display
#' @param level Importance level ("INFO", "WARNING", "ERROR")
log_progress <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] ", level, ": ", message, "\n"))
}

cat("Utility functions loaded successfully!\n")