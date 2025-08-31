# Embodied Cities Philadelphia - Data Processing
# This script processes raw Philadelphia data into neighborhood-level indicators

source(here::here("R", "00_setup.R"))

# Add utility function for normalization
normalize_0_1 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

cat("=== EMBODIED CITIES: ENHANCED DATA PROCESSING ===\n")
cat("Creating embodied stress index with biometric proxies...\n\n")

# =============================================================================
# LOAD ALL DATASETS
# =============================================================================

# Load processed neighborhood data
crime_data <- read_csv(here("data", "raw", "crime_incidents_2023.csv"))
service_311 <- read_csv(here("data", "raw", "311_requests_2023.csv"))

# New health proxy datasets
health_indicators <- read_csv(here("data", "raw", "health_of_city_proxy.csv"))
school_data <- read_csv(here("data", "raw", "school_attendance_proxy.csv"))
health_proxies <- read_csv(here("data", "raw", "health_proxies_enhanced.csv"))
biometric_proxies <- read_csv(here("data", "raw", "biometric_stress_proxies.csv"))

cat("Loaded enhanced datasets:\n")
cat("- Crime incidents:", nrow(crime_data), "records\n")
cat("- 311 requests:", nrow(service_311), "records\n")
cat("- Health indicators:", nrow(health_indicators), "neighborhoods\n")
cat("- School data:", nrow(school_data), "schools\n")
cat("- Biometric proxies:", nrow(biometric_proxies), "neighborhoods\n\n")

# =============================================================================
# PROCESS CRIME DATA (Enhanced with stress categorization)
# =============================================================================

# Create neighborhood grid system (from your original script)
lat_breaks <- seq(39.88, 40.15, length.out = 4)  
lon_breaks <- seq(-75.35, -74.95, length.out = 6) 

# Enhanced crime processing with stress-specific categories
crime_data_enhanced <- crime_data %>%
  filter(!is.na(point_x), !is.na(point_y)) %>%
  filter(point_y > 39.88, point_y < 40.15, point_x > -75.35, point_x < -74.95) %>%
  mutate(
    lat_zone = cut(point_y, breaks = lat_breaks, labels = c("South", "Central", "North")),
    lon_zone = cut(point_x, breaks = lon_breaks, labels = c("West", "WestCentral", "Central", "EastCentral", "East")),
    neighborhood_id = paste0(lat_zone, "_", lon_zone),
    
    # Enhanced crime categorization for embodied stress
    violent_stress_crimes = case_when(
      str_detect(text_general_code, "Assault|Robbery|Homicide") ~ "High Trauma",
      str_detect(text_general_code, "Domestic") ~ "Interpersonal Trauma", 
      TRUE ~ "Other"
    ),
    
    environmental_stress_crimes = case_when(
      str_detect(text_general_code, "Vehicle|Theft") ~ "Security Stress",
      str_detect(text_general_code, "Drug") ~ "Environmental Disorder",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(neighborhood_id))

# Calculate enhanced crime stress indicators
neighborhood_crime_enhanced <- crime_data_enhanced %>%
  group_by(neighborhood_id, lat_zone, lon_zone) %>%
  summarise(
    total_crimes = n(),
    high_trauma_crimes = sum(violent_stress_crimes == "High Trauma", na.rm = TRUE),
    interpersonal_trauma = sum(violent_stress_crimes == "Interpersonal Trauma", na.rm = TRUE),
    environmental_disorder = sum(environmental_stress_crimes == "Environmental Disorder", na.rm = TRUE),
    security_stress = sum(environmental_stress_crimes == "Security Stress", na.rm = TRUE),
    avg_lat = mean(point_y, na.rm = TRUE),
    avg_lon = mean(point_x, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Enhanced embodied stress calculations
    acute_trauma_rate = (high_trauma_crimes + interpersonal_trauma) / total_crimes * 100,
    chronic_stress_rate = (environmental_disorder + security_stress) / total_crimes * 100,
    
    # Embodied stress composite from crime (0-10 scale)
    crime_embodied_stress = pmin(10, 
                                 (high_trauma_crimes / 20) * 4 +           # Acute trauma impact
                                   (interpersonal_trauma / 15) * 2 +         # Interpersonal safety
                                   (environmental_disorder / 25) * 2.5 +     # Environmental disorder
                                   (security_stress / 30) * 1.5              # General security
    )
  )

cat("Enhanced crime analysis created for", nrow(neighborhood_crime_enhanced), "neighborhoods\n")

# =============================================================================
# PROCESS 311 DATA (Enhanced with embodied stress categories)
# =============================================================================

service_311_enhanced <- service_311 %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  filter(lat > 39.88, lat < 40.15, lon > -75.35, lon < -74.95) %>%
  mutate(
    lat_zone = cut(lat, breaks = lat_breaks, labels = c("South", "Central", "North")),
    lon_zone = cut(lon, breaks = lon_breaks, labels = c("West", "WestCentral", "Central", "EastCentral", "East")),
    neighborhood_id = paste0(lat_zone, "_", lon_zone),
    
    # Categorize 311 requests by embodied stress impact
    embodied_stress_category = case_when(
      str_detect(service_name, "Abandoned Vehicle|Illegal Dumping|Graffiti") ~ "Visual Blight",
      str_detect(service_name, "Noise|Music|Party") ~ "Auditory Stress",
      str_detect(service_name, "Street Light|Pothole|Sidewalk") ~ "Mobility Barriers",
      str_detect(service_name, "Trash|Recycling|Sanitation") ~ "Environmental Health",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(neighborhood_id))

# Calculate enhanced 311 stress indicators
neighborhood_311_enhanced <- service_311_enhanced %>%
  group_by(neighborhood_id) %>%
  summarise(
    total_311_requests = n(),
    visual_blight = sum(embodied_stress_category == "Visual Blight", na.rm = TRUE),
    auditory_stress = sum(embodied_stress_category == "Auditory Stress", na.rm = TRUE),
    mobility_barriers = sum(embodied_stress_category == "Mobility Barriers", na.rm = TRUE),
    environmental_health = sum(embodied_stress_category == "Environmental Health", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Environmental embodied stress composite (0-10 scale)
    environmental_embodied_stress = pmin(10,
                                         (visual_blight / 15) * 2.5 +         # Visual environment impact
                                           (auditory_stress / 8) * 2 +          # Sound environment  
                                           (mobility_barriers / 20) * 2 +       # Physical accessibility
                                           (environmental_health / 25) * 1.5 +  # Environmental quality
                                           (total_311_requests / 100) * 2       # Overall neighborhood stress
    )
  )

cat("Enhanced 311 analysis created for", nrow(neighborhood_311_enhanced), "neighborhoods\n")

# =============================================================================
# PROCESS SCHOOL DATA FOR CHILD WELLBEING PROXY
# =============================================================================

# Aggregate school data to neighborhood level
school_neighborhood_summary <- school_data %>%
  group_by(neighborhood_id) %>%
  summarise(
    schools_in_area = n(),
    avg_chronic_absenteeism = mean(chronic_absenteeism_rate, na.rm = TRUE),
    avg_attendance_rate = mean(attendance_rate, na.rm = TRUE),
    total_students = sum(total_enrollment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Child wellbeing embodied stress (0-10 scale, higher = more stress)
    child_embodied_stress = pmin(10, 
                                 (avg_chronic_absenteeism / 10) +     # Chronic absenteeism as stress proxy
                                   ((100 - avg_attendance_rate) * 0.5)  # Poor attendance as additional stress
    )
  )

cat("School-based child wellbeing analysis for", nrow(school_neighborhood_summary), "neighborhoods\n")

# =============================================================================
# COMBINE ALL DATA INTO COMPREHENSIVE EMBODIED STRESS INDEX
# =============================================================================

# Join all datasets
embodied_stress_comprehensive <- neighborhood_crime_enhanced %>%
  left_join(neighborhood_311_enhanced, by = "neighborhood_id") %>%
  left_join(school_neighborhood_summary, by = "neighborhood_id") %>%
  left_join(health_proxies, by = "neighborhood_id") %>%
  left_join(biometric_proxies, by = "neighborhood_id") %>%
  mutate(
    # Handle missing data with neighborhood averages
    total_311_requests = coalesce(total_311_requests, 0),
    environmental_embodied_stress = coalesce(environmental_embodied_stress, 
                                             mean(environmental_embodied_stress, na.rm = TRUE)),
    child_embodied_stress = coalesce(child_embodied_stress,
                                     mean(child_embodied_stress, na.rm = TRUE)),
    health_stress_score = coalesce(health_stress_score,
                                   mean(health_stress_score, na.rm = TRUE)),
    biometric_stress_standardized = coalesce(biometric_stress_standardized,
                                             mean(biometric_stress_standardized, na.rm = TRUE))
  ) %>%
  mutate(
    # COMPREHENSIVE EMBODIED STRESS INDEX 
    # Uses multiple validated proxies for biometric stress
    comprehensive_embodied_stress = (
      crime_embodied_stress * 0.25 +           # Public safety stress
        environmental_embodied_stress * 0.20 +   # Environmental stressors  
        child_embodied_stress * 0.15 +           # Community wellbeing proxy
        health_stress_score * 0.20 +             # Health access stress
        biometric_stress_standardized * 0.20     # Biometric proxy composite
    ),
    
    # Standardize to 0-10 scale
    final_embodied_stress_index = round(pmin(10, pmax(0, comprehensive_embodied_stress)), 2),
    
    # Create meaningful categories
    embodied_stress_category = case_when(
      final_embodied_stress_index <= 2.5 ~ "Low Embodied Stress",
      final_embodied_stress_index <= 5.0 ~ "Moderate Embodied Stress",
      final_embodied_stress_index <= 7.5 ~ "High Embodied Stress", 
      TRUE ~ "Severe Embodied Stress"
    ),
    
    # Geographic coordinates
    lat = avg_lat,
    lon = avg_lon
  ) %>%
  filter(total_crimes > 5)  # Filter areas with minimal data

# =============================================================================
# VALIDATION AND QUALITY CHECKS
# =============================================================================

cat("\n=== COMPREHENSIVE EMBODIED STRESS ANALYSIS ===\n")
cat("Neighborhoods analyzed:", nrow(embodied_stress_comprehensive), "\n")

stress_distribution <- table(embodied_stress_comprehensive$embodied_stress_category)
cat("\nEmbodied Stress Distribution:\n")
print(stress_distribution)
print(round(prop.table(stress_distribution) * 100, 1))

# Correlation analysis of stress components
cat("\n=== STRESS COMPONENT CORRELATIONS ===\n")
stress_correlations <- embodied_stress_comprehensive %>%
  select(
    final_embodied_stress_index, crime_embodied_stress, environmental_embodied_stress,
    child_embodied_stress, health_stress_score, biometric_stress_standardized
  ) %>%
  cor(use = "complete.obs")

print(round(stress_correlations, 3))

# Identify highest stress neighborhoods for intervention targeting
high_stress_areas <- embodied_stress_comprehensive %>%
  arrange(desc(final_embodied_stress_index)) %>%
  head(3) %>%
  select(neighborhood_id, final_embodied_stress_index, crime_embodied_stress, 
         child_embodied_stress, biometric_stress_standardized)

cat("\n=== HIGHEST EMBODIED STRESS NEIGHBORHOODS ===\n")
print(high_stress_areas)

# =============================================================================
# SAVE ENHANCED PROCESSED DATA
# =============================================================================

# Save the comprehensive embodied stress dataset
write_csv(embodied_stress_comprehensive, here("data", "processed", "philadelphia_embodied_stress_comprehensive.csv"))

# Save stress component breakdown for analysis
stress_components_breakdown <- embodied_stress_comprehensive %>%
  select(neighborhood_id, lat_zone, lon_zone, lat, lon,
         crime_embodied_stress, environmental_embodied_stress, child_embodied_stress,
         health_stress_score, biometric_stress_standardized, final_embodied_stress_index,
         embodied_stress_category)

write_csv(stress_components_breakdown, here("data", "processed", "embodied_stress_components.csv"))

# Save correlation matrix for methodology validation
write_csv(as.data.frame(stress_correlations), here("data", "outputs", "stress_correlation_matrix.csv"))

# =============================================================================
# CREATE ZINE-READY METHODOLOGY DOCUMENTATION
# =============================================================================

embodied_methodology <- tribble(
  ~component, ~proxy_used, ~data_source, ~weight, ~rationale,
  
  "Acute Stress Events", "High trauma crimes + interpersonal violence", 
  "Philadelphia Police Department", "25%", 
  "Violent crime creates lasting physiological stress responses",
  
  "Environmental Stressors", "311 visual blight + auditory stress + mobility barriers", 
  "Philadelphia 311 System", "20%", 
  "Environmental disorder correlates with cortisol levels and chronic stress",
  
  "Child Wellbeing", "School chronic absenteeism rates", 
  "School District of Philadelphia", "15%", 
  "Child stress reflects broader community stress; absenteeism indicates family/neighborhood distress",
  
  "Health Access Stress", "Mental health facilities + pharmacy access + food deserts", 
  "Multiple Philadelphia sources", "20%", 
  "Poor health access creates chronic stress and reduces resilience",
  
  "Biometric Proxies", "Composite of acute stress events + chronic health burden + isolation", 
  "Calculated proxies", "20%", 
  "Multi-factor proxy for actual biometric stress measurements"
)

write_csv(embodied_methodology, here("data", "outputs", "embodied_methodology_documentation.csv"))

# =============================================================================
# CREATE COMPARISON WITH ORIGINAL STRESS INDEX
# =============================================================================

# For comparison, calculate your original stress index
embodied_stress_comprehensive <- embodied_stress_comprehensive %>%
  mutate(
    # Original stress index calculation (from your 03_exploratory_analysis.R)
    original_crime_stress_norm = normalize_0_1(total_crimes),
    original_violent_crime_norm = normalize_0_1(acute_trauma_rate),
    original_environmental_norm = normalize_0_1(total_311_requests),
    
    original_composite_stress = (original_crime_stress_norm * 0.5 + 
                                   original_violent_crime_norm * 0.3 + 
                                   original_environmental_norm * 0.2) * 10,
    
    # Compare methodologies
    methodology_difference = final_embodied_stress_index - original_composite_stress,
    methodology_correlation = cor(final_embodied_stress_index, original_composite_stress, use = "complete.obs")
  )

cat("\n=== METHODOLOGY COMPARISON ===\n")
cat("Correlation between original and enhanced stress indices:", 
    round(cor(embodied_stress_comprehensive$final_embodied_stress_index, 
              embodied_stress_comprehensive$original_composite_stress, use = "complete.obs"), 3), "\n")

methodology_comparison <- embodied_stress_comprehensive %>%
  select(neighborhood_id, original_composite_stress, final_embodied_stress_index, methodology_difference) %>%
  arrange(desc(abs(methodology_difference))) %>%
  head(5)

cat("\nBiggest methodology differences:\n")
print(methodology_comparison)

# =============================================================================
# FINAL SUMMARY FOR ZINE INTEGRATION
# =============================================================================

final_summary_stats <- list(
  neighborhoods_analyzed = nrow(embodied_stress_comprehensive),
  
  stress_distribution = table(embodied_stress_comprehensive$embodied_stress_category),
  
  highest_stress_neighborhood = embodied_stress_comprehensive %>%
    filter(final_embodied_stress_index == max(final_embodied_stress_index)) %>%
    pull(neighborhood_id),
  
  highest_stress_score = max(embodied_stress_comprehensive$final_embodied_stress_index),
  
  lowest_stress_neighborhood = embodied_stress_comprehensive %>%
    filter(final_embodied_stress_index == min(final_embodied_stress_index)) %>%
    pull(neighborhood_id),
  
  lowest_stress_score = min(embodied_stress_comprehensive$final_embodied_stress_index),
  
  methodology_validation = cor(embodied_stress_comprehensive$final_embodied_stress_index, 
                               embodied_stress_comprehensive$original_composite_stress, use = "complete.obs")
)

# Save final summary
final_summary_stats_for_json <- list(
  neighborhoods_analyzed = nrow(embodied_stress_comprehensive),
  
  stress_distribution = as.data.frame(table(embodied_stress_comprehensive$embodied_stress_category)),
  
  highest_stress_neighborhood = embodied_stress_comprehensive %>%
    filter(final_embodied_stress_index == max(final_embodied_stress_index)) %>%
    pull(neighborhood_id),
  
  highest_stress_score = max(embodied_stress_comprehensive$final_embodied_stress_index),
  
  lowest_stress_neighborhood = embodied_stress_comprehensive %>%
    filter(final_embodied_stress_index == min(final_embodied_stress_index)) %>%
    pull(neighborhood_id),
  
  lowest_stress_score = min(embodied_stress_comprehensive$final_embodied_stress_index),
  
  methodology_validation = cor(embodied_stress_comprehensive$final_embodied_stress_index, 
                               embodied_stress_comprehensive$original_composite_stress, use = "complete.obs")
)

jsonlite::write_json(final_summary_stats_for_json, here("data", "outputs", "final_embodied_stress_summary.json"), pretty = TRUE)

cat("\n=== ENHANCED PROCESSING COMPLETE ===\n")
cat("✅ Comprehensive embodied stress index created\n")
cat("✅ Biometric proxies integrated successfully\n")
cat("✅ Methodology validation: r =", round(final_summary_stats$methodology_validation, 3), "\n")
cat("✅ Highest stress area:", final_summary_stats$highest_stress_neighborhood, 
    "(", final_summary_stats$highest_stress_score, "/10)\n")
cat("✅ Ready for enhanced clustering and intervention modeling\n")

cat("\n🎯 EMBODIED STRESS INDEX NOW INCLUDES:\n")
cat("• Crime-based trauma indicators\n")
cat("• Environmental stressor proxies\n") 
cat("• Child wellbeing metrics\n")
cat("• Health access barriers\n")
cat("• Multi-factor biometric proxies\n")