# Load Required Libraries
# This file loads all necessary packages for the Embodied Cities analysis

library(tidyverse)
library(sf)
library(leaflet)
library(httr)
library(jsonlite)
library(cluster)
library(here)
library(janitor)

# Set project-wide options
options(scipen = 999)  # Avoid scientific notation
theme_set(theme_minimal())  # Set default ggplot theme

cat("Libraries loaded successfully!\n")