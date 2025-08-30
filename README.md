# Embodied Cities: Philadelphia Analysis

A data-driven approach to measuring and improving urban wellbeing through sensing technology.

## Project Overview

This repository contains the analysis supporting the "Embodied Cities" speculative design project, using real Philadelphia health and neighborhood data to model the impact of embodied sensing technology on urban wellbeing.

## Repository Structure

```
embodied-cities-philadelphia/
├── data/                      # Data files
│   ├── raw/                   # Original data sources  
│   ├── processed/            # Cleaned datasets
│   └── outputs/              # Final metrics for zine
├── R/                        # Analysis scripts
├── output/                   # Generated figures, tables, maps
├── zine/                     # Final digital zine
├── docs/                     # Documentation
└── README.md                 # This file
```

## Methodology

### Phase 1: Neighborhood Selection
- Statistical clustering of Philadelphia neighborhoods by health/stress indicators
- Matched control design for intervention modeling
- Spatial analysis of environmental factors

### Phase 2: Intervention Modeling  
- Predictive modeling of embodied sensing interventions
- ROI calculations based on healthcare cost savings
- Before/after scenario development

### Phase 3: Visualization & Communication
- Interactive maps and data visualizations
- Integration with speculative design zine format

## Data Sources

- Philadelphia Department of Public Health
- OpenDataPhilly.org
- US Census American Community Survey
- Philadelphia Crime Statistics

## Getting Started

1. Install required R packages: `renv::restore()`
2. Run analysis pipeline: `source("R/run_analysis.R")`
3. View results in `output/` directory

## Contact

Created by S • Design X Data X Democracy

