# Embodied Cities: Philadelphia Analysis

A rigorous, data-driven approach to measuring and improving urban wellbeing through embodied sensing technology and community governance.

## Project Overview

This repository contains the analysis supporting the "Embodied Cities" speculative design project for "A Queer Civic Future" zine. Using real Philadelphia health, crime, education, and environmental data, we model the 25-year impact of community-controlled embodied sensing interventions on neighborhood wellbeing.

## Key Innovation

**Embodied stress measurement without invasive monitoring:** Uses administrative data proxies (crime incidents, 311 requests, school absenteeism, health access) to create a comprehensive embodied stress index that captures how neighborhoods affect residents' bodies and mental health.

## Repository Structure

```
embodied-cities-philadelphia/
├── data/                           # All datasets
│   ├── raw/                        # OpenDataPhilly downloads
│   ├── processed/                  # Cleaned analysis datasets  
│   ├── outputs/                    # Final results for zine
│   ├── external/                   # Manual data collection
│   └── validation/                 # Methodology validation
├── R/                              # Analysis scripts (run in order)
│   ├── 00_setup.R                    # Package loading and configuration
│   ├── utils.R                       # Custom analysis functions
│   ├── 01_data_collection.R          # Data download with health proxies
│   ├── 02_data_processing.R          # Embodied stress index creation
│   └── 03_intervention_modeling.R    # 25-year projections
├── output/                         # Generated analysis results
│   ├── figures/                    # Visualizations for zine
│   ├── tables/                     # Summary statistics
│   └── maps/                       # Spatial analysis
├── zine/                           # Digital zine materials
│   ├── assets/                     # Images, graphics
│   ├── data/                       # Final data packages
│   └── narrative/                  # Speculative storytelling
├── docs/                           # Documentation
│   ├── methodology/                # Technical approach
│   ├── funding/                    # Grant materials
│   └── literature/                 # Research background
└── README.md                       # This file
```

## Methodology

### Enhanced Embodied Stress Index (0-10 scale)
Combines 5 validated components:
- **Crime stress** (25%): Violent incidents, interpersonal trauma
- **Environmental stress** (20%): 311 complaints, neighborhood conditions  
- **Child wellbeing** (15%): School chronic absenteeism rates
- **Health access stress** (20%): Mental health facilities, food deserts
- **Biometric proxies** (20%): Composite of acute/chronic stress indicators

### Intervention Framework
Based on documented successful pilot projects:
- **Barcelona Superblocks:** Environmental improvements (25% air pollution reduction)
- **Community Health Worker programs:** Healthcare access (56% effectiveness rate)
- **Medellín Urban Acupuncture:** Community governance and resilience
- **Community violence intervention:** Safety improvements (16-23% reduction)

### Economic Impact Modeling
- **3-year pilot:** $1.4M investment, 1,200 residents, 15% stress reduction
- **25-year scaling:** $131M total investment, 500,000 people impacted
- **Conservative ROI:** Based on healthcare cost savings from stress reduction
- **Policy innovation:** First governance system centering embodied wellbeing

## Data Sources

### Philadelphia Open Data Portal
- Crime incidents (2023): 50,000+ records with location data
- 311 service requests (2023): 25,000+ environmental complaints  
- Census tract boundaries and demographics

### Philadelphia Department of Public Health
- Health of the City report: Depression rates, life expectancy by neighborhood
- Air quality monitoring: Environmental stress indicators

### School District of Philadelphia  
- Chronic absenteeism rates: Child wellbeing proxy (currently 36.6%)
- School performance data by location

### Research Literature
- Community Health Worker effectiveness studies
- Urban intervention impact evaluations  
- Stress-healthcare cost correlations

## Key Findings

### Baseline Conditions (2025)
- **Target neighborhood:** Central_Central district  
- **Embodied stress index:** 8.4/10 (severe stress)
- **Depression rate:** 24% (above national average)
- **Child chronic absenteeism:** 36.6% (stress indicator)

### Projected Transformation (2025 → 2050)
- **Stress reduction:** 8.4 → 4.6 (-45% over 25 years)
- **Healthcare savings:** $420-1,150 per capita annually
- **Community governance:** Transition from economic to embodied wellbeing metrics
- **Policy innovation:** Model replicated in 200+ neighborhoods across multiple cities

## For "A Queer Civic Future" Zine

This analysis provides the foundation for speculative storytelling about governance transformation:
- **Community sovereignty over biometric data**
- **Stress reduction as primary policy goal**  
- **Intersectional approach to urban justice**
- **Care-centered rather than productivity-centered city planning**

## Getting Started

1. **Install required packages:** `renv::restore()`
2. **Run analysis pipeline:** Source scripts 01-03 in order
3. **Generate zine materials:** Check `zine/data/` for final data packages
4. **Review methodology:** See `docs/methodology/` for technical details

## Funding Readiness

The analysis provides conservative, evidence-based projections suitable for:
- Community development grants
- Health equity funding
- Smart cities initiatives  
- Municipal innovation programs

**Created:** 2025 for "A Queer Civic Future" digital zine

---

*Making urban inequity impossible to ignore through biometric justice*

