# Composite Sampling Analysis

## Overview
This analysis framework integrates high-resolution (2cm) and composite (20cm) soil core data to model depth profiles and spatial variability of soil organic carbon (SOC) and bulk density.

## Required Data Files
- `hr_cores.csv`: High-resolution cores (2cm increments)
- `composite_cores.csv`: Composite cores (20cm increments)

### Data Format
Both CSV files should have the following columns:
- `core_id`: Unique identifier for each core
- `latitude`: Latitude coordinate (decimal degrees)
- `longitude`: Longitude coordinate (decimal degrees)
- `depth_top`: Top depth of sample (cm)
- `depth_bottom`: Bottom depth of sample (cm)
- `SOC`: Soil organic carbon concentration (g/kg)
- `bulk_density`: Bulk density (g/cm³)

## Analysis Workflow

### 1. Setup
Run `00_setup.R` first to:
- Install required R packages
- Create directory structure
- Verify data files

### 2. Data Preparation (`01_data_prep.R`)
- Load and clean core data
- Calculate SOC stocks
- Aggregate high-resolution data to composite resolution
- Generate summary statistics

### 3. Exploratory Analysis (`02_exploratory_analysis.R`)
- Visualize depth profiles
- Create spatial distribution maps
- Analyze variability patterns
- Generate correlation matrices

### 4. Depth Modeling (`03_depth_modeling.R`)
- Fit exponential decay models
- Mixed-effects models with random effects for cores
- Generalized Additive Models (GAMs)
- Model comparison and validation

### 5. Spatial Modeling (`04_spatial_modeling.R`)
- Extract covariate values from GEE rasters
- Random Forest modeling with environmental covariates
- Geostatistical analysis (variograms)
- Ordinary kriging for spatial interpolation
- Regression kriging with covariates

## Covariates from Google Earth Engine
Place exported GEE rasters in the `covariates/` folder:
- `DEM.tif`: Digital elevation model
- `NDVI.tif`: Normalized Difference Vegetation Index
- `EVI.tif`: Enhanced Vegetation Index
- `VV.tif`: Sentinel-1 VV polarization
- `VH.tif`: Sentinel-1 VH polarization
- `Red.tif`, `Green.tif`, `Blue.tif`: RGB bands

## Outputs

### Processed Data (`data_processed/`)
- `hr_cores_clean.rds`: Cleaned high-resolution data
- `composite_cores_clean.rds`: Cleaned composite data
- `hr_cores_aggregated.rds`: High-res aggregated to composite resolution
- `combined_cores.rds`: Combined dataset for analysis

### Results (`outputs/`)
- Summary statistics tables (CSV)
- Model performance metrics
- Predictions at standard depths
- Kriging predictions

### Visualizations (`outputs/plots/`)
- Depth profile comparisons
- Spatial distribution maps
- Model diagnostics
- Variable importance plots
- Variograms and kriging maps

### Models (`outputs/models/`)
- Mixed-effects models (lme)
- GAM models
- Random Forest models
- Regression models with covariates

## Helper Functions (`helper_functions.R`)
Reusable functions for:
- SOC stock calculations
- Data aggregation
- Quality checks
- Plotting utilities
- Spatial data export

## Requirements
- R >= 4.0
- Required packages listed in `00_setup.R`
- Sufficient memory for spatial raster operations

## Contact
For questions about this analysis framework, refer to the documentation in each module.

