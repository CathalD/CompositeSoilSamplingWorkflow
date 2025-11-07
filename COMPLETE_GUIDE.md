# Composite Sampling Analysis Framework
## Complete Guide for SOC and Bulk Density Analysis

---

## 📋 Table of Contents
1. [Overview](#overview)
2. [Installation & Setup](#installation--setup)
3. [Data Requirements](#data-requirements)
4. [Analysis Modules](#analysis-modules)
5. [Working with Google Earth Engine](#working-with-google-earth-engine)
6. [Interpretation Guide](#interpretation-guide)
7. [Troubleshooting](#troubleshooting)

---

## Overview

This analysis framework integrates high-resolution (2cm increments) and composite (20cm increments) soil core sampling data to:
- Model depth profiles of SOC and bulk density
- Quantify spatial variability across sampling locations
- Incorporate environmental covariates from remote sensing
- Generate spatially-explicit predictions with uncertainty

### Key Features
- **Modular design**: Run analyses independently or as a complete workflow
- **Multiple statistical approaches**: Mixed-effects models, GAMs, geostatistics, machine learning
- **Flexible covariate integration**: Works with/without remote sensing data
- **Comprehensive outputs**: Publication-ready figures, model diagnostics, prediction maps

---

## Installation & Setup

### Step 1: Install R and RStudio
- Download R (≥4.0): https://cran.r-project.org/
- Download RStudio: https://posit.co/downloads/

### Step 2: Prepare Your Project Directory
```
your_project/
├── 00_setup.R
├── 01_data_prep.R
├── 02_exploratory_analysis.R
├── 03_depth_modeling.R
├── 04_spatial_modeling.R
├── helper_functions.R
├── run_analysis.R
├── hr_cores.csv
├── composite_cores.csv
└── covariates/
    ├── DEM.tif
    ├── NDVI.tif
    └── ... (other rasters)
```

### Step 3: Run Setup Script
```r
# In R console:
setwd("path/to/your_project")
source("00_setup.R")
```

This will:
- Install all required R packages
- Create output directories
- Verify data files
- Generate README documentation

---

## Data Requirements

### Core Data Files

Both `hr_cores.csv` and `composite_cores.csv` must have these columns:

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| core_id | character | Unique core identifier | "HR_001", "COMP_012" |
| latitude | numeric | Latitude (decimal degrees) | 43.6532 |
| longitude | numeric | Longitude (decimal degrees) | -79.3832 |
| depth_top | numeric | Top depth of sample (cm) | 0 |
| depth_bottom | numeric | Bottom depth of sample (cm) | 2 |
| SOC | numeric | Soil organic carbon (g/kg) | 35.4 |
| bulk_density | numeric | Bulk density (g/cm³) | 1.25 |

### Example Data Structure

**High-Resolution Cores** (hr_cores.csv):
```csv
core_id,latitude,longitude,depth_top,depth_bottom,SOC,bulk_density
HR_001,43.6532,-79.3832,0,2,45.2,1.15
HR_001,43.6532,-79.3832,2,4,42.8,1.18
HR_001,43.6532,-79.3832,4,6,38.5,1.22
...
```

**Composite Cores** (composite_cores.csv):
```csv
core_id,latitude,longitude,depth_top,depth_bottom,SOC,bulk_density
COMP_001,43.6545,-79.3820,0,20,38.5,1.20
COMP_001,43.6545,-79.3820,20,40,28.3,1.35
COMP_001,43.6545,-79.3820,40,60,22.1,1.42
...
```

### Covariate Rasters (Optional)

Place GeoTIFF files in the `covariates/` folder:
- **DEM.tif**: Digital Elevation Model
- **NDVI.tif**: Normalized Difference Vegetation Index
- **EVI.tif**: Enhanced Vegetation Index
- **VV.tif**: Sentinel-1 VV backscatter
- **VH.tif**: Sentinel-1 VH backscatter
- **Red.tif, Green.tif, Blue.tif**: RGB bands

All rasters should:
- Cover your study area
- Have the same coordinate reference system
- Be in GeoTIFF format

---

## Analysis Modules

### Module 0: Setup (`00_setup.R`)
**Purpose**: Install packages and create directory structure

**Run this first!**
```r
source("00_setup.R")
```

**What it does**:
- Checks R version
- Installs required packages
- Creates output directories
- Verifies data files

---

### Module 1: Data Preparation (`01_data_prep.R`)
**Purpose**: Load, clean, and harmonize core data

```r
source("01_data_prep.R")
```

**Key Functions**:
1. **Data Quality Checks**
   - Identifies missing values
   - Flags unrealistic values
   - Checks depth consistency

2. **SOC Stock Calculation**
   - Converts SOC concentration → SOC stock (Mg/ha)
   - Formula: `SOC_stock = (SOC/1000) × BD × depth × 100`

3. **Data Harmonization**
   - Aggregates high-res data to composite resolution
   - Creates combined dataset for analysis

**Outputs**:
- `data_processed/hr_cores_clean.rds`
- `data_processed/composite_cores_clean.rds`
- `data_processed/combined_cores.rds`
- `outputs/summary_by_type.csv`

---

### Module 2: Exploratory Analysis (`02_exploratory_analysis.R`)
**Purpose**: Visualize patterns and explore variability

```r
source("02_exploratory_analysis.R")
```

**Key Visualizations**:
1. **Depth Profile Plots**
   - Mean SOC by depth
   - Comparison of high-res vs composite
   - Cumulative SOC stock profiles

2. **Spatial Distribution Maps**
   - Sampling locations
   - Total SOC stocks by location
   - Mean properties across space

3. **Distribution Analysis**
   - Histograms of SOC, bulk density
   - Boxplots by depth increment
   - Coefficient of variation analysis

4. **Correlation Matrix**
   - Relationships between variables

**Key Outputs**:
- `outputs/plots/soc_depth_profile_comparison.png`
- `outputs/plots/spatial_distribution.png`
- `outputs/plots/cv_by_depth.png`
- `outputs/summary_statistics.csv`

---

### Module 3: Depth Modeling (`03_depth_modeling.R`)
**Purpose**: Model depth patterns using statistical models

```r
source("03_depth_modeling.R")
```

**Statistical Approaches**:

1. **Exponential Decay Models**
   - Model: `SOC(z) = a × exp(-b × z) + c`
   - Fits individual cores
   - Extracts decay parameters

2. **Mixed-Effects Models**
   - Accounts for random variation among cores
   - Fixed effects: depth, core type
   - Random effects: depth slopes by core
   - Options: Linear, polynomial, spline

3. **Generalized Additive Models (GAMs)**
   - Flexible, non-parametric depth functions
   - Smooth terms for depth
   - Separate smooths by core type

**Model Comparison**:
- AIC/BIC criteria
- Cross-validation metrics
- Fitted vs. observed plots
- Residual diagnostics

**Key Outputs**:
- `outputs/models/lme_soc_best.rds`
- `outputs/models/gam_soc.rds`
- `outputs/depth_model_predictions.csv`
- `outputs/plots/gam_predictions_vs_data.png`

---

### Module 4: Spatial Modeling (`04_spatial_modeling.R`)
**Purpose**: Model spatial patterns with covariates and geostatistics

```r
source("04_spatial_modeling.R")
```

**Spatial Methods**:

1. **Covariate Extraction**
   - Extracts raster values at sampling points
   - Links environmental data to SOC measurements

2. **Random Forest Model**
   - Machine learning approach
   - Uses all covariates as predictors
   - Variable importance analysis
   - Out-of-bag validation

3. **Geostatistical Analysis**
   - Variogram modeling
   - Tests for spatial autocorrelation (Moran's I)
   - Identifies spatial structure

4. **Ordinary Kriging**
   - Spatial interpolation without covariates
   - Generates prediction maps
   - Quantifies prediction uncertainty

5. **Regression Kriging** (when covariates available)
   - Combines linear regression + kriging
   - Leverages both covariates and spatial correlation

**Key Outputs**:
- `outputs/plots/variogram.png`
- `outputs/plots/kriging_predictions.png`
- `outputs/plots/rf_variable_importance.png`
- `outputs/spatial_model_comparison.csv`

---

### Master Script (`run_analysis.R`)
**Purpose**: Run complete workflow in one go

```r
source("run_analysis.R")
```

**Configuration**: Set which modules to run
```r
RUN_SETUP <- FALSE  # Only TRUE on first run
RUN_DATA_PREP <- TRUE
RUN_EXPLORATORY <- TRUE
RUN_DEPTH_MODELING <- TRUE
RUN_SPATIAL_MODELING <- TRUE
```

**Features**:
- Error handling for each module
- Tracks execution time
- Generates summary report
- Lists all output files

---

## Working with Google Earth Engine

### Exporting Covariates from GEE

#### 1. Create GEE Script
```javascript
// Load your sampling points
var points = ee.FeatureCollection('path/to/your/points');

// Define your study area
var studyArea = points.geometry().buffer(1000);

// Load covariates
var dem = ee.Image('USGS/SRTMGL1_003');
var sentinel2 = ee.ImageCollection('COPERNICUS/S2_SR')
  .filterBounds(studyArea)
  .filterDate('2023-01-01', '2023-12-31')
  .median();

// Calculate indices
var ndvi = sentinel2.normalizedDifference(['B8', 'B4']).rename('NDVI');
var evi = sentinel2.expression(
  '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
    'NIR': sentinel2.select('B8'),
    'RED': sentinel2.select('B4'),
    'BLUE': sentinel2.select('B2')
}).rename('EVI');

// Load Sentinel-1
var sentinel1 = ee.ImageCollection('COPERNICUS/S1_GRD')
  .filterBounds(studyArea)
  .filterDate('2023-01-01', '2023-12-31')
  .filter(ee.Filter.eq('instrumentMode', 'IW'))
  .median();

// Export each covariate
Export.image.toDrive({
  image: dem,
  description: 'DEM',
  scale: 30,
  region: studyArea,
  fileFormat: 'GeoTIFF'
});

Export.image.toDrive({
  image: ndvi,
  description: 'NDVI',
  scale: 10,
  region: studyArea,
  fileFormat: 'GeoTIFF'
});

// Repeat for other covariates...
```

#### 2. Import to R
Place exported files in `covariates/` folder. Module 4 will automatically detect and use them.

---

## Interpretation Guide

### Understanding Key Outputs

#### 1. Depth Profile Models

**Exponential Parameters**:
- `a`: Surface enrichment (difference between surface and baseline)
- `b`: Decay rate (larger = faster decrease with depth)
- `c`: Baseline SOC at depth

**Interpretation**:
- High `a`, high `b`: Rapid surface accumulation
- Low `b`: SOC persists at depth
- High `c`: Elevated baseline SOC

#### 2. Spatial Patterns

**Moran's I**:
- Value: -1 (dispersed) to +1 (clustered)
- p < 0.05: Significant spatial autocorrelation
- Positive I: Similar values cluster together

**Variogram**:
- **Nugget**: Measurement error + micro-scale variation
- **Sill**: Total spatial variance
- **Range**: Distance where spatial correlation disappears

#### 3. Model Performance

**Metrics**:
- **R²**: Proportion of variance explained (higher = better)
- **RMSE**: Average prediction error in original units
- **MAE**: Mean absolute error (less sensitive to outliers)

**Good Performance**:
- R² > 0.7 for depth models
- R² > 0.5 for spatial models
- RMSE < 20% of mean value

#### 4. Variable Importance (Random Forest)

**Interpretation**:
- Higher % Increase in MSE = more important
- Top variables drive spatial patterns
- Can inform targeted sampling

---

## Troubleshooting

### Common Issues

#### 1. "Package not found"
**Solution**:
```r
install.packages("package_name")
```

#### 2. "Cannot open file"
**Check**:
- File is in working directory: `list.files()`
- Correct file name and extension
- File not open in another program

#### 3. "Variogram model doesn't fit"
**Solutions**:
- Check for outliers in data
- Reduce cutoff distance
- Try different variogram model types

#### 4. "Memory allocation error"
**Solutions**:
- Close other programs
- Use smaller raster resolutions
- Process subsets of data

#### 5. Missing covariate files
**Module 4 will work without covariates** - it will:
- Skip Random Forest and Regression Kriging
- Still perform Ordinary Kriging
- Note missing files in output

### Getting Help

**Check logs**:
- Console messages indicate progress
- Warnings (orange) vs. Errors (red)
- Error messages show which line failed

**Verify data format**:
```r
# Check your data structure
head(hr_cores)
str(hr_cores)
summary(hr_cores)
```

**Test with subset**:
```r
# Test with first 5 cores
test_data <- hr_cores[hr_cores$core_id %in% unique(hr_cores$core_id)[1:5], ]
```

---

## Tips for Success

### 1. Data Quality
- **Before analysis**: Plot raw depth profiles to check for errors
- **Remove outliers**: Values that are physically impossible
- **Check coordinates**: Must be in decimal degrees
- **Consistent units**: SOC in g/kg, BD in g/cm³

### 2. Computational Efficiency
- **Start small**: Test with subset of data
- **Save intermediate results**: Allows resuming analysis
- **Use parallel processing** (advanced):
```r
library(parallel)
cl <- makeCluster(detectCores() - 1)
```

### 3. Publication-Ready Figures
- All plots saved at 300 dpi
- Customize colors, labels in module scripts
- Export as PDF for vector graphics:
```r
ggsave("figure.pdf", plot, width = 8, height = 6)
```

### 4. Reproducibility
- Set random seed for Random Forest:
```r
set.seed(123)
```
- Document R and package versions:
```r
sessionInfo()
```
- Keep a lab notebook of analysis decisions

---

## Additional Resources

### Statistical Methods
- **Mixed-effects models**: Pinheiro & Bates (2000) *Mixed-Effects Models in S and S-PLUS*
- **Geostatistics**: Webster & Oliver (2007) *Geostatistics for Environmental Scientists*
- **GAMs**: Wood (2017) *Generalized Additive Models*

### R Resources
- **R for Data Science**: https://r4ds.had.co.nz/
- **Spatial Data Science**: https://r-spatial.org/book/
- **Stack Overflow**: Tag questions with [r] and [geostatistics]

### Google Earth Engine
- **Documentation**: https://developers.google.com/earth-engine
- **Code Editor**: https://code.earthengine.google.com/

---

## Citation

If you use this framework in your research, please cite:
- The statistical methods you used (references in module scripts)
- R packages (run `citation("package_name")` for format)
- Google Earth Engine datasets

---

## Contact & Support

For questions about this analysis framework:
1. Check this guide and module documentation
2. Review error messages and troubleshooting section
3. Examine example outputs to verify expected behavior

**Good luck with your analysis!** 🎉

---

*Last updated: November 2025*
*Version: 1.0*
