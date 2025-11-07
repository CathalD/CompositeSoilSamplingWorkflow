# ============================================================================
# Module 04: Spatial Modeling with Covariates - IMPROVED VERSION
# VERSION: 2.0 - Production-ready with robust spatial analysis
# ============================================================================
# Purpose: Spatial analysis, kriging, and Random Forest modeling with covariates
# Inputs: Processed data from Module 01, optional GEE covariate rasters
# Outputs: Spatial predictions, kriging maps, RF models, variograms
# Author: [Your name]
# Date: Modified on 2024
# ============================================================================

# ============================================================================
# SETUP AND CONFIGURATION
# ============================================================================

# Initialize logging
log_file <- file.path("logs", paste0("spatial_modeling_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting Module 04: Spatial Modeling")

# Set random seed for reproducibility
set.seed(42)
log_message("Random seed set to 42 for reproducibility")

# ============================================================================
# LOAD PACKAGES WITH ERROR HANDLING
# ============================================================================

load_required_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    log_message(sprintf("Package '%s' not available", pkg), "WARNING")
    return(FALSE)
  }
  log_message(sprintf("Loaded package: %s", pkg))
  return(TRUE)
}

# Load core packages (required)
required_packages <- c("dplyr", "ggplot2")
for (pkg in required_packages) {
  if (!load_required_package(pkg)) {
    stop(sprintf("Required package '%s' must be installed", pkg))
  }
}

# Load spatial packages (attempt but continue if not available)
spatial_packages <- c("sf", "raster", "terra", "gstat", "sp", "spdep", "randomForest", "caret")
spatial_available <- setNames(rep(FALSE, length(spatial_packages)), spatial_packages)

for (pkg in spatial_packages) {
  spatial_available[pkg] <- load_required_package(pkg)
}

# Report spatial package availability
cat("\nSpatial Package Availability:\n")
cat("==============================\n")
for (pkg in names(spatial_available)) {
  status <- ifelse(spatial_available[pkg], "✓ Available", "✗ Not Available")
  cat(sprintf("%-15s: %s\n", pkg, status))
}
cat("\n")

if (!any(spatial_available[c("sf", "sp")])) {
  stop("At least one spatial package (sf or sp) is required for spatial modeling")
}

# Source helper functions
tryCatch({
  if (file.exists("helper_functions_improved.R")) {
    source("helper_functions_improved.R")
  } else if (file.exists("helper_functions.R")) {
    source("helper_functions.R")
    log_message("Using original helper_functions.R", "WARNING")
  }
}, error = function(e) {
  log_message(sprintf("Error loading helper functions: %s", e$message), "WARNING")
})

# ============================================================================
# 1. LOAD PROCESSED DATA
# ============================================================================

log_message("Loading processed data")

# Safe data loading function
load_data_safe <- function(filename) {
  filepath <- file.path("data_processed", filename)
  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s. Please run Module 01 first.", filepath))
  }
  
  tryCatch({
    data <- readRDS(filepath)
    log_message(sprintf("Loaded %s: %d rows", filename, nrow(data)))
    return(data)
  }, error = function(e) {
    stop(sprintf("Error loading %s: %s", filename, e$message))
  })
}

# Load combined dataset
combined_cores <- load_data_safe("combined_cores.rds")

# Validate data
if (nrow(combined_cores) < 3) {
  stop("Insufficient data for spatial modeling (need at least 3 locations)")
}

cat("Data loaded successfully!\n")
cat("Total samples:", nrow(combined_cores), "\n")
cat("Unique locations:", length(unique(combined_cores$core_id)), "\n\n")

# ============================================================================
# 2. PREPARE SPATIAL DATA
# ============================================================================

log_message("Preparing spatial data for analysis")

# Aggregate by core location (sum SOC stocks to total per location)
core_spatial <- combined_cores %>%
  group_by(core_id, latitude, longitude, core_type) %>%
  summarise(
    total_stock = sum(soc_stock, na.rm = TRUE),
    mean_soc = mean(soc, na.rm = TRUE),
    mean_bd = mean(bulk_density, na.rm = TRUE),
    max_depth = max(depth_bottom, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(total_stock) & is.finite(total_stock)) %>%
  filter(!is.na(latitude) & !is.na(longitude))

log_message(sprintf("Created spatial dataset with %d unique locations", nrow(core_spatial)))

if (nrow(core_spatial) < 3) {
  stop("Insufficient spatial locations for modeling (need at least 3)")
}

# ============================================================================
# 3. SPATIAL EXTENT AND DISTANCES
# ============================================================================

log_message("Calculating spatial statistics")

# Calculate pairwise distances between points
coords_matrix <- as.matrix(core_spatial[, c("longitude", "latitude")])
distances <- as.vector(dist(coords_matrix))

# Spatial statistics
spatial_stats <- list(
  lon_range = range(core_spatial$longitude),
  lat_range = range(core_spatial$latitude),
  mean_distance = mean(distances),
  max_distance = max(distances),
  n_locations = nrow(core_spatial)
)

cat("\nSpatial extent:\n")
cat("  Longitude range:", spatial_stats$lon_range, "\n")
cat("  Latitude range:", spatial_stats$lat_range, "\n")
cat("  Mean distance between points:", round(spatial_stats$mean_distance, 4), "degrees\n")
cat("  Max distance:", round(spatial_stats$max_distance, 4), "degrees\n\n")

# Test for spatial autocorrelation if spdep is available
if (spatial_available["spdep"]) {
  log_message("Testing for spatial autocorrelation")
  
  tryCatch({
    # Create spatial weights (k-nearest neighbors)
    k <- min(5, nrow(core_spatial) - 1)
    coords_matrix <- as.matrix(core_spatial[, c("longitude", "latitude")])
    knn_nb <- knn2nb(knearneigh(coords_matrix, k = k))
    knn_weights <- nb2listw(knn_nb, style = "W")
    
    # Moran's I test
    moran_result <- moran.test(core_spatial$total_stock, knn_weights)
    
    cat("Moran's I test for spatial autocorrelation:\n")
    cat("  Moran's I statistic:", round(moran_result$estimate[1], 4), "\n")
    cat("  p-value:", format.pval(moran_result$p.value), "\n")
    
    if (moran_result$p.value < 0.05) {
      cat("  → Significant spatial autocorrelation detected!\n\n")
      log_message("Significant spatial autocorrelation detected")
    } else {
      cat("  → No significant spatial autocorrelation detected.\n\n")
      log_message("No significant spatial autocorrelation")
    }
    
  }, error = function(e) {
    log_message(sprintf("Error in Moran's I test: %s", e$message), "WARNING")
  })
} else {
  log_message("spdep not available - skipping autocorrelation test", "WARNING")
}

# ============================================================================
# 4. VARIOGRAM ANALYSIS (if gstat available)
# ============================================================================

if (spatial_available["gstat"]) {
  log_message("Performing variogram analysis")
  
  cat("Performing variogram analysis...\n")
  
  tryCatch({
    # Create spatial object
    core_spatial_sp <- core_spatial
    coordinates(core_spatial_sp) <- ~longitude+latitude
    proj4string(core_spatial_sp) <- CRS("+proj=longlat +datum=WGS84")
    
    # Calculate empirical variogram
    v_empirical <- variogram(total_stock ~ 1, core_spatial_sp)
    
    # Fit variogram models
    variogram_models <- list()
    model_types <- c("Sph", "Exp", "Gau", "Mat")
    
    for (model_type in model_types) {
      tryCatch({
        v_fit <- fit.variogram(v_empirical, vgm(model_type))
        variogram_models[[model_type]] <- v_fit
        log_message(sprintf("Fitted %s variogram model", model_type))
      }, error = function(e) {
        log_message(sprintf("Failed to fit %s model: %s", model_type, e$message), "WARNING")
      })
    }
    
    # Select best model based on SSE
    if (length(variogram_models) > 0) {
      sse_values <- sapply(variogram_models, function(m) {
        sum((v_empirical$gamma - variogramLine(m, dist_vector = v_empirical$dist)$gamma)^2)
      })
      
      best_vgm_name <- names(which.min(sse_values))
      best_vgm <- variogram_models[[best_vgm_name]]
      
      cat("Best variogram model:", best_vgm_name, "\n")
      cat("Model parameters:\n")
      print(best_vgm)
      
      # Save variogram
      saveRDS(list(empirical = v_empirical, fitted = best_vgm), 
              "outputs/models/variogram_model.rds")
      log_message("Saved variogram model")
      
      # Plot variogram
      tryCatch({
        png("outputs/plots/variogram.png", width = 800, height = 600, res = 100)
        plot(v_empirical, best_vgm, 
             main = paste("Variogram Model:", best_vgm_name),
             xlab = "Distance", ylab = "Semivariance")
        dev.off()
        log_message("Saved variogram plot")
      }, error = function(e) {
        log_message(sprintf("Error plotting variogram: %s", e$message), "WARNING")
      })
      
      # Perform ordinary kriging if enough points
      if (nrow(core_spatial) >= 10) {
        log_message("Performing ordinary kriging")
        
        tryCatch({
          # Create prediction grid
          lon_seq <- seq(min(core_spatial$longitude), max(core_spatial$longitude), length.out = 50)
          lat_seq <- seq(min(core_spatial$latitude), max(core_spatial$latitude), length.out = 50)
          pred_grid <- expand.grid(longitude = lon_seq, latitude = lat_seq)
          coordinates(pred_grid) <- ~longitude+latitude
          proj4string(pred_grid) <- CRS("+proj=longlat +datum=WGS84")
          
          # Perform kriging
          krige_result <- krige(total_stock ~ 1, core_spatial_sp, pred_grid, model = best_vgm)
          
          # Convert to data frame for plotting
          krige_df <- as.data.frame(krige_result)
          names(krige_df)[3] <- "prediction"
          names(krige_df)[4] <- "variance"
          
          # Plot kriging predictions
          p_krige <- ggplot() +
            geom_tile(data = krige_df, aes(x = longitude, y = latitude, fill = prediction)) +
            geom_point(data = core_spatial, aes(x = longitude, y = latitude), 
                      shape = 21, size = 3, fill = "white", color = "black") +
            scale_fill_viridis_c(name = "SOC Stock\n(Mg/ha)") +
            labs(
              title = "Kriging Predictions of Total SOC Stock",
              subtitle = paste("Model:", best_vgm_name),
              x = "Longitude",
              y = "Latitude"
            ) +
            theme_minimal() +
            coord_equal()
          
          ggsave("outputs/plots/kriging_predictions.png", p_krige,
                 width = 10, height = 8, dpi = 300)
          log_message("Saved kriging predictions plot")
          
          # Save kriging results
          saveRDS(krige_df, "outputs/kriging_predictions.rds")
          write.csv(krige_df, "outputs/kriging_predictions.csv", row.names = FALSE)
          
        }, error = function(e) {
          log_message(sprintf("Error in kriging: %s", e$message), "WARNING")
        })
      } else {
        log_message("Not enough points for kriging (need at least 10)", "WARNING")
      }
      
    } else {
      log_message("No variogram models could be fitted", "WARNING")
    }
    
  }, error = function(e) {
    log_message(sprintf("Error in variogram analysis: %s", e$message), "WARNING")
  })
} else {
  log_message("gstat not available - skipping variogram analysis", "WARNING")
  cat("Variogram analysis skipped (gstat package not available)\n\n")
}

# ============================================================================
# 5. RANDOM FOREST MODELING WITH COVARIATES (if available)
# ============================================================================

if (spatial_available["randomForest"]) {
  log_message("Checking for covariate data")
  
  # Check if covariates directory exists
  if (dir.exists("covariates")) {
    covariate_files <- list.files("covariates", pattern = "\\.tif$", full.names = TRUE)
    
    if (length(covariate_files) > 0) {
      log_message(sprintf("Found %d covariate raster files", length(covariate_files)))
      cat("\nCovariate files found:\n")
      for (f in basename(covariate_files)) {
        cat("  -", f, "\n")
      }
      cat("\n")
      
      # Extract covariate values at core locations
      if (spatial_available["raster"] || spatial_available["terra"]) {
        log_message("Extracting covariate values at core locations")
        
        tryCatch({
          # Use terra if available, otherwise raster
          if (spatial_available["terra"]) {
            covariates_list <- lapply(covariate_files, terra::rast)
            covariates_stack <- terra::rast(covariates_list)
            
            # Extract values
            coords_sf <- st_as_sf(core_spatial, coords = c("longitude", "latitude"), crs = 4326)
            covariate_values <- terra::extract(covariates_stack, coords_sf)
            
          } else {
            covariates_list <- lapply(covariate_files, raster::raster)
            covariates_stack <- raster::stack(covariates_list)
            
            # Extract values
            coords_matrix <- as.matrix(core_spatial[, c("longitude", "latitude")])
            covariate_values <- raster::extract(covariates_stack, coords_matrix)
          }
          
          # Combine with spatial data
          rf_data <- cbind(core_spatial, covariate_values)
          rf_data <- rf_data[complete.cases(rf_data), ]  # Remove NA values
          
          log_message(sprintf("Extracted covariates for %d locations", nrow(rf_data)))
          
          if (nrow(rf_data) >= 10) {
            # Fit Random Forest model
            log_message("Fitting Random Forest model")
            
            # Prepare predictors
            predictor_cols <- names(covariate_values)
            formula_rf <- as.formula(paste("total_stock ~", paste(predictor_cols, collapse = " + ")))
            
            # Fit model
            rf_model <- randomForest(
              formula_rf,
              data = rf_data,
              ntree = 500,
              importance = TRUE,
              na.action = na.omit
            )
            
            # Save model
            saveRDS(rf_model, "outputs/models/random_forest_model.rds")
            log_message("Saved Random Forest model")
            
            # Model performance
            cat("\nRandom Forest Model Performance:\n")
            cat("  R-squared:", round(rf_model$rsq[500], 3), "\n")
            cat("  RMSE:", round(sqrt(mean(rf_model$mse)), 2), "Mg/ha\n\n")
            
            # Variable importance
            importance_df <- as.data.frame(importance(rf_model))
            importance_df$variable <- rownames(importance_df)
            importance_df <- importance_df[order(-importance_df$`%IncMSE`), ]
            
            write.csv(importance_df, "outputs/rf_variable_importance.csv", row.names = FALSE)
            
            # Plot variable importance
            p_importance <- ggplot(importance_df, 
                                  aes(x = reorder(variable, `%IncMSE`), y = `%IncMSE`)) +
              geom_col(fill = "steelblue") +
              coord_flip() +
              labs(
                title = "Random Forest Variable Importance",
                x = "Variable",
                y = "% Increase in MSE"
              ) +
              theme_minimal()
            
            ggsave("outputs/plots/rf_variable_importance.png", p_importance,
                   width = 8, height = 6, dpi = 300)
            log_message("Saved variable importance plot")
            
          } else {
            log_message("Not enough complete cases for Random Forest modeling", "WARNING")
          }
          
        }, error = function(e) {
          log_message(sprintf("Error in Random Forest modeling: %s", e$message), "WARNING")
        })
        
      } else {
        log_message("No raster package available for covariate extraction", "WARNING")
      }
      
    } else {
      log_message("No covariate raster files found in covariates/ directory", "WARNING")
      cat("No covariate raster files found. Place .tif files in the covariates/ directory.\n\n")
    }
  } else {
    log_message("Covariates directory not found", "WARNING")
    cat("Covariates directory not found. Skipping Random Forest modeling.\n")
    cat("Create a 'covariates/' directory and add .tif raster files to enable this analysis.\n\n")
  }
} else {
  log_message("randomForest package not available", "WARNING")
  cat("Random Forest modeling skipped (randomForest package not available)\n\n")
}

# ============================================================================
# 6. SPATIAL VISUALIZATION
# ============================================================================

log_message("Creating spatial visualization")

# Create spatial map of total SOC stock
p_spatial_map <- ggplot(core_spatial, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = total_stock, color = total_stock), alpha = 0.7) +
  scale_color_viridis_c(name = "Total SOC Stock\n(Mg/ha)") +
  scale_size_continuous(name = "Total SOC Stock\n(Mg/ha)", range = c(3, 10)) +
  labs(
    title = "Spatial Distribution of Total SOC Stock",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  coord_equal()

ggsave("outputs/plots/spatial_soc_map.png", p_spatial_map,
       width = 10, height = 8, dpi = 300)
log_message("Saved spatial SOC map")

# ============================================================================
# 7. SUMMARY AND EXPORT
# ============================================================================

log_message("Creating summary outputs")

# Spatial summary statistics
spatial_summary <- data.frame(
  metric = c("Number of locations", "Mean SOC stock", "SD SOC stock", 
             "Min SOC stock", "Max SOC stock", "Mean distance (degrees)"),
  value = c(
    nrow(core_spatial),
    round(mean(core_spatial$total_stock), 2),
    round(sd(core_spatial$total_stock), 2),
    round(min(core_spatial$total_stock), 2),
    round(max(core_spatial$total_stock), 2),
    round(spatial_stats$mean_distance, 4)
  )
)

write.csv(spatial_summary, "outputs/spatial_summary_statistics.csv", row.names = FALSE)
log_message("Saved spatial summary statistics")

# Export spatial data
if (spatial_available["sf"]) {
  tryCatch({
    core_spatial_sf <- st_as_sf(core_spatial, 
                                coords = c("longitude", "latitude"), 
                                crs = 4326)
    st_write(core_spatial_sf, "outputs/core_locations.gpkg", delete_dsn = TRUE, quiet = TRUE)
    log_message("Exported spatial data to GeoPackage")
  }, error = function(e) {
    log_message(sprintf("Error exporting spatial data: %s", e$message), "WARNING")
  })
}

# ============================================================================
# 8. COMPLETION
# ============================================================================

cat("\n========================================\n")
cat("SPATIAL MODELING COMPLETE!\n")
cat("========================================\n\n")

cat("Outputs saved:\n")
cat("  - Spatial summary statistics\n")
cat("  - Spatial SOC map\n")
if (spatial_available["gstat"] && exists("best_vgm")) {
  cat("  - Variogram analysis and kriging predictions\n")
}
if (spatial_available["randomForest"] && exists("rf_model")) {
  cat("  - Random Forest model and variable importance\n")
}
cat("\n")

log_message("Module 04 complete")
cat("Check the outputs/ directory for all results.\n\n")

# Session info for reproducibility
cat("Session information:\n")
print(sessionInfo())
