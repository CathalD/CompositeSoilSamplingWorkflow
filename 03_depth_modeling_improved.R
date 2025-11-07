# ============================================================================
# Module 03: Depth Profile Modeling - IMPROVED VERSION
# VERSION: 2.0 - Production-ready with robust model fitting
# ============================================================================
# Purpose: Model SOC and bulk density depth patterns using statistical models
# Inputs: Processed data from Module 01
# Outputs: Fitted models, predictions, and diagnostic plots
# Author: [Your name]
# Date: Modified on 2024
# ============================================================================

# ============================================================================
# SETUP AND CONFIGURATION
# ============================================================================

# Initialize logging
log_file <- file.path("logs", paste0("depth_modeling_", Sys.Date(), ".log"))
if (!dir.exists("logs")) dir.create("logs")

log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(log_entry, "\n")
  cat(log_entry, "\n", file = log_file, append = TRUE)
}

log_message("Starting Module 03: Depth Profile Modeling")

# Set random seed for reproducibility
set.seed(42)
log_message("Random seed set to 42 for reproducibility")

# ============================================================================
# LOAD PACKAGES WITH ERROR HANDLING
# ============================================================================

load_required_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Install with: install.packages('%s')", pkg, pkg))
  }
  log_message(sprintf("Loaded package: %s", pkg))
}

# Load required packages
required_packages <- c("dplyr", "ggplot2", "nlme", "splines", "mgcv")

for (pkg in required_packages) {
  tryCatch({
    load_required_package(pkg)
  }, error = function(e) {
    log_message(sprintf("Failed to load %s: %s", pkg, e$message), "ERROR")
    stop(sprintf("Cannot proceed without package: %s", pkg))
  })
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
  log_message(sprintf("Error loading helper functions: %s", e$message), "ERROR")
  stop("Cannot proceed without helper functions")
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

# Load datasets
hr_cores <- load_data_safe("hr_cores_clean.rds")
composite_cores <- load_data_safe("composite_cores_clean.rds")
combined_cores <- load_data_safe("combined_cores.rds")

# Validate data
if (nrow(combined_cores) < 10) {
  stop("Insufficient data for modeling (need at least 10 samples)")
}

cat("Data loaded successfully!\n")
cat("Samples available for modeling:", nrow(combined_cores), "\n\n")

# ============================================================================
# 2. PREPARE DATA FOR MODELING
# ============================================================================

log_message("Preparing data for modeling")

# Remove any infinite or missing values
modeling_data <- combined_cores %>%
  filter(
    is.finite(soc) & !is.na(soc),
    is.finite(bulk_density) & !is.na(bulk_density),
    is.finite(depth_mid) & !is.na(depth_mid),
    soc > 0,  # Ensure positive values for log transformations
    bulk_density > 0
  ) %>%
  mutate(
    log_soc = log(soc),
    sqrt_depth = sqrt(depth_mid),
    depth_scaled = depth_mid / 100  # Scale depth to 0-1
  )

log_message(sprintf("Prepared %d samples for modeling (removed %d samples with invalid values)",
                    nrow(modeling_data), nrow(combined_cores) - nrow(modeling_data)))

# ============================================================================
# 3. EXPONENTIAL DECAY MODELS
# ============================================================================

log_message("Fitting exponential decay models")

# Enhanced exponential model fitting with better error handling
fit_exp_model_robust <- function(data, max_iter = 200) {
  
  # Multiple starting value strategies
  start_strategies <- list(
    # Strategy 1: Based on data range
    list(a = max(data$soc) - min(data$soc), b = 0.02, c = min(data$soc)),
    # Strategy 2: Using quantiles
    list(a = quantile(data$soc, 0.9) - quantile(data$soc, 0.1), 
         b = 0.03, c = quantile(data$soc, 0.1)),
    # Strategy 3: Log-linear approximation
    list(a = exp(coef(lm(log(soc) ~ depth_mid, data = data))[1]), 
         b = -coef(lm(log(soc) ~ depth_mid, data = data))[2], 
         c = 0)
  )
  
  best_model <- NULL
  best_aic <- Inf
  
  for (i in seq_along(start_strategies)) {
    tryCatch({
      model <- nls(
        soc ~ a * exp(-b * depth_mid) + c,
        data = data,
        start = start_strategies[[i]],
        control = nls.control(maxiter = max_iter, warnOnly = TRUE),
        algorithm = "port",
        lower = c(a = 0, b = 0, c = 0),
        upper = c(a = 500, b = 1, c = 200)
      )
      
      current_aic <- AIC(model)
      if (current_aic < best_aic) {
        best_model <- model
        best_aic <- current_aic
      }
      
    }, error = function(e) {
      # Try next strategy
    }, warning = function(w) {
      # Continue with warning
    })
  }
  
  if (!is.null(best_model)) {
    # Calculate R-squared
    ss_res <- sum(residuals(best_model)^2)
    ss_tot <- sum((data$soc - mean(data$soc))^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Extract parameters
    params <- coef(best_model)
    
    return(list(
      model = best_model,
      a = params["a"],
      b = params["b"],
      c = params["c"],
      r_squared = r_squared,
      aic = best_aic,
      rmse = sqrt(mean(residuals(best_model)^2))
    ))
  } else {
    return(NULL)
  }
}

# Fit exponential models by core
exp_models_by_core <- list()
successful_cores <- character()

for (core in unique(hr_cores$core_id)) {
  core_data <- hr_cores %>% filter(core_id == core)
  
  if (nrow(core_data) >= 5) {  # Need at least 5 points
    result <- fit_exp_model_robust(core_data)
    if (!is.null(result)) {
      exp_models_by_core[[core]] <- result
      successful_cores <- c(successful_cores, core)
      log_message(sprintf("Fitted exponential model for core %s (R² = %.3f)", 
                          core, result$r_squared))
    }
  }
}

# Summary of exponential models
if (length(exp_models_by_core) > 0) {
  exp_params_df <- data.frame(
    core_id = names(exp_models_by_core),
    a = sapply(exp_models_by_core, function(x) x$a),
    b = sapply(exp_models_by_core, function(x) x$b),
    c = sapply(exp_models_by_core, function(x) x$c),
    r_squared = sapply(exp_models_by_core, function(x) x$r_squared),
    rmse = sapply(exp_models_by_core, function(x) x$rmse)
  )
  
  # Save parameters
  write.csv(exp_params_df, "outputs/exponential_model_parameters.csv", row.names = FALSE)
  
  cat("\nExponential Model Summary:\n")
  cat("Successful fits:", length(exp_models_by_core), "out of", 
      length(unique(hr_cores$core_id)), "cores\n")
  cat("Mean R²:", round(mean(exp_params_df$r_squared), 3), "\n")
  cat("Mean RMSE:", round(mean(exp_params_df$rmse), 2), "\n\n")
}

# ============================================================================
# 4. MIXED-EFFECTS MODELS
# ============================================================================

log_message("Fitting mixed-effects models")

# Create model directory
if (!dir.exists("outputs/models")) {
  dir.create("outputs/models", recursive = TRUE)
}

# Function to fit and evaluate mixed models
fit_mixed_model <- function(formula, data, model_name) {
  log_message(sprintf("Fitting mixed model: %s", model_name))
  
  tryCatch({
    model <- lme(
      formula,
      random = ~ 1 | core_id,
      data = data,
      method = "REML",
      control = lmeControl(
        maxIter = 200,
        msMaxIter = 200,
        tolerance = 1e-6,
        opt = "optim",
        optimMethod = "BFGS"
      )
    )
    
    # Model diagnostics
    aic_val <- AIC(model)
    bic_val <- BIC(model)
    
    # Calculate pseudo R-squared (Nakagawa & Schielzeth method)
    var_fixed <- var(as.numeric(fitted(model, level = 0)))
    var_random <- as.numeric(VarCorr(model)[1, 1])
    var_resid <- as.numeric(VarCorr(model)[2, 1])
    r2_marginal <- var_fixed / (var_fixed + var_random + var_resid)
    r2_conditional <- (var_fixed + var_random) / (var_fixed + var_random + var_resid)
    
    # Save model
    saveRDS(model, file.path("outputs/models", paste0(model_name, ".rds")))
    
    log_message(sprintf("%s - AIC: %.1f, BIC: %.1f, R²m: %.3f, R²c: %.3f",
                        model_name, aic_val, bic_val, r2_marginal, r2_conditional))
    
    return(list(
      model = model,
      name = model_name,
      aic = aic_val,
      bic = bic_val,
      r2_marginal = r2_marginal,
      r2_conditional = r2_conditional
    ))
    
  }, error = function(e) {
    log_message(sprintf("Failed to fit %s: %s", model_name, e$message), "WARNING")
    return(NULL)
  })
}

# Fit various mixed-effects models
mixed_models <- list()

# Model 1: Linear depth effect
mixed_models$linear <- fit_mixed_model(
  soc ~ depth_mid * core_type,
  modeling_data,
  "lme_soc_linear"
)

# Model 2: Polynomial depth effect
mixed_models$poly2 <- fit_mixed_model(
  soc ~ poly(depth_mid, 2) * core_type,
  modeling_data,
  "lme_soc_poly2"
)

# Model 3: Natural spline
library(splines)
mixed_models$spline <- fit_mixed_model(
  soc ~ ns(depth_mid, df = 3) * core_type,
  modeling_data,
  "lme_soc_spline"
)

# Model 4: Log-transformed SOC
mixed_models$log <- fit_mixed_model(
  log_soc ~ depth_mid * core_type,
  modeling_data,
  "lme_log_soc"
)

# Model 5: Square root depth
mixed_models$sqrt <- fit_mixed_model(
  soc ~ sqrt_depth * core_type,
  modeling_data,
  "lme_soc_sqrt_depth"
)

# Remove NULL models
mixed_models <- mixed_models[!sapply(mixed_models, is.null)]

# ============================================================================
# 5. GENERALIZED ADDITIVE MODELS (GAMs)
# ============================================================================

log_message("Fitting GAM models")

# Fit GAMs with different smoothers
fit_gam_models <- function(data) {
  gam_models <- list()
  
  # GAM 1: Basic smooth
  tryCatch({
    gam1 <- gam(
      soc ~ s(depth_mid, k = 10) + core_type,
      data = data,
      method = "REML"
    )
    gam_models$basic <- list(
      model = gam1,
      name = "GAM_basic",
      aic = AIC(gam1),
      r2 = summary(gam1)$r.sq,
      dev_explained = summary(gam1)$dev.expl
    )
    saveRDS(gam1, "outputs/models/gam_basic.rds")
    log_message(sprintf("GAM basic - AIC: %.1f, R²: %.3f", AIC(gam1), summary(gam1)$r.sq))
  }, error = function(e) {
    log_message(sprintf("Failed to fit basic GAM: %s", e$message), "WARNING")
  })
  
  # GAM 2: Interaction with core type
  tryCatch({
    gam2 <- gam(
      soc ~ s(depth_mid, by = core_type, k = 10) + core_type,
      data = data,
      method = "REML"
    )
    gam_models$interaction <- list(
      model = gam2,
      name = "GAM_interaction",
      aic = AIC(gam2),
      r2 = summary(gam2)$r.sq,
      dev_explained = summary(gam2)$dev.expl
    )
    saveRDS(gam2, "outputs/models/gam_interaction.rds")
    log_message(sprintf("GAM interaction - AIC: %.1f, R²: %.3f", AIC(gam2), summary(gam2)$r.sq))
  }, error = function(e) {
    log_message(sprintf("Failed to fit interaction GAM: %s", e$message), "WARNING")
  })
  
  # GAM 3: With random effects
  tryCatch({
    gam3 <- gam(
      soc ~ s(depth_mid, k = 10) + core_type + s(core_id, bs = "re"),
      data = data,
      method = "REML"
    )
    gam_models$random <- list(
      model = gam3,
      name = "GAM_random",
      aic = AIC(gam3),
      r2 = summary(gam3)$r.sq,
      dev_explained = summary(gam3)$dev.expl
    )
    saveRDS(gam3, "outputs/models/gam_random.rds")
    log_message(sprintf("GAM random - AIC: %.1f, R²: %.3f", AIC(gam3), summary(gam3)$r.sq))
  }, error = function(e) {
    log_message(sprintf("Failed to fit random effects GAM: %s", e$message), "WARNING")
  })
  
  return(gam_models)
}

gam_models <- fit_gam_models(modeling_data)

# ============================================================================
# 6. MODEL COMPARISON AND SELECTION
# ============================================================================

log_message("Comparing models")

# Compile model comparison
model_comparison <- data.frame()

# Add mixed models
if (length(mixed_models) > 0) {
  mixed_comp <- do.call(rbind, lapply(mixed_models, function(m) {
    data.frame(
      model_type = "Mixed Effects",
      model_name = m$name,
      aic = m$aic,
      bic = m$bic,
      r2 = m$r2_marginal,
      stringsAsFactors = FALSE
    )
  }))
  model_comparison <- rbind(model_comparison, mixed_comp)
}

# Add GAM models
if (length(gam_models) > 0) {
  gam_comp <- do.call(rbind, lapply(gam_models, function(m) {
    data.frame(
      model_type = "GAM",
      model_name = m$name,
      aic = m$aic,
      bic = NA,  # BIC not standard for GAMs
      r2 = m$r2,
      stringsAsFactors = FALSE
    )
  }))
  model_comparison <- rbind(model_comparison, gam_comp)
}

# Sort by AIC
model_comparison <- model_comparison %>%
  arrange(aic) %>%
  mutate(delta_aic = aic - min(aic))

# Save comparison
write.csv(model_comparison, "outputs/model_comparison.csv", row.names = FALSE)

cat("\nModel Comparison:\n")
print(model_comparison)

# Select best model
best_model_name <- model_comparison$model_name[1]
log_message(sprintf("Best model (lowest AIC): %s", best_model_name))

# ============================================================================
# 7. PREDICTIONS AT STANDARD DEPTHS
# ============================================================================

log_message("Generating predictions at standard depths")

# Define standard depths
standard_depths <- seq(0, 100, 5)
prediction_grid <- expand.grid(
  depth_mid = standard_depths,
  core_type = unique(modeling_data$core_type),
  stringsAsFactors = FALSE
) %>%
  mutate(
    sqrt_depth = sqrt(depth_mid),
    depth_scaled = depth_mid / 100,
    core_id = modeling_data$core_id[1]  # Use first core for random effect
  )

# Generate predictions from best model
predictions <- data.frame()

# Find and use best model for predictions
tryCatch({
  if (grepl("lme", best_model_name)) {
    # Mixed model predictions
    best_mixed <- mixed_models[[which(sapply(mixed_models, function(x) x$name == best_model_name))]]
    if (!is.null(best_mixed)) {
      # Use tryCatch for predict to handle formula issues
      pred_values <- tryCatch({
        predict(best_mixed$model, newdata = prediction_grid, level = 0)
      }, error = function(e) {
        log_message(sprintf("Standard predict failed: %s. Trying alternative method.", e$message), "WARNING")
        # Alternative: Generate predictions manually using fixef
        # This works even when predict() fails
        fixed_effects <- fixef(best_mixed$model)
        # For simple linear model, calculate manually
        if (best_model_name == "lme_soc_linear") {
          pred <- fixed_effects[1] + 
            fixed_effects[2] * prediction_grid$depth_mid +
            ifelse(prediction_grid$core_type == "composite", 
                   fixed_effects[3] + fixed_effects[5] * prediction_grid$depth_mid, 0)
          return(pred)
        } else {
          # For complex models, return NA and skip predictions
          log_message("Cannot generate manual predictions for complex model formula", "WARNING")
          return(rep(NA, nrow(prediction_grid)))
        }
      })
      
      predictions <- prediction_grid %>%
        mutate(
          predicted_soc = pred_values,
          model = best_model_name
        ) %>%
        filter(!is.na(predicted_soc))  # Remove NAs if prediction failed
    }
  } else if (grepl("GAM", best_model_name)) {
    # GAM predictions
    best_gam <- gam_models[[which(sapply(gam_models, function(x) x$name == best_model_name))]]
    if (!is.null(best_gam)) {
      pred_values <- predict(best_gam$model, newdata = prediction_grid, type = "response")
      predictions <- prediction_grid %>%
        mutate(
          predicted_soc = pred_values,
          model = best_model_name
        )
    }
  }
}, error = function(e) {
  log_message(sprintf("Error generating predictions: %s", e$message), "ERROR")
  log_message("Continuing without predictions - check model diagnostics", "WARNING")
})

if (nrow(predictions) > 0) {
  # Save predictions
  write.csv(predictions, "outputs/depth_model_predictions.csv", row.names = FALSE)
  
  # Plot predictions
  p_predictions <- ggplot(predictions, aes(x = predicted_soc, y = -depth_mid, 
                                           color = core_type)) +
    geom_line(linewidth = 1.2) +
    geom_point(data = modeling_data, aes(x = soc, y = -depth_mid), 
               alpha = 0.3, size = 1) +
    facet_wrap(~core_type) +
    labs(
      title = paste("Depth Model Predictions:", best_model_name),
      subtitle = "Lines: predictions, Points: observations",
      x = "SOC (g/kg)",
      y = "Depth (cm)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("outputs/plots/depth_model_predictions.png", p_predictions,
         width = 10, height = 6, dpi = 300)
  log_message("Saved depth model predictions")
}

# ============================================================================
# 8. MODEL DIAGNOSTICS
# ============================================================================

log_message("Creating model diagnostic plots")

# Function to create diagnostic plots
create_diagnostics <- function(model, model_name) {
  
  # Create diagnostic plot directory
  diag_dir <- file.path("outputs/plots", "diagnostics")
  if (!dir.exists(diag_dir)) dir.create(diag_dir, recursive = TRUE)
  
  tryCatch({
    # Extract residuals and fitted values
    if (inherits(model, "lme")) {
      resids <- residuals(model, type = "normalized")
      fitted_vals <- fitted(model)
    } else if (inherits(model, "gam")) {
      resids <- residuals(model)
      fitted_vals <- fitted(model)
    } else {
      return()
    }
    
    # Create diagnostic data frame
    diag_data <- data.frame(
      fitted = fitted_vals,
      residuals = resids,
      abs_residuals = abs(resids)
    )
    
    # Q-Q plot
    p_qq <- ggplot(diag_data, aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line() +
      labs(
        title = paste("Q-Q Plot:", model_name),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      theme_minimal()
    
    # Residuals vs Fitted
    p_resid <- ggplot(diag_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", se = TRUE, color = "blue") +
      labs(
        title = paste("Residuals vs Fitted:", model_name),
        x = "Fitted Values",
        y = "Residuals"
      ) +
      theme_minimal()
    
    # Scale-Location plot
    p_scale <- ggplot(diag_data, aes(x = fitted, y = sqrt(abs_residuals))) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = TRUE, color = "blue") +
      labs(
        title = paste("Scale-Location:", model_name),
        x = "Fitted Values",
        y = "√|Residuals|"
      ) +
      theme_minimal()
    
    # Combine plots
    library(gridExtra)
    p_combined <- grid.arrange(p_qq, p_resid, p_scale, ncol = 2)
    
    # Save combined diagnostic plot
    ggsave(file.path(diag_dir, paste0("diagnostics_", model_name, ".png")),
           p_combined, width = 10, height = 8, dpi = 300)
    
    log_message(sprintf("Saved diagnostics for %s", model_name))
    
  }, error = function(e) {
    log_message(sprintf("Error creating diagnostics for %s: %s", 
                        model_name, e$message), "WARNING")
  })
}

# Create diagnostics for top models
if (nrow(model_comparison) > 0) {
  top_models <- head(model_comparison, 3)  # Top 3 models
  
  for (i in 1:nrow(top_models)) {
    model_name <- top_models$model_name[i]
    
    # Find the model
    if (model_name %in% sapply(mixed_models, function(x) x$name)) {
      model_obj <- mixed_models[[which(sapply(mixed_models, function(x) x$name == model_name))]]$model
      create_diagnostics(model_obj, model_name)
    } else if (model_name %in% sapply(gam_models, function(x) x$name)) {
      model_obj <- gam_models[[which(sapply(gam_models, function(x) x$name == model_name))]]$model
      create_diagnostics(model_obj, model_name)
    }
  }
}

# ============================================================================
# 9. BULK DENSITY MODELING
# ============================================================================

log_message("Modeling bulk density patterns")

# Fit simple BD model
bd_model <- tryCatch({
  lme(
    bulk_density ~ poly(depth_mid, 2) * core_type,
    random = ~ 1 | core_id,
    data = modeling_data,
    method = "REML"
  )
}, error = function(e) {
  log_message(sprintf("Error fitting BD model: %s", e$message), "WARNING")
  # Fallback to simpler model
  lm(bulk_density ~ depth_mid * core_type, data = modeling_data)
})

if (!is.null(bd_model)) {
  saveRDS(bd_model, "outputs/models/bd_model.rds")
  
  # Generate BD predictions
  bd_predictions <- prediction_grid %>%
    mutate(
      predicted_bd = predict(bd_model, newdata = prediction_grid, 
                             level = if(inherits(bd_model, "lme")) 0 else NULL)
    )
  
  write.csv(bd_predictions, "outputs/bd_predictions.csv", row.names = FALSE)
  log_message("Bulk density model fitted and saved")
}

# ============================================================================
# 10. SUMMARY AND CLEANUP
# ============================================================================

# Create modeling summary
modeling_summary <- list(
  timestamp = Sys.time(),
  n_samples = nrow(modeling_data),
  n_cores = length(unique(modeling_data$core_id)),
  
  exponential_models = if(exists("exp_params_df")) {
    list(
      n_fitted = nrow(exp_params_df),
      mean_r2 = mean(exp_params_df$r_squared),
      mean_rmse = mean(exp_params_df$rmse)
    )
  } else NULL,
  
  mixed_models = if(length(mixed_models) > 0) {
    lapply(mixed_models, function(m) {
      list(name = m$name, aic = m$aic, r2 = m$r2_marginal)
    })
  } else NULL,
  
  gam_models = if(length(gam_models) > 0) {
    lapply(gam_models, function(m) {
      list(name = m$name, aic = m$aic, r2 = m$r2)
    })
  } else NULL,
  
  best_model = best_model_name,
  model_comparison = model_comparison
)

# Save summary
saveRDS(modeling_summary, "outputs/depth_modeling_summary.rds")

# Clear large objects
rm(modeling_data, prediction_grid)
gc()

log_message("Memory cleanup completed")

# Print final summary
cat("\n========================================\n")
cat("MODULE 03 COMPLETE\n")
cat("========================================\n\n")

cat("Depth Modeling Summary:\n")
cat("----------------------\n")
cat("✓ Exponential models fitted:", 
    if(exists("exp_params_df")) nrow(exp_params_df) else 0, "\n")
cat("✓ Mixed-effects models fitted:", length(mixed_models), "\n")
cat("✓ GAM models fitted:", length(gam_models), "\n")
cat("✓ Best model:", best_model_name, "\n")
cat("✓ Models saved to outputs/models/\n")
cat("✓ Predictions saved to outputs/\n")
cat("✓ Diagnostic plots saved to outputs/plots/diagnostics/\n")
cat("✓ Log saved to:", log_file, "\n\n")

cat("Next steps:\n")
cat("1. Review model comparison in outputs/model_comparison.csv\n")
cat("2. Check diagnostic plots in outputs/plots/diagnostics/\n")
cat("3. Run Module 04: source('04_spatial_modeling.R')\n\n")

# Save session info
sessionInfo <- sessionInfo()
saveRDS(sessionInfo, "outputs/session_info_module03.rds")

log_message("Module 03 completed successfully")