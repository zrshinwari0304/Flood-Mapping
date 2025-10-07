setwd("G:/Ph.D Research Work/3rd Paper database/Rstudio")
.libPaths("G:/Ph.D Research Work/Codes/Rstudio/Library")
# ---------- Libraries ----------
library(terra)
library(dplyr)
library(ggplot2)

# ---------- Helper functions ----------
# Compute continuous metrics: R2 (from correlation), RMSE, MAE, MBE
continuous_metrics <- function(obs_rast, pred_rast) {
  v_obs <- values(obs_rast)
  v_pred <- values(pred_rast)
  ok <- !is.na(v_obs) & !is.na(v_pred)
  o <- v_obs[ok]; p <- v_pred[ok]
  n <- length(o)
  if(n < 2) return(NA)
  r2 <- ifelse(var(o) == 0, NA, cor(o, p, use="complete.obs")^2)
  rmse <- sqrt(mean((p - o)^2, na.rm = TRUE))
  mae  <- mean(abs(p - o), na.rm = TRUE)
  mbe  <- mean(p - o, na.rm = TRUE)
  data.frame(n=n, r2=r2, rmse=rmse, mae=mae, mbe=mbe)
}

# Compute categorical metrics from two integer-coded rasters
categorical_metrics <- function(obs_rast, pred_rast) {
  o <- values(obs_rast); p <- values(pred_rast)
  ok <- !is.na(o) & !is.na(p)
  o <- as.integer(o[ok]); p <- as.integer(p[ok])
  tab <- table(observed=o, predicted=p)
  N <- sum(tab)
  oa <- sum(diag(tab))/N
  # expected agreement (pe)
  rowp <- rowSums(tab)/N
  colp <- colSums(tab)/N
  pe  <- sum(rowp * colp)
  kappa <- (oa - pe) / (1 - pe)
  # per-class precision/recall/f1
  classes <- sort(unique(c(names(rowSums(tab)), names(colSums(tab)))))
  res_df <- data.frame(class=integer(), precision=double(), recall=double(), f1=double(), support=integer())
  for(cls in as.integer(colnames(tab))) {
    tp <- ifelse(is.na(tab[as.character(cls), as.character(cls)]), 0, tab[as.character(cls), as.character(cls)])
    pred_pos <- sum(tab[, as.character(cls)])
    actual_pos <- sum(tab[as.character(cls), ])
    prec <- ifelse(pred_pos == 0, NA, tp / pred_pos)
    rec  <- ifelse(actual_pos == 0, NA, tp / actual_pos)
    f1   <- ifelse(is.na(prec) | is.na(rec) | (prec + rec)==0, NA, 2 * prec * rec / (prec + rec))
    sup  <- actual_pos
    res_df <- rbind(res_df, data.frame(class=as.integer(cls), precision=prec, recall=rec, f1=f1, support=sup))
  }
  list(confusion = tab, overall_accuracy = oa, kappa = kappa, per_class = res_df)
}

# small function to sample points for scatterplot (avoid huge plots)
sample_df_for_plot <- function(obs_r, pred_r, max_points=20000) {
  vo <- values(obs_r); vp <- values(pred_r)
  ok <- !is.na(vo) & !is.na(vp)
  df <- data.frame(obs = vo[ok], pred = vp[ok])
  if(nrow(df) > max_points) df <- df %>% sample_n(max_points)
  df
}

# ---------- User inputs: set your file paths ----------
# Baseline observed/reference 1km rasters (native)
precip_1km_obs <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/Baseline Observed/PR.tif"   # observed/proxy 1km baseline precipitation
pop_1km_obs    <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/Baseline Observed/POP.tif"      # observed/proxy 1km baseline population
lc_1km_obs     <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/Baseline Observed/LC.tif"       # observed/proxy 1km baseline landcover (integer classes)

# Downscaled 30m rasters (your XGBoost outputs)
precip_30m_ds  <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Baseline30m/PR.tif"
pop_30m_ds     <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Baseline30m/POP.tif"   # either counts per pixel or density (note below)
lc_30m_ds      <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Baseline30m/LC.tif"    # integer class codes

# Output filenames (aggregated back to 1km)
precip_agg_1km <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/aggregated back to 1km/precip_30m_agg_to_1km.tif"
pop_agg_1km    <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/aggregated back to 1km/pop_30m_agg_to_1km.tif"
lc_agg_1km     <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/aggregated back to 1km/lc_30m_agg_to_1km.tif"

# ---------- Load reference and downscaled rasters ----------
ref_precip <- rast(precip_1km_obs)    # reference 1km grid
precip30   <- rast(precip_30m_ds)
pop30      <- rast(pop_30m_ds)
lc30       <- rast(lc_30m_ds)

# Check CRS and resolution
if (!compareGeom(ref_precip, precip30, stopOnError=FALSE)) {
  message("Warning: reference 1km and downscaled 30m precipitation rasters differ in geometry/CRS. Reprojecting 30m to reference CRS...")
  precip30 <- project(precip30, ref_precip)
  pop30    <- project(pop30, ref_precip)
  lc30     <- project(lc30, ref_precip)
}

# ---------- Aggregation / resampling (simple method) ----------
# For continuous predictors (precip) -> bilinear (approximate area-weighted)
precip_agg <- resample(precip30, ref_precip, method="bilinear")
writeRaster(
  precip_agg,  # the SpatRaster you created
  "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/agg 1km/precip_agg_1km.tif",
  overwrite = TRUE
)

# For population -> handle counts vs density
# If pop30 contains **counts per 30m pixel**, set is_density = FALSE (default).
# If pop30 contains **density (persons per km^2)** set is_density = TRUE.
is_density <- FALSE

area30_m2 <- prod(res(pop30))  # in CRS units (should be meters if projected)
area30_km2 <- area30_m2 / 1e6
area_ref_km2 <- prod(res(ref_precip)) / 1e6

if(!is_density) {
  # convert counts -> density (people per km2), resample, then back to counts
  pop30_density <- pop30 / area30_km2
  pop_density_1km <- resample(pop30_density, ref_precip, method="bilinear")
  pop_agg <- pop_density_1km * area_ref_km2
} else {
  # pop30 already density per km2
  pop_density_1km <- resample(pop30, ref_precip, method="bilinear")
  pop_agg <- pop_density_1km * area_ref_km2
}
writeRaster(
  pop_agg,  # the SpatRaster you created
  "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/agg 1km/pop_agg_1km.tif",
  overwrite = TRUE
)



# For landcover (categorical): nearest neighbor is the simplest in terra
lc_agg <- resample(lc30, ref_precip, method="near")
writeRaster(lc_agg, "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/agg 1km/lc_agg_1km.tif", overwrite=TRUE)

# ---------- Compute validation metrics (baseline) ----------
obs_precip <- rast(precip_1km_obs)
metric_precip <- continuous_metrics(obs_precip, precip_agg)

obs_pop <- rast(pop_1km_obs)
metric_pop <- continuous_metrics(obs_pop, pop_agg)
# relative bias (%) for population total
mask_ok <- !is.na(values(obs_pop)) & !is.na(values(pop_agg))
rel_bias_pct <- 100 * (sum(values(pop_agg)[mask_ok]) - sum(values(obs_pop)[mask_ok])) / sum(values(obs_pop)[mask_ok])

obs_lc <- rast(lc_1km_obs)
cat_metrics_lc <- categorical_metrics(obs_lc, lc_agg)

# print summary
print("Precipitation metrics (baseline):")
print(metric_precip)
print("Population metrics (baseline):")
print(metric_pop)
cat("Population total relative bias (%) = ", round(rel_bias_pct,2), "\n")
print("Landcover metrics (baseline):")
print(paste0("Overall accuracy = ", round(cat_metrics_lc$overall_accuracy,3)))
print(paste0("Kappa = ", round(cat_metrics_lc$kappa,3)))
print(cat_metrics_lc$per_class)

# ---------- Quick plots ----------
# Scatter: precip
dfp <- sample_df_for_plot(obs_precip, precip_agg, max_points=20000)
ggplot(dfp, aes(x=obs, y=pred)) + geom_point(alpha=0.3, size=0.6) + 
  geom_abline(intercept=0, slope=1, color="red") + labs(title="Precip: observed 1km vs downsc->agg 1km",
                                                        x="Observed 1 km", y="Downscaled aggregated to 1km")

# Residual map for precip
resid_precip <- precip_agg - obs_precip
plot(resid_precip, main="Residual (downscaled_agg - observed) Precipitation")

# Scatter: pop
dfpop <- sample_df_for_plot(obs_pop, pop_agg, max_points=20000)
ggplot(dfpop, aes(x=obs, y=pred)) + geom_point(alpha=0.3, size=0.6) + 
  geom_abline(intercept=0, slope=1, color="red") + labs(title="Population: observed 1km vs downsc->agg 1km",
                                                        x="Observed 1 km", y="Downscaled aggregated to 1km")

# mismatch map for landcover (0 matches, 1= mismatch)
mismatch <- (lc_agg != obs_lc)
plot(mismatch, main="Landcover mismatch (1 = different)")

# ---------- Save metrics to CSV ----------
metrics_tbl <- list(
  precip = c(metric_precip, list(variable="precipitation")),
  population = c(metric_pop, list(rel_bias_pct = rel_bias_pct, variable="population")),
  landcover_overall = list(variable="landcover", overall_accuracy=cat_metrics_lc$overall_accuracy, kappa=cat_metrics_lc$kappa)
)
# For convenience, write simple csv summary
summary_df <- data.frame(
  variable = c("precipitation","population","landcover"),
  r2 = c(metric_precip$r2, metric_pop$r2, NA),
  rmse = c(metric_precip$rmse, metric_pop$rmse, NA),
  overall_accuracy = c(NA, NA, cat_metrics_lc$overall_accuracy),
  kappa = c(NA, NA, cat_metrics_lc$kappa)
)
write.csv(summary_df, "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/downscale_validation_summary_baseline.csv", row.names = FALSE)
##___________________________________________________________________________________________________________________________________
#######      Now we have to check the consistency of the dwonscaled baseline variables with the future years as we don't ahve the observed points

# --------------------------------------------------------------
#  consistency and comparison of 2020 downscaled data with future downscale data
# --------------------------------------------------------------
library(terra)     # for raster I/O
library(dplyr)     # for data frame handling

# ----- User Inputs -----
data_dir <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/Consistency and comparsion yearw ise/LC"

# Named vector of file names for each year
file_names <- c(
  "2020" = "LC2020.tif",
  "2040" = "LC2040.tif",
  "2060" = "LC2060.tif",
  "2080" = "LC2080.tif",
  "2100" = "LC2100.tif"
)

# Helper function to read raster and return values as numeric vector
read_raster_vals <- function(path) {
  r <- rast(path)
  v <- values(r)
  as.numeric(v)
}

# ----- Read reference (2020) -----
ref_path <- file.path(data_dir, file_names["2020"])
ref_vals <- read_raster_vals(ref_path)
ref_vals <- ref_vals[!is.na(ref_vals)]
std_ref  <- sd(ref_vals)

# Results list
results <- list()

# Reference year entry
results[[1]] <- data.frame(
  Year = "2020",
  Standard_Deviation         = std_ref,
  Standard_Deviation_Normalized = 1.0,
  RMSE                       = 0.0,
  RMSE_Normalized            = 0.0,
  Correlation                = 1.0,
  Reference_Std_Dev          = std_ref
)

# ----- Compare each future raster to baseline -----
for (yr in c("2040", "2060", "2080", "2100")) {
  fut_path <- file.path(data_dir, file_names[yr])
  fut_vals <- read_raster_vals(fut_path)
  
  # Mask to pixels where both have values
  mask <- !is.na(ref_vals) & !is.na(fut_vals)
  ref_clean <- ref_vals[mask]
  fut_clean <- fut_vals[mask]
  
  std_fut  <- sd(fut_clean)
  rmse     <- sqrt(mean((fut_clean - ref_clean)^2))
  corr     <- cor(ref_clean, fut_clean)
  
  results[[length(results)+1]] <- data.frame(
    Year                        = yr,
    Standard_Deviation          = std_fut,
    Standard_Deviation_Normalized = std_fut / std_ref,
    RMSE                        = rmse,
    RMSE_Normalized             = rmse / std_ref,
    Correlation                 = corr,
    Reference_Std_Dev           = std_ref
  )
}

# ----- Combine & Save -----
results_df <- bind_rows(results)
out_csv <- "G:/Ph.D Research Work/3rd Paper database/Raster dataset/Validation of downscaling/consistency graphs/LC_validation_metrics.csv"
write.csv(results_df, out_csv, row.names = FALSE)

cat("Results saved to:", out_csv, "\n\n")
print(results_df)
