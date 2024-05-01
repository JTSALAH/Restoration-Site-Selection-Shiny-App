# Restoration Site Choice via AHP

# ---- 0. Load in Packages ----

  require(terra)
  require(sf)
  require(landscapemetrics)
  require(ahpsurvey)
  require(tidyverse)
  require(here)

# ---- 1. Prepare Data Layers ----

  nlcd = rast(here('data', 'nlcd_2021', 'NLCD_STUDY.tif'))
  parcels = st_read(here('data', 'Parcels', 'MTTOBY_PARCELS.shp'))
  parcels = st_transform(parcels, crs(nlcd))
  dem = rast(here('data', 'Lidar_Elevation_and_Shaded_Relief', 'MassDEM.tif'))
  dem = project(dem, crs(nlcd))
  slope <- terrain(dem, v = "slope", unit = "degrees")

# ---- 2. Run Parcel Metrics ----

  # List your metrics - this example highlights habitat fragmentation
  metrics = c('lsm_l_cohesion',  # Patch Cohesion Index
              'lsm_l_contig_mn', # Asses the spatial connectedness (contiguity) of cells in patches
              'lsm_l_np')        # Number of patches in the landscape
  
  # Create Storage Dataframe
  parcels_metrics <- data.frame()
  
  # Analyze Data
  for (i in seq_len(nrow(parcels))) { 
    parcel <- parcels[i, ]
    parcel_nlcd <- mask(nlcd, parcel)
    
    # 1. Determine Environmental Characteristics
    elev <- terra::extract(dem, parcel, na.rm = TRUE)
    slp  <- terra::extract(slope, parcel, na.rm = TRUE)
    lsm  <- calculate_lsm(parcel_nlcd, what = metrics)
    
    # 2. Format Environmental Metrics
    parcel_elev <- mean(elev$Band_1, na.rm = TRUE)
    parcel_slp <- mean(slp$slope, na.rm = TRUE)
    parcel_stats <- data.frame(avg_elev = parcel_elev,
                               avg_slope = parcel_slp)
    lsm_wide <- lsm %>%
      select(metric, value) %>%
      pivot_wider(names_from = metric, values_from = value)
    
    # 3. Finalize Output
    parcel_stats <- cbind(parcel_stats, lsm_wide)
    parcels_metrics <- rbind(parcels_metrics, parcel_stats)  # Append to parcels_metrics
  }

# ---- 3. Create Normalization Custom Function ----

  # Specify Criteria where Higher & Lower Values are Better
  criteria_hb  <- c("np")
  criteria_lb  <- c("avg_elev", "avg_slope", "cohesion", "contig_mn")
  
  normalize_metrics <- function(parcels_metrics, criteria_hb, criteria_lb) {
    # Normalize and standardize variables as per MCDA
    normalize_hb  <- function(x) { x / max(x, na.rm = TRUE) }
    normalize_lb  <- function(x) { min(x, na.rm = TRUE) / x }
    
    for (criterion in criteria_hb) {
      parcels_metrics[[criterion]] <- normalize_hb(parcels_metrics[[criterion]])
    }
    
    for (criterion in criteria_lb) {
      parcels_metrics[[criterion]] <- normalize_lb(parcels_metrics[[criterion]])
    }
    
    # Save to CSV
    write.csv(st_drop_geometry(parcels_metrics), here("Normalized_Parcel_Metrics.csv"))
  }
  
  normalize_metrics(parcels_metrics, criteria_hb, criteria_lb)


# ---- 4. Analytical Hierarchy Process ----

  atts <- c("contig", "cohesion", "np", "elev", "slope")
  importance = data.frame(
    contig_cohesion = 1,  # Contiguity is the same as cohesion
    contig_np       = 4,  # Contiguity is 4x more important than number of patches
    contig_elev     = 8,  # Contiguity is 8x more important than elevation
    contig_slope    = 4,  # Contiguity is 4x more important than slope
    
    cohesion_np     = 3,  # Cohesion is 3x more important than number of patches
    cohesion_elev   = 7,  # Cohesion is 7x more important than elevation
    cohesion_slope  = 5,  # Cohesion is 5x more important than slope
    
    np_elev         = 5,  # Number of patches is 5x more important than elevation
    np_slope        = 2,  # Number of patches is 2x more important than slope
    
    elev_slope      = -2  # Slope is 2x more important than elevation
  )
  
  ahp_mat <- importance %>% ahp.mat(atts = atts, negconvert = FALSE)
  ahp_mat
  
  imean   <- ahp.indpref(ahp_mat, atts, method = "arithmetic")
  imean
  
  # If CR < 0.10, the ratio indicates a reasonable level of consistency in the pairwise comparison
  cr = ahp_mat %>%
    ahp.cr(atts)
  cr
  cr < 0.1
  
  # Assuming `all_parcels` contains the normalized criteria columns
  all_parcels <- read.csv(here('Normalized_Parcel_Metrics.csv'))
  
  # Define the criteria weights
  orig_atts <- c("contig_mn", "cohesion", "np", "avg_elev", "avg_slope")
  weights <- imean
  names(weights) <- orig_atts
  
  # Calculate the weighted scores for each parcel
  all_parcels$score <- rowSums(sapply(names(weights), function(criterion) {
    all_parcels[[criterion]] * weights[[criterion]]
  }))
  
  # Sort parcels by score to find the optimal parcel
  optimal_parcel <- all_parcels[order(-all_parcels$score),]
  
  # Output the best parcel
  print(optimal_parcel[1,])
  