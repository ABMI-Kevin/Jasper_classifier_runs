# This script consolidates the HE results from multiple classifier versions
# and compares them against each other, by reporting the highest value of each
# classifier for each species+recording combo and then 

library(tidyverse)
library(data.table)

#### 1. Read in HE results to version specific dataframes
# 1.1 Define the version result directories
version_file_path_108 <- "C:/Users/Kevin Kelly/Documents/Repos/Jasper_classifier_runs/hawkears_results/2026-09-30/v1_0_8"
version_file_path_200 <- "C:/Users/Kevin Kelly/Documents/Repos/Jasper_classifier_runs/hawkears_results/2026-09-30/v2_0_0"
version_file_path_230 <- "C:/Users/Kevin Kelly/Documents/Repos/Jasper_classifier_runs/hawkears_results/2026-09-30/v2_3_0"

# 1.2 Read each file and combine them into one data frame
# v1.0.8
HE_raw_108 <- list.files(path = version_file_path_108, pattern = "\\HawkEars.txt$", full.names = TRUE) %>%
  set_names() %>% 
  map_dfr(~ read_tsv(.x, col_names = FALSE, show_col_types = FALSE), .id = "source_file")

HE_clean_108 <- HE_raw_108 %>%
  rename(start_time = X1,
         end_time = X2) %>%
  separate_wider_delim(
    cols = X3,
    delim = ";",
    names = c("species_code", "score")
  ) %>%
  mutate(recording_name = basename(source_file)) %>%
  mutate(recording_name = str_remove_all(recording_name, "_HawkEars.txt")) %>%
  separate_wider_regex(
    cols = recording_name,
    patterns = c(
      location = ".*?",
      "_",
      date_part = "\\d{8}",
      "_",
      time_part = "\\d{6}"
    )
  ) %>%
  mutate(
    recording_date_time = ymd_hms(paste(date_part, time_part))
  ) %>%
  select(-source_file, -date_part, -time_part)

#v2.0.0
HE_raw_200 <- list.files(path = version_file_path_200, pattern = "\\_scores.txt$", full.names = TRUE) %>%
  set_names() %>% 
  map_dfr(~ read_tsv(.x, col_names = FALSE, show_col_types = FALSE), .id = "source_file")

HE_clean_200 <- HE_raw_200 %>%
  rename(start_time = X1,
         end_time = X2) %>%
  separate_wider_delim(
    cols = X3,
    delim = ";",
    names = c("species_code", "score")
  ) %>%
  mutate(recording_name = basename(source_file)) %>%
  mutate(recording_name = str_remove_all(recording_name, "_scores.txt")) %>%
  separate_wider_regex(
    cols = recording_name,
    patterns = c(
      location = ".*?",
      "_",
      date_part = "\\d{8}",
      "_",
      time_part = "\\d{6}"
    )
  ) %>%
  mutate(
    recording_date_time = ymd_hms(paste(date_part, time_part))
  ) %>%
  select(-source_file, -date_part, -time_part)

#v2.3.0
HE_raw_230 <- list.files(path = version_file_path_230, pattern = "\\_scores.txt$", full.names = TRUE) %>%
  set_names() %>% 
  map_dfr(~ read_tsv(.x, col_names = FALSE, show_col_types = FALSE), .id = "source_file")

HE_clean_230 <- HE_raw_230 %>%
  rename(start_time = X1,
         end_time = X2) %>%
  separate_wider_delim(
    cols = X3,
    delim = ";",
    names = c("species_code", "score")
  ) %>%
  mutate(recording_name = basename(source_file)) %>%
  mutate(recording_name = str_remove_all(recording_name, "_scores.txt")) %>%
  separate_wider_regex(
    cols = recording_name,
    patterns = c(
      location = ".*?",
      "_",
      date_part = "\\d{8}",
      "_",
      time_part = "\\d{6}"
    )
  ) %>%
  mutate(
    recording_date_time = ymd_hms(paste(date_part, time_part))
  ) %>%
  select(-source_file, -date_part, -time_part)

#### 2. Combine dfs and compare results
HE_all <- bind_rows(
  HE_clean_108 %>% mutate(model_version = "v1.0.8"),
  HE_clean_200 %>% mutate(model_version = "v2.0.0"),
  HE_clean_230 %>% mutate(model_version = "v2.3.0")
)

#### 3. Compare number of species detected at different thresholds
# Define threshold steps
thresholds <- seq(0.5, 0.9, by = 0.1)

# Getmodel averages/recording at different thresholds
model_averages_by_thresh <- map_dfr(thresholds, function(th) {
  HE_all %>%
    filter(score >= th) %>%
    # First summarize per recording
    group_by(model_version, location, recording_date_time) %>%
    summarise(
      num_detections = n(),
      species_richness = n_distinct(species_code),
      .groups = "drop"
    ) %>%
    # Then summarize across all recordings for each model
    group_by(model_version) %>%
    summarise(
      threshold = th,
      avg_detections_per_rec = mean(num_detections),
      avg_species_richness_per_rec = mean(species_richness),
      total_detections = sum(num_detections),
      .groups = "drop"
    )
}) %>%
  select(threshold, model_version, everything()) %>%
  arrange(threshold, model_version)

write.csv(model_averages_by_thresh, "./outputs/model_averages_by_threshold.csv", row.names = FALSE)

#### 4. Compare detection overlaps between the models
# Function to compute overlap and unique counts for a pair of models at a threshold
compare_pair_at_thresh <- function(df_a, df_b, name_a, name_b, thresh) {
  
  # 1. Filter by threshold
  a <- df_a %>% 
    filter(score >= thresh)
  
  b <- df_b %>% 
    filter(score >= thresh)
  
  total_a <- nrow(a)
  total_b <- nrow(b)
  
  # 2. Handle cases where one or both models have no detections
  if (total_a == 0 || total_b == 0) {
    unique_a <- total_a
    unique_b <- total_b
    
    return(tibble(
      threshold = thresh,
      model_pair = paste0(name_a, " vs ", name_b),
      total_A = total_a,
      total_B = total_b,
      overlapping_A = 0,
      overlapping_B = 0,
      unique_to_A = unique_a,
      unique_to_B = unique_b,
      percent_unique_A = ifelse(total_a > 0, unique_a / total_a * 100, NA_real_),
      percent_unique_B = ifelse(total_b > 0, unique_b / total_b * 100, NA_real_)
    ))
  }
  
  # 3. Convert to data.table and add IDs
  dt_a <- as.data.table(a)[, `:=`(
    start_dummy = start_time,
    end_dummy = end_time,
    id_a = .I
  )]
  
  dt_b <- as.data.table(b)[, `:=`(
    start_dummy = start_time,
    end_dummy = end_time,
    id_b = .I
  )]
  
  # 4. Set keys
  setkey(
    dt_a,
    location,
    recording_date_time,
    species_code,
    start_dummy,
    end_dummy
  )
  
  setkey(
    dt_b,
    location,
    recording_date_time,
    species_code,
    start_dummy,
    end_dummy
  )
  
  # 5. Find all A detections that overlap at least one B detection
  a_matches <- foverlaps(
    dt_a,
    dt_b,
    type = "any"
  )
  
  overlapping_a_count <- n_distinct(
    a_matches$id_a[!is.na(a_matches$id_b)]
  )
  
  # 6. Find all B detections that overlap at least one A detection
  b_matches <- foverlaps(
    dt_b,
    dt_a,
    type = "any"
  )
  
  overlapping_b_count <- n_distinct(
    b_matches$id_b[!is.na(b_matches$id_a)]
  )
  
  # 7. Calculate unique detections
  unique_a <- total_a - overlapping_a_count
  unique_b <- total_b - overlapping_b_count
  
  # 8. Calculate percentage unique
  percent_unique_a <- unique_a / total_a * 100
  percent_unique_b <- unique_b / total_b * 100
  
  # 9. Return results
  tibble(
    threshold = thresh,
    model_pair = paste0(name_a, " vs ", name_b),
    total_A = total_a,
    total_B = total_b,
    overlapping_A = overlapping_a_count,
    overlapping_B = overlapping_b_count,
    unique_to_A = unique_a,
    unique_to_B = unique_b,
    percent_unique_A = percent_unique_a,
    percent_unique_B = percent_unique_b
  )
}

# Define thresholds and pairs
thresholds <- seq(0.5, 0.9, by = 0.1)
pairs <- list(
  list(HE_clean_108, HE_clean_200, "v1.0.8", "v2.0.0"),
  list(HE_clean_200, HE_clean_230, "v2.0.0", "v2.3.0"),
  list(HE_clean_108, HE_clean_230, "v1.0.8", "v2.3.0")
)

# Run function
pairwise_comparison<- map_dfr(thresholds, function(th) {
  map_dfr(pairs, function(p) {
    compare_pair_at_thresh(p[[1]], p[[2]], p[[3]], p[[4]], th)
  })
})

write.csv(pairwise_comparison, "./outputs/model_pairwise_comparison.csv", row.names = FALSE)
