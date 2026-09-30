# This script consolidates the HE results from multiple classifier versions
# and compares them against each other, by reporting the highest value of each
# classifier for each species+recording combo and then 

library(tidyverse)

#### 1. Read in HE results to version specific dataframes
# 1.1 Define the version result directories
version_file_path_108 <- "C:/Users/Kevin/Documents/Repos/Jasper_classifier_runs/hawkears_results/v1_0_8"
version_file_path_200 <- "C:/Users/Kevin/Documents/Repos/Jasper_classifier_runs/hawkears_results/v2_0_0"
version_file_path_230 <- "C:/Users/Kevin/Documents/Repos/Jasper_classifier_runs/hawkears_results/v2_3_0"

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
