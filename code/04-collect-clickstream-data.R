## --------------------------------------------------------------------
## The Lancet Countdown: Tracking Progress on Health and Climate Change
## Indicators of Public Engagement in Health and Climate Change
## Simon Munzert
## --------------------------------------------------------------------


## load packages and functions --------
source("packages.r")
source("functions.r")


## Import raw clickstream data ------------------

# load pagenames
load("../data/pagenames_df.RData")

# identify raw clickstream files
clickstream_files_tsv <- list.files(path = "../data/clickstream_raw", pattern = ".tsv$", full.names = TRUE)


# import and efficiently store clickstream data
for(i in seq_along(clickstream_files_tsv)) {
  clickstream_df <- read.table(clickstream_files_tsv[i], header = FALSE, col.names = c("prev", "curr", "type", "n"), fill = TRUE, stringsAsFactors = FALSE)
  saveRDS(clickstream_df, file = paste0("../data/clickstream_raw/", str_replace(basename(clickstream_files_tsv[i]), "tsv$", "RDa")))
}


# subset data
clickstream_files_rda <- list.files(path = "../data/clickstream_raw", pattern = ".RDa$", full.names = TRUE)
for(i in seq_along(clickstream_files_rda)) {
  clickstream_df <- readRDS(clickstream_files_rda[i])
  clickstream_df_subset <- filter(clickstream_df, (prev %in%  pagenames_df$page_underscore | curr %in% pagenames_df$page_underscore))
  clickstream_df_subset$month <- ifelse(i < 10, paste0("2025-0", i), paste0("2025-", i))
  saveRDS(clickstream_df_subset, file = paste0("../data/clickstream_subset/clickstream_df_subset_", str_replace(basename(clickstream_files_rda[i]), ".RDa$", "-subset.RDa")))
}


# join subsetted data
clickstream_files_subset <- list.files(path = "../data/clickstream_subset", pattern = ".RDa$", full.names = TRUE)
clickstream_df_subset_list <- lapply(clickstream_files_subset, readRDS)
clickstream_df_subset <- bind_rows(clickstream_df_subset_list)
clickstream_df_subset$n <- as.numeric(clickstream_df_subset$n)
saveRDS(clickstream_df_subset, "../data/clickstream_df_subset_2025.RDa")

