## --------------------------------------------------------------------
## The Lancet Countdown: Tracking Progress on Health and Climate Change
## Indicators of Public Engagement in Health and Climate Change
## Simon Munzert
## --------------------------------------------------------------------


## Create dataset of page names ------------------

load("../data/pagenames_climate.RData")
load("../data/pagenames_health.RData")
load("../data/pagenames_links_outward_filtered.RData")
load("../data/pagenames_links_inward_filtered.RData")
load("../data/climate_health_keywords.RData")

pages <- sort(unique(c(pagenames_climate_categories, 
                       pagenames_climate_keywords,
                       pagenames_climate_keywords_red,
                       pagenames_health_categories, 
                       pagenames_health_keywords,
                       pagenames_health_keywords_red,
                       pagenames_links_climate_keywords_out, 
                       pagenames_links_climate_categories_out,
                       pagenames_links_health_keywords_out,
                       pagenames_links_health_categories_out,
                       pagenames_links_climate_keywords_in, 
                       pagenames_links_climate_categories_in,
                       pagenames_links_health_keywords_in,
                       pagenames_links_health_categories_in)))

load("../data/pagenames_climate_download_failed.RData")
load("../data/pagenames_health_download_failed.RData")
load("../data/pagenames_links_download_failed.RData")

pages_failed <- unique(c(pagenames_climate_categories_failed,
                         pagenames_health_keywords_failed # ,                         pagenames_links_failed))
))

pagenames_df <- data.frame(page = pages,
                           page_underscore = str_replace_all(pages, " ", "_"),
                           climate_categories = pages %in% pagenames_climate_categories,
                           climate_keywords   = pages %in% pagenames_climate_keywords,
                           climate_keywords_red = pages %in% pagenames_climate_keywords_red,
                           health_categories = pages %in% pagenames_health_categories,
                           health_keywords = pages %in% pagenames_health_keywords,
                           health_keywords_red = pages %in% pagenames_health_keywords_red,
                           covid_keywords = pages %in% pagenames_covid_keywords, # covid keywords
                           links_climate_keywords_out = pages %in% pagenames_links_climate_keywords_out,
                           links_climate_categories_out = pages %in% pagenames_links_climate_categories_out,
                           links_health_keywords_out = pages %in% pagenames_links_health_keywords_out,
                           links_health_categories_out = pages %in% pagenames_links_health_categories_out,
                           links_climate_keywords_in = pages %in% pagenames_links_climate_keywords_in,
                           links_climate_categories_in = pages %in% pagenames_links_climate_categories_in,
                           links_health_keywords_in = pages %in% pagenames_links_health_keywords_in,
                           links_health_categories_in = pages %in% pagenames_links_health_categories_in,
                           failed_pageviews_download = pages %in% pages_failed,
                           stringsAsFactors = FALSE)
pagenames_df$health <- dplyr::select(pagenames_df, contains("health")) %>% rowMeans() > 0
pagenames_df$climate <- dplyr::select(pagenames_df, contains("climate")) %>% rowMeans() > 0

# add indicator whether pageviews were successfully downloaded or not
pageviews_downloaded <- list.files("../data/pageviews") %>% str_replace(".csv$", "")
pagenames_df$pageviews_downloaded <- pagenames_df$page %in% pageviews_downloaded

# filter out failed pageviews downloads
pagenames_df <- pagenames_df %>% filter(!failed_pageviews_download)

failed_names <- pagenames_df %>% filter(failed_pageviews_download) %>% pull(page)

# save data frame
save(pagenames_df, file = "../data/pagenames_df.RData")

# number of climate-related articles
pagenames_df$page[pagenames_df$climate_keywords == TRUE] %>% str_subset(regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE)) %>% length()

# number of health-related articles
pagenames_df$page[pagenames_df$health_keywords == TRUE] %>% str_subset(regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE)) %>% length()

# number of covid-related articles
pagenames_df$page[pagenames_df$health_keywords == TRUE] %>% str_subset(regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE)) %>% length()

# export article names
pagenames_health_full <- pagenames_df$page[pagenames_df$health_keywords == TRUE] %>% str_subset(regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE))
paste(pagenames_health_full, collapse = ", ")

pagenames_climate_full <- pagenames_df$page[pagenames_df$climate_keywords == TRUE] %>% str_subset(regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE))
paste(pagenames_climate_full, collapse = ", ")

pagenames_covid_full <- pagenames_df$page[pagenames_df$health_keywords == TRUE] %>% str_subset(regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE))
paste(pagenames_covid_full, collapse = ", ")


# number of articles with out-links
pagenames_health_full <- pagenames_df$page[
  pagenames_df$health_keywords == TRUE | 
    pagenames_df$links_health_keywords_out == TRUE
  ] %>% 
  str_subset(regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE)) %>% 
  str_replace_all(" ", "_")

pagenames_climate_full <- pagenames_df$page[
  pagenames_df$climate_keywords == TRUE | 
    pagenames_df$links_climate_keywords_out == TRUE
  ] %>% 
  str_subset(regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE)) %>% 
  str_replace_all(" ", "_")

length(pagenames_health_full)
length(pagenames_climate_full)
