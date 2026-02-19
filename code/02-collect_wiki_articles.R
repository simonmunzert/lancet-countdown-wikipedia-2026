## --------------------------------------------------------------------
## The Lancet Countdown: Tracking Progress on Health and Climate Change
## Indicators of Public Engagement in Health and Climate Change
## Simon Munzert
## --------------------------------------------------------------------


## input of this script -----------------

c("../data/climate_health_keywords.RData")

## output of this script -----------------

c("../data/pagenames_climate.RData",
  "../data/pagenames_health.RData",
  "../data/pagenames_links_outward_filtered.RData",
  "../data/pagenames_links_inward_filtered.RData")


## load packages and functions --------
source("packages.r")
source("functions.r")

## load keywords  -------------------------------
load("../data/climate_health_keywords.RData")


## look up health articles linked in effects of climate change on human health article -----------------
#browseURL("https://en.wikipedia.org/wiki/Effects_of_climate_change_on_human_health")
health_links <- page_links("en", "wikipedia", page = "Effects_of_climate_change_on_human_health", clean_response = TRUE, limit = 500) %>% unlist() %>% as.character() %>% str_subset("[^0]")
health_links_rev <- page_links("en", "wikipedia", page = "Effects_of_climate_change_on_human_health", clean_response = TRUE, limit = 500, direction = "descending") %>% unlist() %>% as.character() %>% str_subset("[^0]")
write_xlsx(data.frame(health_article = health_links), path = "../data/health_articles_effects_of_climate_change_on_human_health.xlsx")
write_xlsx(data.frame(health_article = health_links_rev), path = "../data/health_articles_effects_of_climate_change_on_human_health_rev.xlsx")
health_articles <- read_xlsx("../data/health_articles_effects_of_climate_change_on_human_health_classified.xlsx")
health_articles <- filter(health_articles, health == 1)
pagenames_health_classified <- health_articles$health_article


## collect pages -------------------------------

# search parameters
hits_limit <- 100 # first xx hits in wikipedia search
wordcount_min <- 300 # minimum word count of articles
numcats_max <- 50 # maximum number of categories to be identified
dailyviewsavg_min <- 20 # minimum average daily views
pageviews_eval_startdate <- "2018010100"
pageviews_eval_enddate <- "2025123100"
pageviews_download_date_start <- "2018-01-01"
pageviews_download_date_end <- "2025-12-31"
language <- "en"


## CLIMATE CHANGE -----------------------------------------

# do Wikipedia keyword search
pages_search_list <- lapply(climate_keywords, 
                            searchWikiFun, 
                            limit = hits_limit, 
                            wordcount.min = wordcount_min,
                            language = language)
pages_search <- pages_search_list %>% unlist %>% unique()

# look for Wikipedia categories of these pages; subset categories based on keywords
cats_search <- searchWikiCatsFun(pages =  pages_search, 
                                 output = "cats", 
                                 subcats = TRUE, 
                                 max.numcats = numcats_max)
cats_subset <- str_subset(cats_search, 
                          regex(paste0(climate_keywords_extended, collapse = "|"), # changed from climate_keywords to climate_keywords_extended in 2022
                                ignore_case = TRUE))
save(cats_subset, file = "../data/categories_climate.RData")

# search for pages within categories
pages_cats_search <- searchWikiCatsFun(categories = cats_search, 
                                       output = "pages")

# select pages
pagenames_climate_categories <- pages_cats_search # all within categories
pagenames_climate_keywords <- str_subset(pages_cats_search, 
                                         regex(paste0(climate_keywords_extended, collapse = "|"), 
                                               ignore_case = TRUE)) # keyword filtering
pagenames_climate_keywords <- str_subset(pagenames_climate_keywords, 
                                         "^Error in", 
                                         negate = TRUE)
pagenames_climate_keywords_red <- str_subset(pagenames_climate_keywords, 
                                             regex(paste0(climate_keywords, collapse = "|"), 
                                                   ignore_case = TRUE))
pagenames_climate_categories <- str_subset(pagenames_climate_categories, 
                                           "^Error in", 
                                           negate = TRUE)


# manually add pages on effects of global warming (keep for earlier time series)
pagenames_effects_global_warming <- c("Effects of climate change", "Effects of climate change on human health", "Effects of climate change on mental health", "Effects of global warming", "Effects of global warming on humans", "Effects of global warming on human health")

pagenames_climate_keywords <- unique(c(pagenames_climate_keywords, pagenames_effects_global_warming))
pagenames_climate_keywords_red <- unique(c(pagenames_climate_keywords_red, pagenames_effects_global_warming))
pagenames_climate_categories <- unique(c(pagenames_climate_categories, pagenames_effects_global_warming))


# save pagenames
save(pagenames_climate_categories, 
     pagenames_climate_keywords,
     pagenames_climate_keywords_red,
     file = "../data/pagenames_climate.RData")

# download pageviews data
pagenames_climate_categories_failed <- pageviewsDownload(
                  pages = pagenames_climate_categories, 
                  folder = "../data/pageviews/", 
                  from = pageviews_download_date_start, 
                  to = pageviews_download_date_end, 
                  language = language, 
                  package = "pageviews",
                  return.failed.pages = TRUE)



# download global warming articles manually (because it only captured the climate change articles)
pageviewsDownload(c("Effects of global warming", "Effects of global warming on humans", "Effects of global warming on human health"),
                  folder = "../data/pageviews/", 
                  from = pageviews_download_date_start, 
                  to = pageviews_download_date_end, 
                  language = language, 
                  package = "pageviews",
                  return.failed.pages = TRUE)

# download htmls
download_articles <- function(url, path){
  filename <- paste0(str_replace_all(basename(url), "/", ""), ".html")
  if(!file.exists(paste0(path, filename))){
    try(download.file(url, destfile = paste0(path, filename)))
    Sys.sleep(runif(1, 0, 1))
  }
}
wiki_urls <- paste0("https://en.wikipedia.org/wiki/", str_replace_all(pagenames_climate_keywords, " ", "_")) # changed from pagenames_climate_categories to pagenames_climate_keywords in 2022
map(wiki_urls, download_articles, path = "../data/wikipedia_htmls/")

# save information about failed download of pageviews statistics
save(pagenames_climate_categories_failed, file = "../data/pagenames_climate_download_failed.RData")


## HEALTH -------------------------------------------------------

# do Wikipedia keyword search
pages_search_list <- lapply(health_keywords, 
                            searchWikiFun, 
                            limit = hits_limit, 
                            wordcount.min = wordcount_min,
                            language = language)
pages_search <- pages_search_list %>% unlist %>% unique()

# look for Wikipedia categories of these pages; subset categories based on keywords
cats_search <- searchWikiCatsFun(pages =  pages_search, 
                                 output = "cats", 
                                 subcats = TRUE, 
                                 max.numcats = numcats_max)
cats_subset <- str_subset(cats_search, 
                          regex("film|american|people|animal|actor|actress", 
                                ignore_case = TRUE), 
                          negate = TRUE) %>%
               str_subset(regex(paste0(health_keywords_extended, collapse = "|"), 
                                ignore_case = TRUE))
save(cats_subset, file = "../data/categories_health.RData")

# search for pages within categories
pages_cats_search <- searchWikiCatsFun(categories = cats_subset, 
                                       output = "pages")

# select pages
pagenames_health_categories <- pages_cats_search # all within categories
pagenames_health_keywords <- str_subset(pages_cats_search, 
                                        regex(paste0(health_keywords_extended, collapse = "|"), 
                                              ignore_case = TRUE)) # keyword filtering

# combine with manually selected articles from "effects of global warming on human health" article 
pagenames_health_categories <- c(pagenames_health_categories, pagenames_health_classified)
pagenames_health_keywords <- c(pagenames_health_keywords, pagenames_health_classified)
pagenames_health_keywords <- str_subset(pagenames_health_keywords, "^Error in", negate = TRUE)
pagenames_health_keywords_red <- str_subset(pagenames_health_keywords, regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE))
pagenames_health_categories <- str_subset(pagenames_health_categories, "^Error in", negate = TRUE)

# covid keywords
pagenames_covid_keywords <- str_subset(pagenames_health_categories, 
                                       regex(paste0(covid_keywords, collapse = "|"), 
                                             ignore_case = TRUE))

# save pagenames 
save(pagenames_health_categories, 
     pagenames_health_keywords,
     pagenames_health_keywords_red,
     pagenames_covid_keywords,
     file = "../data/pagenames_health.RData")

# download pageviews data
pagenames_health_keywords_failed <- pageviewsDownload(
                                  pages = pagenames_health_keywords, # here: keywords-based list only; otherwise it's too much!
                                  folder = "../data/pageviews/", 
                                  from = pageviews_download_date_start, 
                                  to = pageviews_download_date_end, 
                                  language = language, 
                                  package = "pageviews",
                                  return.failed.pages = TRUE)

# save information about failed download of pageviews statistics
save(pagenames_health_keywords_failed, file = "../data/pagenames_health_download_failed.RData")

# download htmls
wiki_urls <- paste0("https://en.wikipedia.org/wiki/", str_replace_all(pagenames_health_keywords, " ", "_"))
map(wiki_urls, download_articles, path = "../data/wikipedia_htmls/")

# check overlap of articles - this is interesting; there are more articles at the intersection now!
pagenames_climate_categories[pagenames_climate_categories %in% pagenames_health_categories]
pagenames_climate_keywords[pagenames_climate_keywords %in% pagenames_health_keywords]
pagenames_health_keywords[pagenames_health_keywords %in% pagenames_climate_keywords]
pagenames_climate_categories[pagenames_climate_categories %in% pagenames_health_keywords]



## collect second-level pages -------------------------------

## import pagenames
load("../data/pagenames_health.RData")
load("../data/pagenames_climate.RData")

## identify inward and outward links

links_climate_keywords_out <- pages_links_out(pagenames_climate_keywords, language = "en")
links_climate_categories_out <- pages_links_out(pagenames_climate_categories, language = "en")

links_health_keywords_out <- pages_links_out(pagenames_health_keywords, language = "en")
links_health_categories_out <- pages_links_out(pagenames_health_categories, language = "en")

save(links_climate_keywords_out, 
     links_climate_categories_out,
     links_health_keywords_out,
     links_health_categories_out,
     file = "../data/pagenames_links_outward.RData")

links_climate_keywords_in <- pages_links_in(pagenames_climate_keywords, language = "en")
links_climate_categories_in <- pages_links_in(pagenames_climate_categories, language = "en")

links_health_keywords_in <- pages_links_in(pagenames_health_keywords, language = "en")
links_health_categories_in <- pages_links_in(pagenames_health_categories, language = "en")

save(links_climate_keywords_in, 
     links_climate_categories_in,
     links_health_keywords_in,
     links_health_categories_in,
     file = "../data/pagenames_links_inward.RData")


## filter articles by keywords

pagenames_links_climate_keywords_out <- str_subset(unique(unlist(links_climate_keywords_out)), regex(paste0(climate_keywords_extended, collapse = "|"), ignore_case = TRUE))
pagenames_links_climate_categories_out <- str_subset(unique(unlist(links_climate_categories_out)), regex(paste0(climate_keywords_extended, collapse = "|"), ignore_case = TRUE))

pagenames_links_health_keywords_out <- str_subset(unique(unlist(links_health_keywords_out)), regex(paste0(health_keywords_extended, collapse = "|"), ignore_case = TRUE))
pagenames_links_health_categories_out <- str_subset(unique(unlist(links_health_categories_out)), regex(paste0(health_keywords_extended, collapse = "|"), ignore_case = TRUE))

pagenames_links_climate_keywords_in <- str_subset(unique(unlist(links_climate_keywords_in)), regex(paste0(climate_keywords_extended, collapse = "|"), ignore_case = TRUE))
pagenames_links_climate_categories_in <- str_subset(unique(unlist(links_climate_categories_in)), regex(paste0(climate_keywords_extended, collapse = "|"), ignore_case = TRUE))

pagenames_links_health_keywords_in <- str_subset(unique(unlist(links_health_keywords_in)), regex(paste0(health_keywords_extended, collapse = "|"), ignore_case = TRUE))
pagenames_links_health_categories_in <- str_subset(unique(unlist(links_health_categories_in)), regex(paste0(health_keywords_extended, collapse = "|"), ignore_case = TRUE))

save(pagenames_links_climate_keywords_out, 
     pagenames_links_climate_categories_out,
     pagenames_links_health_keywords_out,
     pagenames_links_health_categories_out,
     file = "../data/pagenames_links_outward_filtered.RData")

save(pagenames_links_climate_keywords_in, 
     pagenames_links_climate_categories_in,
     pagenames_links_health_keywords_in,
     pagenames_links_health_categories_in,
     file = "../data/pagenames_links_inward_filtered.RData")


## download remaining article pageviews

load("../data/pagenames_links_outward_filtered.RData")
load("../data/pagenames_links_inward_filtered.RData")

links_all <- unique(c(pagenames_links_climate_keywords_out, 
                      pagenames_links_climate_categories_out,
                      pagenames_links_health_keywords_out,
                      pagenames_links_health_categories_out,
                      pagenames_links_climate_keywords_in, 
                      pagenames_links_climate_categories_in,
                      pagenames_links_health_keywords_in,
                      pagenames_links_health_categories_in))

pagenames_links_failed <- pageviewsDownload(
                  pages = rev(links_all), 
                  folder = "../data/pageviews_links/", 
                  from = pageviews_download_date_start, 
                  to = pageviews_download_date_end, 
                  language = language, 
                  package = "pageviews",
                  return.failed.pages = TRUE)

# save information about failed download of pageviews statistics
save(pagenames_links_failed, file = "../data/pagenames_links_download_failed.RData")

# download htmls
wiki_urls <- paste0("https://en.wikipedia.org/wiki/", str_replace_all(links_all, " ", "_"))
map(rev(wiki_urls), download_articles, path = "../data/wikipedia_htmls_links/")


# check overlap of articles
pagenames_links_climate_keywords_out[pagenames_links_climate_keywords_out %in% pagenames_links_health_keywords_out]

