## --------------------------------------------------------------------
## The Lancet Countdown: Tracking Progress on Health and Climate Change
## Indicators of Public Engagement in Health and Climate Change
## Simon Munzert
## --------------------------------------------------------------------

## load packages and functions --------
source("packages.r")
source("functions.r")


## load data --------------------------
load("../data/pagenames_df.RData")



## Identify COVID-19 coverage in climate change articles ------------------------

# remove ambiguity for articles that are matched in both the climate and health domain; set to climate == FALSE
pagenames_df$climate[pagenames_df$health == TRUE & pagenames_df$climate == TRUE] <- FALSE

# climate change pages
climate_pages <- pagenames_df$page_underscore[pagenames_df$failed_pageviews_download == FALSE & pagenames_df$climate_keywords == TRUE & pagenames_df$climate == TRUE & pagenames_df$health == FALSE]

# import them as raw text
library(readtext)
html_files <- paste0("../data/wikipedia_htmls/", climate_pages, ".html")[file.exists(paste0("../data/wikipedia_htmls/", climate_pages, ".html"))]
html_files_rawtext <- readtext(html_files)

# check for COVID-19 keywords
html_files_rawtext$covid19 <- str_count(html_files_rawtext$text, regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE))
prop.table(table(html_files_rawtext$covid19 > 0))
# filter(html_files_rawtext, covid19 > 0) %>% View()

## Identify references to gender in (climate change & health) articles ------------------------

# climate change pages
climate_health_pages <- pagenames_df$page_underscore[pagenames_df$failed_pageviews_download == FALSE & pagenames_df$climate == TRUE & pagenames_df$health == TRUE]

# import them as raw text
html_files <- list.files("../data/wikipedia_htmls_links/")
climate_health_pages_html <- str_subset(html_files, regex(paste0(climate_health_pages, collapse = "|"), ignore_case = TRUE))

library(readtext)
html_files_rawtext <- readtext(paste0("../data/wikipedia_htmls_links/", climate_health_pages_html))
html_files_rawtext <- filter(html_files_rawtext, !str_detect(text, "\\(Redirected from"))

# check for gender keywords
html_files_rawtext$gender <- str_count(html_files_rawtext$text, regex(paste0(gender_keywords, collapse = "|"), ignore_case = TRUE))









