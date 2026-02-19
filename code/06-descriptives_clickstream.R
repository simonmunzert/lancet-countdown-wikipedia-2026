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
load("../data/climate_health_keywords.RData")

clickstream_df_subset_2025 <- readRDS("../data/clickstream_df_subset_2025.RDa")

# uncomment if analyses are to be run with latest pagenames definition
# combine with data from previous years
clickstream_df_subset_2024 <- readRDS("../data/clickstream_df_subset_2024.RDa")
clickstream_df_subset_2023 <- readRDS("../data/clickstream_df_subset_2023.RDa")
clickstream_df_subset_2022 <- readRDS("../data/clickstream_df_subset_2022.RDa")
clickstream_df_subset_2021 <- readRDS("../data/clickstream_df_subset_2021.RDa")
clickstream_df_subset_2020 <- readRDS("../data/clickstream_df_subset_2020.RDa")
clickstream_df_subset_2019 <- readRDS("../data/clickstream_df_subset_2019.RDa")
clickstream_df_subset <- rbind(clickstream_df_subset_2025, clickstream_df_subset_2024, clickstream_df_subset_2023, clickstream_df_subset_2022, clickstream_df_subset_2021, clickstream_df_subset_2020, clickstream_df_subset_2019)
saveRDS(clickstream_df_subset, "../data/clickstream_df_subset_2019_2025.RDa")

clickstream_df_subset <- readRDS("../data/clickstream_df_subset_2019_2025.RDa")



## subset data ------------------------

# using pre-defined variables
pagenames_df$health <- pagenames_df %>% select(contains("health")) %>% select(contains("keywords")) %>% rowMeans() > 0
pagenames_df$climate <- pagenames_df %>% select(contains("climate")) %>% select(contains("keywords")) %>% rowMeans() > 0

# using original keywords
pagenames_df$health <-  str_detect(pagenames_df$page, regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE))
pagenames_df$climate <-  str_detect(pagenames_df$page, regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE))

# remove ambiguity for articles that are matched in both the climate and health domain; set to climate == FALSE
pagenames_df$page[pagenames_df$health == TRUE & pagenames_df$climate == TRUE]
pagenames_df$climate[pagenames_df$health == TRUE & pagenames_df$climate == TRUE] <- FALSE

# define pagenames
pagenames_climate <- pagenames_df$page_underscore[pagenames_df$climate == TRUE]
pagenames_health <- pagenames_df$page_underscore[pagenames_df$health == TRUE]
length(pagenames_climate)
# 2328 for report, extended analyses
length(pagenames_health)
# 10238 for report, extended analyses


# subset health <--> climate
clickstream_health_climate_df <- filter(clickstream_df_subset, 
                               (prev %in% c(pagenames_climate) & curr %in% c(pagenames_health)) |
                                 (prev %in% c(pagenames_health) & curr %in% c(pagenames_climate))  )
clickstream_health_climate_df$health_to_climate <- clickstream_health_climate_df$prev %in% pagenames_health
clickstream_health_climate_df$climate_to_health <- clickstream_health_climate_df$prev %in% pagenames_climate


# subset health <--> climate, health <--> health, climate <--> climate
clickstream_health_climate_all_df <- filter(clickstream_df_subset, 
                                        (prev %in% c(pagenames_climate, pagenames_health) & curr %in% c(pagenames_climate, pagenames_health)))
clickstream_health_climate_all_df$health_to_climate <- (clickstream_health_climate_all_df$prev %in% pagenames_health) & (clickstream_health_climate_all_df$curr %in% pagenames_climate)
  clickstream_health_climate_all_df$climate_to_health <- (clickstream_health_climate_all_df$prev %in% pagenames_climate) & (clickstream_health_climate_all_df$curr %in% pagenames_health)
  clickstream_health_climate_all_df$climate_to_climate <- (clickstream_health_climate_all_df$prev %in% pagenames_climate) & (clickstream_health_climate_all_df$curr %in% pagenames_climate)
  clickstream_health_climate_all_df$health_to_health <- (clickstream_health_climate_all_df$prev %in% pagenames_health) & (clickstream_health_climate_all_df$curr %in% pagenames_health)
  
  
# uncomment if analyses are to be run with latest pagenames definition
# export subsets
write_csv(clickstream_health_climate_df, file = "../data/clickstream_health_climate_cross_df_2019_2025.csv")
write_csv(clickstream_health_climate_all_df, file = "../data/clickstream_health_climate_df_2019_2025.csv")

# uncomment if analyses are to be run with frozen data from past years
# export subsets
# write_csv(clickstream_health_climate_df, file = "../data/clickstream_health_climate_cross_df_2024.csv")
# write_csv(clickstream_health_climate_all_df, file = "../data/clickstream_health_climate_df_2024.csv")

# combine with data from previous years
# clickstream_health_climate_df_2019_2023 <- read_csv("../../analyses-report-2024/data/clickstream_health_climate_cross_df_2019_2024.csv")
# clickstream_health_climate_all_df_2019_2022 <- read_csv("../../analyses-report-2024/data/clickstream_health_climate_df_2019_2024.csv")
# 
# clickstream_health_climate_df <- rbind(clickstream_health_climate_df_2019_2024, clickstream_health_climate_df)
# clickstream_health_climate_all_df <- rbind(clickstream_health_climate_all_df_2019_2024, clickstream_health_climate_all_df)



## explore: what goes into specific articles on climate change and human health ------------------------

# clickstream_df_subset_2025 %>% filter(curr == "Effects_of_climate_change_on_human_health") %>% View()



## identify relevant articles in the health <-> climate clickstream data ------------------------

# climate articles
str_subset(str_replace_all(clickstream_health_climate_df$curr, "_", " "), regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()
str_subset(str_replace_all(clickstream_health_climate_df$prev, "_", " "), regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()

# health articles
str_subset(str_replace_all(clickstream_health_climate_df$curr, "_", " "), regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()
str_subset(str_replace_all(clickstream_health_climate_df$prev, "_", " "), regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()

# covid articles
str_subset(str_replace_all(clickstream_health_climate_df$curr, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()
str_subset(str_replace_all(clickstream_health_climate_df$prev, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()



## identify relevant articles in the health <-> climate clickstream data ------------------------

clickstream_df_subset_zero <- clickstream_df_subset
clickstream_df_subset_zero$n[is.na(clickstream_df_subset_zero$n)] <- 0

# proportions of climate -> health clickviews among all x -> health clickviews
total_health <- dplyr::filter(clickstream_df_subset_zero, prev %in% pagenames_health) %>% magrittr::use_series(n) %>% sum
prev_health <- sum(clickstream_health_climate_df$n[clickstream_health_climate_df$prev %in% pagenames_health])
(proportion_health <- prev_health/total_health)

# proportions of health -> climate clickviews among all x -> climate clickviews
total_climate <- dplyr::filter(clickstream_df_subset_zero, prev %in% c(pagenames_climate)) %>% magrittr::use_series(n) %>% sum
prev_climate <- sum(clickstream_health_climate_df$n[clickstream_health_climate_df$prev %in% pagenames_climate])
(proportion_climate <- prev_climate/total_climate)


# just the current year

clickstream_df_subset_zero_this_year <- filter(clickstream_df_subset_zero, str_detect(month, "2025"))
clickstream_health_climate_df_this_year <- filter(clickstream_health_climate_df, str_detect(month, "2025"))


# proportions of climate -> health clickviews among all x -> health clickviews
total_health <- dplyr::filter(clickstream_df_subset_zero_this_year, prev %in% pagenames_health) %>% magrittr::use_series(n) %>% sum
prev_health <- sum(clickstream_health_climate_df_this_year$n[clickstream_health_climate_df_this_year$prev %in% pagenames_health])
(proportion_health <- prev_health/total_health)

# proportions of health -> climate clickviews among all x -> climate clickviews
total_climate <- dplyr::filter(clickstream_df_subset_zero_this_year, prev %in% c(pagenames_climate)) %>% magrittr::use_series(n) %>% sum
prev_climate <- sum(clickstream_health_climate_df_this_year$n[clickstream_health_climate_df_this_year$prev %in% pagenames_climate])
(proportion_climate <- prev_climate/total_climate)



## identify top nodes/pairs  -------------------------------------

clickstream_health_climate_df$pair <- paste0(
  str_replace_all(clickstream_health_climate_df$prev, "_", " "), 
  " -> ",
  str_replace_all(clickstream_health_climate_df$curr, "_", " "))

nodes_total <- c(clickstream_health_climate_df$prev, clickstream_health_climate_df$curr) %>% str_replace_all("_", " ") %>% tabyl() %>% arrange(desc(n))

pairs_total <- clickstream_health_climate_df %>% group_by(pair) %>% summarize(n = sum(n)) %>% arrange(desc(n))
pairs_2019 <- filter(clickstream_health_climate_df, str_detect(month, "2019")) %>% group_by(pair) %>% summarize(n = sum(n), health_to_climate = first(health_to_climate)) %>% arrange(desc(n))
pairs_2020 <- filter(clickstream_health_climate_df, str_detect(month, "2020")) %>% group_by(pair) %>% summarize(n = sum(n), health_to_climate = first(health_to_climate)) %>% arrange(desc(n))
pairs_2021 <- filter(clickstream_health_climate_df, str_detect(month, "2021")) %>% group_by(pair) %>% summarize(n = sum(n), health_to_climate = first(health_to_climate)) %>% arrange(desc(n))
pairs_2022 <- filter(clickstream_health_climate_df, str_detect(month, "2022")) %>% group_by(pair) %>% summarize(n = sum(n), health_to_climate = first(health_to_climate)) %>% arrange(desc(n))
pairs_2023 <- filter(clickstream_health_climate_df, str_detect(month, "2023")) %>% group_by(pair) %>% summarize(n = sum(n), health_to_climate = first(health_to_climate)) %>% arrange(desc(n))
pairs_2024 <- filter(clickstream_health_climate_df, str_detect(month, "2024")) %>% group_by(pair) %>% summarize(n = sum(n), health_to_climate = first(health_to_climate)) %>% arrange(desc(n))
pairs_2025 <- filter(clickstream_health_climate_df, str_detect(month, "2025")) %>% group_by(pair) %>% summarize(n = sum(n), health_to_climate = first(health_to_climate)) %>% arrange(desc(n))




top_n(pairs_total, 10, wt = n)$pair
top_n(pairs_2019, 10, wt = n)$pair
top_n(pairs_2020, 10, wt = n)$pair
top_n(pairs_2021, 10, wt = n)$pair
top_n(pairs_2022, 10, wt = n)$pair
top_n(pairs_2023, 10, wt = n)$pair
top_n(pairs_2024, 10, wt = n)$pair
top_n(pairs_2025, 10, wt = n)$pair


# top five pairs per month
clickstream_health_climate_df %>% filter(str_detect(month, "2025")) %>% group_by(month) %>% arrange(desc(n)) %>% dplyr::select(month, pair, n, health_to_climate, climate_to_health) %>% slice_head(n = 10)

# top ten pairs of the year
clickstream_health_climate_df %>% filter(str_detect(month, "2025")) %>% group_by(pair) %>% summarize(coviews_total = sum(n)) %>% arrange(desc(coviews_total)) %>% slice_head(n = 10) 


# percentage increase year by year
100*(sum(pairs_2020$n) - sum(pairs_2019$n)) / sum(pairs_2019$n)
100*(sum(pairs_2021$n) - sum(pairs_2020$n)) / sum(pairs_2020$n)
100*(sum(pairs_2022$n) - sum(pairs_2021$n)) / sum(pairs_2021$n)
100*(sum(pairs_2023$n) - sum(pairs_2022$n)) / sum(pairs_2022$n)
100*(sum(pairs_2024$n) - sum(pairs_2023$n)) / sum(pairs_2023$n)
100*(sum(pairs_2025$n) - sum(pairs_2024$n)) / sum(pairs_2024$n)


# percentage change year by year in health to climate articles
100*(sum(pairs_2020[pairs_2020$health_to_climate == TRUE,]$n) - sum(pairs_2019[pairs_2019$health_to_climate == TRUE,]$n)) / sum(pairs_2019[pairs_2019$health_to_climate == TRUE,]$n)
100*(sum(pairs_2021[pairs_2021$health_to_climate == TRUE,]$n) - sum(pairs_2020[pairs_2020$health_to_climate == TRUE,]$n)) / sum(pairs_2020[pairs_2020$health_to_climate == TRUE,]$n)
100*(sum(pairs_2022[pairs_2022$health_to_climate == TRUE,]$n) - sum(pairs_2021[pairs_2021$health_to_climate == TRUE,]$n)) / sum(pairs_2021[pairs_2021$health_to_climate == TRUE,]$n)
100*(sum(pairs_2023[pairs_2023$health_to_climate == TRUE,]$n) - sum(pairs_2022[pairs_2022$health_to_climate == TRUE,]$n)) / sum(pairs_2022[pairs_2022$health_to_climate == TRUE,]$n)
100*(sum(pairs_2024[pairs_2024$health_to_climate == TRUE,]$n) - sum(pairs_2023[pairs_2023$health_to_climate == TRUE,]$n)) / sum(pairs_2023[pairs_2023$health_to_climate == TRUE,]$n)
100*(sum(pairs_2025[pairs_2025$health_to_climate == TRUE,]$n) - sum(pairs_2024[pairs_2024$health_to_climate == TRUE,]$n)) / sum(pairs_2024[pairs_2024$health_to_climate == TRUE,]$n)



# percentage change year by year in climate to health articles
100*(sum(pairs_2020[pairs_2020$health_to_climate == FALSE,]$n) - sum(pairs_2019[pairs_2019$health_to_climate == FALSE,]$n)) / sum(pairs_2019[pairs_2019$health_to_climate == FALSE,]$n)
100*(sum(pairs_2021[pairs_2021$health_to_climate == FALSE,]$n) - sum(pairs_2020[pairs_2020$health_to_climate == FALSE,]$n)) / sum(pairs_2020[pairs_2020$health_to_climate == FALSE,]$n)
100*(sum(pairs_2022[pairs_2022$health_to_climate == FALSE,]$n) - sum(pairs_2021[pairs_2021$health_to_climate == FALSE,]$n)) / sum(pairs_2021[pairs_2021$health_to_climate == FALSE,]$n)
100*(sum(pairs_2023[pairs_2023$health_to_climate == FALSE,]$n) - sum(pairs_2022[pairs_2022$health_to_climate == FALSE,]$n)) / sum(pairs_2022[pairs_2022$health_to_climate == FALSE,]$n)
100*(sum(pairs_2024[pairs_2024$health_to_climate == FALSE,]$n) - sum(pairs_2023[pairs_2023$health_to_climate == FALSE,]$n)) / sum(pairs_2023[pairs_2023$health_to_climate == FALSE,]$n)
100*(sum(pairs_2025[pairs_2025$health_to_climate == FALSE,]$n) - sum(pairs_2024[pairs_2024$health_to_climate == FALSE,]$n)) / sum(pairs_2024[pairs_2024$health_to_climate == FALSE,]$n)


# plot multiple pairs over time

clickstream_health_climate_df$year <- str_extract(clickstream_health_climate_df$month, "[:digit:]{4}") %>% as.numeric()
clickstream_health_climate_df$date <- as.Date(paste0(as.character(clickstream_health_climate_df$month), "-01"))
clickstream_health_climate_df$date_num <- as.numeric(clickstream_health_climate_df$date)

top_pairs_to_label <- clickstream_health_climate_df %>% group_by(pair)  %>% arrange(desc(n)) %>% slice_head(n = 1)

# identify joined set of top 5 overall + top 3 in latest year
top5_n <- sort(top_pairs_to_label$n, decreasing = TRUE)[5]
top3_n_year <- sort(top_pairs_to_label$n[str_detect(top_pairs_to_label$month, "2025-")], decreasing = TRUE)[3]
top_pairs_to_label <- top_pairs_to_label %>% filter(n >= top5_n | (n >= top3_n_year & year == 2023))
top_pairs_to_label$pair_label <- str_replace(top_pairs_to_label$pair, "-> ", "-\n") %>%   str_replace_all("(([:alpha:]+ ){3})", "\\1\n")
colrs <- brewer.pal(3, "Set1") 
top_pairs_to_label$color <- ifelse(top_pairs_to_label$health_to_climate == TRUE, colrs[1], colrs[2])

# plot
p <- ggplot(clickstream_health_climate_df, 
       aes(y = n, x = date, group = pair, color = health_to_climate)) + 
  geom_vline(xintercept = clickstream_health_climate_df$date_num, col = "darkgrey") + 
  geom_vline(xintercept = as.Date(unique(str_subset(clickstream_health_climate_df$date, "-01-01"))), color = "black", size = 1.1) + 
  geom_line() + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y", expand = c(0.02, 0.02)) + 
  scale_y_continuous(limits = c(0, max(clickstream_health_climate_df$n) + 200)) + 
  scale_color_manual(values = colrs[c(2, 1)]) + 
  xlab("Timeline") + ylab("Monthly co-views") + 
  theme_ipsum_rc(grid = FALSE, grid_col = "white") + 
  theme(legend.position = "none",
        panel.grid.minor.y =   element_blank(),
        panel.grid.minor.x =   element_blank(),
        panel.grid.major.x =   element_line(colour = "darkgrey",size=.5),
        panel.grid.major.y =   element_line(colour = "darkgrey",size=.5),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) + 
  annotate("text", x = top_pairs_to_label$date, y = top_pairs_to_label$n + 100, label = top_pairs_to_label$pair_label, size = 2, color = top_pairs_to_label$color, parse = FALSE)
p
ggsave(p, file = "../figures/fig-app-clickstream-views-2019-2025-by-pair.png", width = 8, height = 2, units = "cm", dpi = 300, scale = 5)



## Figure 6
## plot clickstream views health <-> climate ---------------------------

cs_health_climate_df_sum <- 
  clickstream_health_climate_df %>% 
  group_by(month) %>%
  summarize(n_views = sum(n),
            n_views_health_to_climate = sum(n[health_to_climate == TRUE]),
            n_views_climate_to_health = sum(n[health_to_climate == FALSE]))

cs_health_climate_df_sum$date <- as.Date(paste0(as.character(cs_health_climate_df_sum$month), "-01"))
cs_health_climate_df_sum$date_num <- as.numeric(cs_health_climate_df_sum$date)

# share of health -> climate co-clicks among all health <-> climate co-clicks
sum(cs_health_climate_df_sum$n_views_health_to_climate) / (sum(cs_health_climate_df_sum$n_views_health_to_climate) + sum(cs_health_climate_df_sum$n_views_climate_to_health))

# share of climate -> health co-clicks among all health <-> climate co-clicks

sum(cs_health_climate_df_sum$n_views_climate_to_health) / (sum(cs_health_climate_df_sum$n_views_health_to_climate) + sum(cs_health_climate_df_sum$n_views_climate_to_health))

# export data
cs_health_climate_df_sum$year <- year(cs_health_climate_df_sum$date) 
dat_sum <- cs_health_climate_df_sum %>% 
  group_by(year) %>% 
  summarize(n_views_total = sum(n_views),
            n_views_health_to_climate = sum(n_views_health_to_climate),
            n_views_climate_to_health = sum(n_views_climate_to_health))

dat_sum %>% mutate(across(where(is.numeric), round, 0)) %>% write_csv(file = "../data/co-views-by-year-climate-health.csv")

# cs_health_climate_df_sum
cs_health_climate_df_sum %>% write_csv(file = "../data/co-views-by-month-2019-2025-climate-health.csv")

# summary statistics for 2025 data table

# calculate column totals
cs_health_climate_df_sum %>% filter(str_detect(month, "2025")) %>% 
  summarize(n_views_total = sum(n_views),
            n_views_health_to_climate = sum(n_views_health_to_climate),
            n_views_climate_to_health = sum(n_views_climate_to_health)) %>% 
  mutate(across(where(is.numeric), round, 0)) 

# plot
p <- ggplot(cs_health_climate_df_sum, aes(y = n_views, x = date)) +
  geom_line(color = "#00AFBB", size = 1) + 
  geom_vline(xintercept = cs_health_climate_df_sum$date_num, col = "darkgrey") + 
  geom_vline(xintercept = c(cs_health_climate_df_sum$date[1], cs_health_climate_df_sum$date[12], cs_health_climate_df_sum$date[24], cs_health_climate_df_sum$date[36], cs_health_climate_df_sum$date[48], cs_health_climate_df_sum$date[60],cs_health_climate_df_sum$date[72]), color = "black", size = 1.1) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y", expand = c(0.02, 0.02)) + 
  scale_y_continuous(limits = c(0, max(cs_health_climate_df_sum$n_views) + 200)) + 
  xlab("Timeline") + ylab("Monthly co-views") + 
  theme_ipsum_rc(grid = FALSE, grid_col = "white") + 
  theme(panel.grid.minor.y =   element_blank(),
        panel.grid.minor.x =   element_blank(),
        panel.grid.major.x =   element_line(colour = "darkgrey",size=.5),
        panel.grid.major.y =   element_line(colour = "darkgrey",size=.5),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) + 
  geom_line(color = "#00AFBB", size = 1) + 
  geom_line(data = cs_health_climate_df_sum, aes(y = n_views_health_to_climate, x = date), color = "red", size = 1) + 
  geom_line(data = cs_health_climate_df_sum, aes(y = n_views_climate_to_health, x = date), color = "blue", size = 1) + 
  annotate("text", x = cs_health_climate_df_sum$date[4] + days(15), y = cs_health_climate_df_sum$n_views[15] + 210, label = "health %<->% climate", size = 3, color = "#00AFBB", parse = TRUE) +
  annotate("text", x = cs_health_climate_df_sum$date[4] + days(15), y = cs_health_climate_df_sum$n_views_health_to_climate[15] + 400, label = "health %->% climate", size = 3, color = "red", parse = TRUE) +
  annotate("text", x = cs_health_climate_df_sum$date[4] + days(15), y = cs_health_climate_df_sum$n_views_climate_to_health[15] - 600, label = "climate %->% health", size = 3, color = "blue", parse = TRUE) +
annotate("text", x = cs_health_climate_df_sum$date[c(1,12,24,36,48,60,72)] + days(15), y = 2800, label = paste0("bold('", c(2019, 2020, 2021, 2022, 2023, 2024, 2025), "')"), size = 4, color = "black", parse = TRUE, angle = 90)
p
ggsave(p, file = "../figures/fig-6-clickstream-views-2019-2025.png", width = 8, height = 2, units = "cm", dpi = 300, scale = 5)


## plot clickstream views health <-> climate, EXCLUDING COVID articles ---------------------------

cs_health_climate_df_sum <- 
  clickstream_health_climate_df %>% 
  filter(
    str_detect(str_replace_all(clickstream_health_climate_df$prev, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE), negate = TRUE) & 
      str_detect(str_replace_all(clickstream_health_climate_df$curr, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE), negate = TRUE)) %>%
  group_by(month) %>%
  summarize(n_views = sum(n),
            n_views_health_to_climate = sum(n[health_to_climate == TRUE]),
            n_views_climate_to_health = sum(n[health_to_climate == FALSE]))

cs_health_climate_df_sum$date <- as.Date(paste0(as.character(cs_health_climate_df_sum$month), "-01"))
cs_health_climate_df_sum$date_num <- as.numeric(cs_health_climate_df_sum$date)

sum(cs_health_climate_df_sum$n_views_health_to_climate) / (sum(cs_health_climate_df_sum$n_views_health_to_climate) + sum(cs_health_climate_df_sum$n_views_climate_to_health))

sum(cs_health_climate_df_sum$n_views_climate_to_health) / (sum(cs_health_climate_df_sum$n_views_health_to_climate) + sum(cs_health_climate_df_sum$n_views_climate_to_health))

# plot
p <- ggplot(cs_health_climate_df_sum, aes(y = n_views, x = date)) +
  geom_line(color = "#00AFBB", size = 1) + 
  geom_vline(xintercept = cs_health_climate_df_sum$date_num, col = "darkgrey") + 
  geom_vline(xintercept = c(cs_health_climate_df_sum$date[1], cs_health_climate_df_sum$date[12], cs_health_climate_df_sum$date[24], cs_health_climate_df_sum$date[36],  cs_health_climate_df_sum$date[48],  cs_health_climate_df_sum$date[60], cs_health_climate_df_sum$date[72]), color = "black", size = 1.1) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y", expand = c(0.02, 0.02)) + 
  scale_y_continuous(limits = c(0, max(cs_health_climate_df_sum$n_views) + 200)) + 
  xlab("Timeline") + ylab("Monthly co-views") + 
  theme_ipsum_rc(grid = FALSE, grid_col = "white") + 
  theme(panel.grid.minor.y =   element_blank(),
        panel.grid.minor.x =   element_blank(),
        panel.grid.major.x =   element_line(colour = "darkgrey",size=.5),
        panel.grid.major.y =   element_line(colour = "darkgrey",size=.5),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) + 
  geom_line(color = "#00AFBB", size = 1) + 
  geom_line(data = cs_health_climate_df_sum, aes(y = n_views_health_to_climate, x = date), color = "red", size = 1) + 
  geom_line(data = cs_health_climate_df_sum, aes(y = n_views_climate_to_health, x = date), color = "blue", size = 1) + 
  annotate("text", x = cs_health_climate_df_sum$date[4] + days(15), y = cs_health_climate_df_sum$n_views[15] + 210, label = "health %<->% climate", size = 3, color = "#00AFBB", parse = TRUE) +
  annotate("text", x = cs_health_climate_df_sum$date[4] + days(15), y = cs_health_climate_df_sum$n_views_health_to_climate[15] + 400, label = "health %->% climate", size = 3, color = "red", parse = TRUE) +
  annotate("text", x = cs_health_climate_df_sum$date[4] + days(15), y = cs_health_climate_df_sum$n_views_climate_to_health[15] - 600, label = "climate %->% health", size = 3, color = "blue", parse = TRUE) +
  annotate("text", x = cs_health_climate_df_sum$date[c(1,12,24,36,48,60,72)] + days(15), y = 2800, label = paste0("bold('", c(2019, 2020, 2021, 2022, 2023, 2024, 2025), "')"), size = 4, color = "black", parse = TRUE, angle = 90)
p
ggsave(p, file = "../figures/fig-7-clickstream-views-2019-2025-wo-covid.png", width = 8, height = 2, units = "cm", dpi = 300, scale = 5)





## plot clickstream views COVID-19 <-> climate ---------------------------

cs_covid_climate_df_sum <- 
  clickstream_health_climate_df %>% 
  filter(
    str_detect(str_replace_all(clickstream_health_climate_df$prev, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE)) | 
    str_detect(str_replace_all(clickstream_health_climate_df$curr, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE))) %>%
  group_by(month) %>%
  summarize(n_views = sum(n),
            n_views_health_to_climate = sum(n[health_to_climate == TRUE]),
            n_views_climate_to_health = sum(n[health_to_climate == FALSE]))

cs_covid_climate_df_sum$date <- as.Date(paste0(as.character(cs_covid_climate_df_sum$month), "-01"))
cs_covid_climate_df_sum$date_num <- as.numeric(cs_covid_climate_df_sum$date)

sum(cs_covid_climate_df_sum$n_views_health_to_climate) / (sum(cs_covid_climate_df_sum$n_views_health_to_climate) + sum(cs_covid_climate_df_sum$n_views_climate_to_health))

sum(cs_covid_climate_df_sum$n_views_climate_to_health) / (sum(cs_covid_climate_df_sum$n_views_health_to_climate) + sum(cs_covid_climate_df_sum$n_views_climate_to_health))

# plot
p <- ggplot(cs_covid_climate_df_sum, aes(y = n_views, x = date)) +
  geom_line(color = "#00AFBB", size = 1) + 
  geom_vline(xintercept = cs_covid_climate_df_sum$date_num, col = "darkgrey") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y", expand = c(0.02, 0.02)) + 
  scale_y_continuous(limits = c(0, max(cs_covid_climate_df_sum$n_views) + 20)) + 
  xlab("Timeline") + ylab("Monthly co-views") + 
  theme_ipsum_rc(grid = FALSE, grid_col = "white") + 
  theme(panel.grid.minor.y =   element_blank(),
        panel.grid.minor.x =   element_blank(),
        panel.grid.major.x =   element_line(colour = "darkgrey",size=.5),
        panel.grid.major.y =   element_line(colour = "darkgrey",size=.5),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) + 
  geom_line(color = "#00AFBB", size = 1) + 
  geom_line(data = cs_covid_climate_df_sum, aes(y = n_views_health_to_climate, x = date), color = "red", size = 1) + 
  geom_line(data = cs_covid_climate_df_sum, aes(y = n_views_climate_to_health, x = date), color = "blue", size = 1) + 
  annotate("text", x = cs_covid_climate_df_sum$date[6] + days(15), y = cs_covid_climate_df_sum$n_views[6] + 130, label = "COVID-19 %<->% Climate", size = 3, color = "#00AFBB", parse = TRUE) +
  annotate("text", x = cs_covid_climate_df_sum$date[6] + days(15), y = cs_covid_climate_df_sum$n_views_health_to_climate[6] - 90, label = "COVID-19 %->% Climate", size = 3, color = "red", parse = TRUE) +
  annotate("text", x = cs_covid_climate_df_sum$date[6] + days(15), y = cs_covid_climate_df_sum$n_views_climate_to_health[6] + 110, label = "Climate %->% COVID-19", size = 3, color = "blue", parse = TRUE)
p
ggsave(p, file = "../figures/fig-app-clickstream-views-covid-2021-2025.png", width = 8, height = 2, units = "cm", dpi = 300, scale = 5)


