### Simon Munzert

## load packages and functions ------------------
source("packages.r")
source("functions.r")

# import article names
load("../data/pageviews_unadj_xts.RData")
load("../data/pageviews_adj_xts.RData")

# load keywords
load("../data/pagenames_df.RData")
load("../data/climate_health_keywords.RData")

# # subset xts data
# pageviews_climate_xts <- pageviews_climate_xts[,str_subset(names(pageviews_climate_xts), regex("climate change|warming|IPCC|greenhouse", ignore_case = TRUE))] # more restrictive regex makes sense here
 pageviews_health_xts <- pageviews_health_xts[,str_subset(names(pageviews_health_xts), regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE))]
 health_removes <- c("Alzheimer's disease", "Coeliac disease", "Kuru (disease)", "Opioid epidemic", "Whipple's disease", "Diseases and epidemics of the 19th century", "Bachelor of Science in Public Health", "Ear infection", "Dental public health", "Diseases of affluence", "Epidemiology of autism", "Epidemiology of domestic violence", "Epidemiology of HIV-AIDS", "Overnutrition", "Public health informatics", "Public health law", "Public health nursing", "Two-tier healthcare")
 pageviews_health_xts <- pageviews_health_xts[,!(names(pageviews_health_xts) %in% health_removes)]
 climate_removes <- c("Atmosphere", "Bunny Greenhouse", "Cenosphere", "Glacial earthquake", "Glacial relict", "Glacial series", "Glacial survival hypothesis", "Glass microsphere", "Gunz-Haslach interglacial", "Haslach-Mindel interglacial", "Holstein interglacial", "Integrated Biosphere Simulator", "King Abdullah City for Atomic and Renewable Energy", "Last Glacial Maximum", "Last Glacial Maximum refugia", "Last Glacial Period", "Lithosphere", "Mycorrhizosphere", "Palaeogeography", "Palaeoclimatology", "Palaeoecology", "Paleo-climate of the Snake River Plain", "Paleoatmosphere", "Paleoceanography and Paleoclimatology", 
"Paleoclimate Modelling Intercomparison Project", 
"Paleoclimatology", "Penultimate Glacial Period", "Pliocene climate", "Rare biosphere", "Rhizosphere", "Space climate", "Spermosphere", "Telesphere", "Unisphere Networks")
 pageviews_climate_xts <- pageviews_climate_xts[,!(names(pageviews_climate_xts) %in% climate_removes)]
 pageviews_covid_xts <- pageviews_health_xts[,str_subset(names(pageviews_health_xts), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE))]
 
 
# export article names
names(pageviews_climate_xts) %>% sort %>% paste0(collapse = ", ") %>% write(file = "../figures/climate_articles_list.txt")
names(pageviews_health_xts) %>% sort %>% paste0(collapse = ", ") %>% write(file = "../figures/health_articles_list.txt")
names(pageviews_covid_xts) %>% sort %>% paste0(collapse = ", ") %>% write(file = "../figures/covid_articles_list.txt")


# str_subset(str_replace_all(clickstream_health_climate_df$curr, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()
# str_subset(str_replace_all(clickstream_health_climate_df$prev, "_", " "), regex(paste0(covid_keywords, collapse = "|"), ignore_case = TRUE)) %>% unique()






# average and total views per article  -----------

# climate
climate_sum_df <-   data.frame(page = names(pageviews_climate_xts),
                       mean = pageviews_climate_xts %>% as.data.frame %>% apply(2, mean) %>% round(),
                       max = pageviews_climate_xts %>% as.data.frame %>% apply(2, max) %>% round(),
                       total = pageviews_climate_xts %>% as.data.frame %>% apply(2, sum) %>% round(),
                       stringsAsFactors = FALSE)
climate_sum_df <- arrange(climate_sum_df, desc(mean))

climate_sum_df_sub <- filter(climate_sum_df, !(page %in% c("Effects of global warming", "Effects of global warming on humans", "Effects of global warming on human health")))
climate_top_pages <- top_n(climate_sum_df_sub, 15, wt = mean)$page

climate_sum_df_exp <- climate_sum_df
names(climate_sum_df_exp) <- c("Article", "Mean",  "Maximum", "Total (Year)")
write.table(climate_sum_df_exp, file = "../figures/summarytable_climate_articles.txt", row.names = FALSE)

climate_sum_df_top <- head(climate_sum_df_exp, 15)
print(xtable(climate_sum_df_top, align = rep("r", ncol(climate_sum_df_top)+1), digits = 0, caption = "Top climate articles.\\label{tab:climate}"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!h", include.rownames = FALSE, sanitize.text.function = function(x) {x}, file = "../figures/summarytable_top_climate_articles.tex")


# health
health_sum_df <-   data.frame(page = names(pageviews_health_xts),
                               mean = pageviews_health_xts %>% as.data.frame %>% apply(2, mean) %>% round(),
                               max = pageviews_health_xts %>% as.data.frame %>% apply(2, max) %>% round(),
                               total = pageviews_health_xts %>% as.data.frame %>% apply(2, sum) %>% round(),
                               stringsAsFactors = FALSE)
health_sum_df <- arrange(health_sum_df, desc(mean))
health_top_pages <- top_n(health_sum_df, 15, wt = mean)$page

health_sum_df_exp <- health_sum_df
names(health_sum_df_exp) <- c("Article", "Mean",  "Maximum", "Total(Year)")
write.table(health_sum_df_exp, file = "../figures/summarytable_health_articles.txt", row.names = FALSE)

health_sum_df_top <- head(health_sum_df_exp, 15)
print(xtable(health_sum_df_top, align = rep("r", ncol(health_sum_df_top)+1), digits = 0, caption = "Top health articles.\\label{tab:health}"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!h", include.rownames = FALSE, sanitize.text.function = function(x) {x}, file = "../figures/summarytable_top_health_articles.tex")


# summary stats for 2025 data table

# All selected climate change articles
dat_sub <- pageviews_climate_xts %>% subset(year(index(.))>= 2025)
sum(dat_sub)

# Article "Effects of climate change"
sum(dat_sub$`Effects of climate change`)

# Article "Effects of climate change on humans health"
sum(dat_sub$`Effects of climate change on human health`)

# All selected health articles
dat_sub <- pageviews_health_xts %>% subset(year(index(.))>= 2024)
sum(dat_sub)







### Figure 1

# small multiples time series plot;  articles on the effects of climate change -----------

dat <- as.data.frame(pageviews_climate_xts)
articles <- c("Effects of climate change", "Effects of climate change on humans", "Effects of climate change on human health",
              "Effects of global warming", "Effects of global warming on humans", "Effects of global warming on human health")
articles_comb <- c("Effects of global warming/climate change", "Effects of global warming/climate change on human health") # removed "Effects of global warming/climate change on humans" because it has been integrated into "Effects of climate change" article

# combine global warming/climate change
dat$`Effects of global warming/climate change` <- dat$`Effects of climate change` + dat$`Effects of global warming`
dat$`Effects of global warming/climate change on human health` <- dat$`Effects of climate change on human health` + dat$`Effects of global warming on human health`

dat <- dplyr::select(dat, all_of(articles_comb))
dat$date <- index(pageviews_climate_xts)
dat_gather <- tidyr::gather(dat, key = "page", value = "views", -date)

# sort by popularity
dat_gather$page <- factor(dat_gather$page,levels = unique(dat_gather$page))


# identify peaks
dat_gather %<>% group_by(page) %>% mutate(
                  views_l1 = lag(views, 1),
                  views_f1 = lead(views, 1),
                  views_l2 = lag(views, 2),
                  views_f2 = lead(views, 2),
                  peak = ((views >= 2*views_l1 | views >= 2*views_f1) &
                            views > views_l2))
tabyl(dat_gather$peak)
dat_gather$peak[dat_gather$date == "2022-02-24" & dat_gather$page == "Effects of global warming/climate change"] <- TRUE
dat_gather$peak[dat_gather$date == "2022-10-18" & dat_gather$page == "Effects of global warming/climate change"] <- TRUE

# labels for peak dates
dat_peaks <- filter(dat_gather, peak == TRUE) 
dat_peaks$label <- paste(month(dat_peaks$date, label = TRUE, abbr = TRUE), day(dat_peaks$date))
dat_peaks$page <- as.factor(dat_peaks$page)

# plot
p <- ggplot(data = dat_gather, aes(x = date, y = views)) +
  geom_line(color = "#00AFBB", size = .5) +
  scale_x_date(breaks = "1 years", date_minor_breaks = "2 months", date_labels = "%Y", expand = c(0.01, 0.01)) +
  #scale_y_continuous(limits = c(0, NA), expand = expand_scale(mult = c(0, .1))) +
  facet_wrap(~page, ncol = 1, scales = "free") +
  #geom_smooth(span = 0.05, se = FALSE, color = "blue", size = .5) + 
  theme_ipsum_rc() +
  theme(strip.placement = "outside",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.spacing = unit(.5, "lines"),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) +
  xlab("Timeline") + ylab("Daily page views") +
  geom_text(data = dat_peaks, mapping = aes(x = date, y = views*1.25, label = label), size = 2)
suppressMessages(print(p))
ggsave("../figures/fig-1-climate_effects_small_multiples_2018-2025.png", width = 8, height = 4, limitsize = FALSE)


# summary stats ---------------------

# number of articles

ncol(pageviews_climate_xts)
# 965 for report
ncol(pageviews_health_xts)
# 1403 for report

# average per page
visits_avg <- dat_gather %>% 
  group_by(page) %>% 
  summarize(avg = mean(views))
visits_avg$avg[1]/visits_avg$avg[2]

# identify average year-by-year % change
dat_gather$year <- year(dat_gather$date) 
dat_gather <- dat_gather %>% mutate(year_month = format_ISO8601(date, precision = "ym"))

dat_sum <- dat_gather %>% 
  group_by(page, year) %>% 
  summarize(n_total = sum(views)) %>% 
  ungroup %>% 
  group_by(page) %>%
  mutate(perc_change = 100*(n_total - dplyr::lag(n_total, n= 1, order_by = year))/dplyr::lag(n_total, n= 1, order_by = year))
dat_sum

dat_sum_yearmonth <- dat_gather %>% 
  group_by(page, year_month) %>% 
  summarize(n_total = sum(views)) %>% 
  ungroup %>% 
  group_by(page) %>%
  mutate(perc_change = 100*(n_total - dplyr::lag(n_total, n= 1, order_by = year_month))/dplyr::lag(n_total, n= 1, order_by = year_month))
dat_sum_yearmonth

# export data
dat_sum %>% mutate(across(where(is.numeric), round, 0)) %>% write_csv(file = "../data/pageviews-by-year-effects-of-climate-change.csv")

# articles with keyword overlap
filter(pagenames_df, health_keywords == TRUE, climate_keywords == TRUE) %>% pull(page) %>% unique()



### Figure 2
# small multiples time series plot; popular articles on climate change -----------

dat <- as.data.frame(pageviews_climate_xts)
dat <- dplyr::select(dat, all_of(climate_top_pages))
dat$date <- index(pageviews_climate_xts)
dat_gather <- tidyr::gather(dat, key = "page", value = "views", -date)

# do text wrap for overly long page titles
dat_gather$page[dat_gather$page == "List of countries by carbon dioxide emissions"] <- "List of countries by\ncarbon dioxide emissions"
dat_gather$page[dat_gather$page == "Intergovernmental Panel on Climate Change"] <- "Intergovernmental Panel\non Climate Change"
dat_gather$page[dat_gather$page == "List of countries by carbon dioxide emissions"] <- "List of countries by carbon\ndioxide emissions"
dat_gather$page[dat_gather$page == "Highest temperature recorded on Earth"] <- "Highest temperature recorded\non Earth"

# sort by popularity
dat_gather$page <- factor(dat_gather$page,levels = unique(dat_gather$page))

# labels for peak dates
dat_peaks <- dat_gather %>% group_by(page) %>% summarize(date_peak = date[which.max(views)], max = max(views))
dat_peaks$label <- paste(month(dat_peaks$date_peak, label = TRUE, abbr = TRUE), day(dat_peaks$date_peak))
dat_peaks$page <- as.factor(dat_peaks$page)

# plot
p2 <- ggplot(data = dat_gather, aes(x = date, y = views)) +
  geom_line(color = "#00AFBB", size = .5) +
  geom_smooth(span = 0.2, se = FALSE, color = "blue") +
  scale_x_date(breaks = "1 years", date_minor_breaks = "2 months", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA)) +
  facet_wrap(~page, ncol = 3, scales = "free") +
  theme_ipsum_rc() +
  theme(strip.placement = "outside",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.spacing = unit(.5, "lines"),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) +
  xlab("") + ylab("") + 
  geom_text(data = dat_peaks, mapping = aes(x = date_peak, y = max*1.075, label = label), size = 2)
suppressMessages(print(p2))
ggsave("../figures/fig-2-climate_popular_small_multiples_2018-2025.png", width = 8, height = 10, limitsize = FALSE)

# inspect climate change article
summary(dat_gather$views[dat_gather$page == "Climate change"])


### Figure 3
# aggregated climate time series plot ----------------------------------

# aggregate data
dat_agg <- data.frame(views_abs = rowSums(pageviews_climate_xts),
                      views_mean = rowMeans(pageviews_climate_xts),
                      date = index(pageviews_climate_xts))

# identify name of article likely causing peak
#pageviews_climate_xts_sub <- pageviews_climate_xts[,rank(-colMeans(pageviews_climate_xts)) <= 50]
pageviews_climate_xts_sub <- pageviews_climate_xts
which_page_max_change <- diff(scale(pageviews_climate_xts_sub)) %>% apply(1, which.max)
names(which_page_max_change) <- NULL
which_page_max_change <- data.frame(date = index(pageviews_climate_xts_sub)[-1],
                                    page = names(unlist(which_page_max_change)))

# identify peaks
dat_agg <- mutate(dat_agg,
                  mean_l1 = lag(views_mean, 1),
                  mean_f1 = lead(views_mean, 1),
                  mean_l2 = lag(views_mean, 2),
                  mean_f2 = lead(views_mean, 2),
                  peak = (((views_mean >= 1.5*mean_l1 | views_mean >= 1.5*mean_f1) &
                             views_mean > 100000/ncol(pageviews_climate_xts)) &
                            (views_mean > mean_l2) |
                            ((views_abs > 150000) & (views_mean > mean_f1)  & (views_mean > mean_l1)) |
                            as.character(date) %in% c("2022-07-19", "2022-11-07")
                  ))

# labels for peak dates
dat_peaks <- filter(dat_agg, peak == TRUE)
dat_peaks$label <- paste(month(dat_peaks$date, label = TRUE, abbr = TRUE), day(dat_peaks$date))
dat_peaks <- merge(dat_peaks, which_page_max_change, by = "date", all.x = TRUE)
dat_peaks$follow_up <- lag(dat_peaks$date, 1) %in% c(dat_peaks$date - days(1), dat_peaks$date - days(2), dat_peaks$date - days(3), dat_peaks$date - days(4), dat_peaks$date - days(5), dat_peaks$date - days(6))
dat_peaks <- filter(dat_peaks, !follow_up)

# plot
p4 <- ggplot(dat_agg, aes(y = views_abs, x = date)) +
  geom_line(color = "#00AFBB", size = .5) +
  scale_x_date(breaks = "1 years", date_minor_breaks = "2 months", date_labels = "%Y", expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, NA)) +
  #geom_smooth(span = 0.05, se = FALSE, color = "blue") +
  ylab("Daily total page views") +
  xlab("Timeline") + 
  theme_ipsum_rc() +
  theme(strip.placement = "outside",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.spacing = unit(.5, "lines"),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) + 
  geom_text(data = dat_peaks, mapping = aes(x = date, y = views_abs + 30000, label = label), size = 2) # +
#geom_text(data = dat_peaks, mapping = aes(x = date, y = views_abs + 8000, label = page), hjust = 0, angle = 90, size = 2)
p4
ggsave(p4, file = paste0("../figures/fig-3-climate_mean_2018-2025.png"), width = 4, height = 2, units = "cm", dpi = 300, scale = 5)


# year-by-year average + growth

(mean_overall <- mean(dat_agg$views_abs))
(mean_2018 <- filter(dat_agg, year(date) == 2018) %>% summarize(mean(views_abs)))
(mean_2019 <- filter(dat_agg, year(date) == 2019) %>% summarize(mean(views_abs)))
(mean_2020 <- filter(dat_agg, year(date) == 2020) %>% summarize(mean(views_abs)))
(mean_2021 <- filter(dat_agg, year(date) == 2021) %>% summarize(mean(views_abs)))
(mean_2022 <- filter(dat_agg, year(date) == 2022) %>% summarize(mean(views_abs)))
(mean_2023 <- filter(dat_agg, year(date) == 2023) %>% summarize(mean(views_abs)))
(mean_2024 <- filter(dat_agg, year(date) == 2024) %>% summarize(mean(views_abs)))
(mean_2025 <- filter(dat_agg, year(date) == 2025) %>% summarize(mean(views_abs)))

100*(mean_2024 - mean_2023)/mean_2024
100*(mean_2025 - mean_2024)/mean_2025


# export data
dat_agg$year <- year(dat_agg$date) 
dat_sum <- dat_agg %>% 
  group_by(year) %>% 
  summarize(n_total = sum(views_abs))

dat_sum %>% mutate(across(where(is.numeric), round, 0)) %>% write_csv(file = "../data/pageviews-by-year-climate-change-overall.csv")





## Plot Appendix
# small multiples time series plot; popular articles on health -----------

dat <- as.data.frame(pageviews_health_xts)
dat <- dplyr::select(dat, health_top_pages)
dat$date <- index(pageviews_health_xts)
dat_gather <- tidyr::gather(dat, key = "page", value = "views", -date)

# do text wrap for overly long page titles
dat_gather$page[dat_gather$page == "Diagnostic and Statistical Manual of Mental Disorders"] <- "Diagnostic and Statistical Manual\nof Mental Disorders"

# sort by popularity
dat_gather$page <- factor(dat_gather$page,levels=unique(dat_gather$page))

# labels for peak dates
dat_peaks <- dat_gather %>% group_by(page) %>% summarize(date_peak = date[which.max(views)], max = max(views))
dat_peaks$label <- paste(month(dat_peaks$date_peak, label = TRUE, abbr = TRUE), day(dat_peaks$date_peak))
dat_peaks$page <- as.factor(dat_peaks$page)

# plot
p3 <- ggplot(data = dat_gather, aes(x = date, y = views)) +
  geom_line(color = "#00AFBB", size = .5) +
  geom_smooth(span = 0.05, se = FALSE, color = "blue") +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA), expand = expand_scale(mult = c(0, .1))) +
  facet_wrap(~page, ncol = 3, scales = "free") +
  theme_ipsum_rc() +
  theme(strip.placement = "outside",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.spacing = unit(.5, "lines"),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) +
  xlab("") + ylab("") #+
  geom_text(data = dat_peaks, mapping = aes(x = date_peak, y = max*1.075, label = label), size = 2)
suppressMessages(print(p3))
ggsave("../figures/fig-app-health_popular_small_multiples_2018-2025.png", width = 8, height = 10, limitsize = FALSE)




### aggregated health time series plot ----------------------------------

# aggregate data
dat_agg <- data.frame(views_abs = rowSums(pageviews_health_xts),
                      views_mean = rowMeans(pageviews_health_xts),
                      date = index(pageviews_health_xts))

# identify name of article likely causing peak
which_page_max_change <- diff(scale(pageviews_health_xts)) %>% apply(1, which.max)
names(which_page_max_change) <- NULL
which_page_max_change <- data.frame(date = index(pageviews_health_xts)[-1],
                                    page = names(unlist(which_page_max_change)))

# identify peaks
dat_agg <- mutate(dat_agg,
                  mean_l1 = lag(views_mean, 1),
                  mean_f1 = lead(views_mean, 1),
                  mean_l2 = lag(views_mean, 2),
                  mean_f2 = lead(views_mean, 2),
                  peak = ((views_mean >= 1.2*mean_l1 | views_mean >= 1.2*mean_f1) &
                            views_mean > 36000/ncol(pageviews_health_xts)) &
                    views_mean > mean_l2)

# labels for peak dates
dat_peaks <- filter(dat_agg, peak == TRUE)
dat_peaks$label <- paste(month(dat_peaks$date, label = TRUE, abbr = TRUE), day(dat_peaks$date))
dat_peaks <- merge(dat_peaks, which_page_max_change, by = "date", all.x = TRUE)
dat_peaks$follow_up <- lag(dat_peaks$date, 1) == (dat_peaks$date - days(1))
dat_peaks <- filter(dat_peaks, !follow_up)

# plot
options(scipen = 999)
p5 <- ggplot(dat_agg, aes(y = views_abs, x = date)) +
  geom_line(color = "#00AFBB", size = .5) +
  #geom_smooth(span = 0.1, se = FALSE, color = "blue") +
  scale_x_date(breaks = "1 years", date_minor_breaks = "2 months", date_labels = "%Y") +
  #scale_y_continuous(limits = c(0, NA), expand = expand_scale(mult = c(0, .3))) +
  ylab("Daily total page views") +
  xlab("Timeline") + 
  theme_ipsum_rc() +
  theme(strip.placement = "outside",
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        strip.text = element_text(size = 8),
        panel.spacing = unit(.5, "lines"),
        plot.margin=unit(c(.2,.2,.2,.2),"cm")) + 
  geom_text(data = dat_peaks, mapping = aes(x = date, y = views_abs + 4000, label = label), size = 2) 
  #geom_text(data = dat_peaks, mapping = aes(x = date, y = views_abs + 6000, label = page), hjust = 0, angle = 90, size = 2)
p5
ggsave(p5, file = paste0("../figures/fig-app-health_mean_2018-2025.png"), width = 4, height = 1.5, units = "cm", dpi = 300, scale = 5)

# export data
dat_agg$year <- year(dat_agg$date) 
dat_sum <- dat_agg %>% 
  group_by(year) %>% 
  summarize(n_total = sum(views_abs))

dat_sum %>% mutate(across(where(is.numeric), round, 0)) %>% write_csv(file = "../data/pageviews-by-year-health-overall.csv")





### selected time-series plots -----------------------------------------------


# function for article plot
article_plot <- function(pagename, data, save = TRUE, folder = "") {
  p <- ggplot(data, aes_string(y = paste0("`", pagename, "`"), x = index(data))) +
    geom_line(color = "#00AFBB", size = 1) +
    scale_x_date(breaks = "1 years", date_labels = "%y") +
    scale_y_continuous(limits = c(0, max(data[,pagename]))) +
    xlab("Year") + ylab("Daily Page views") +
    ggtitle(pagename)+
    theme_ipsum_rc()
  if(save == TRUE){
    ggsave(p, file = paste0(folder, str_replace_all(tolower(pagename), " ", "-"), ".png"), width = 5, height = 2, units = "cm", dpi = 300, scale = 5)
  }
  p
}

article_plot("Infant_mortality", pageviews_health_xts, folder = "../figures/")
article_plot("Effects_of_global_warming_on_human_health", pageviews_climate_xts, folder = "../figures/")
article_plot("Effects_of_global_warming_on_humans", pageviews_climate_xts, folder = "../figures/")
article_plot("Global_warming", pageviews_climate_xts, folder = "../figures/")
article_plot("Scientific_opinion_on_climate_change", pageviews_climate_xts, folder = "../figures/")



### run factor analysis to identify common latent trends ----------------------------------

# standardize data;
pageviews_climate_fa <- pageviews_climate_xts %>% scale()  %>% diff()
pageviews_health_fa <- pageviews_health_xts %>% scale() # %>% diff()

# do scree test
fa.parallel(pageviews_climate_fa)
fa.parallel(pageviews_health_fa)

# run factor analysis for climate change articles
fact <- fa(pageviews_climate_fa, nfactors = 5, rotate = "varimax", scores = "regression", fm = "ml")
print(fact, cut = 0.3, digits = 2, sort = TRUE)

fa_scores <- factor.scores(pageviews_climate_fa, fact, method = "Thurstone")$scores
plot(fa_scores[,1], type = "l")
plot(fa_scores[,2], type = "l")
plot(fa_scores[,3], type = "l")
plot(fa_scores[,4], type = "l")

# run factor analysis for health articles
fact <- fa(pageviews_health_fa, nfactors = 12, rotate = "varimax", scores = "regression", fm = "ml")
print(fact, cut = 0.3, digits = 2, sort = TRUE)



### explore data ----------------------------------

climate_keywords <- c("climate", "warming", "ipcc", "temperature", "green house", "greenhouse", "weather", "carbon")
health_keywords <- c("epidem", "diseas", "malaria", "diarrhoea", "infection", "sars", "measles", "pneumonia", "epidemic", "pandemic", "public health", "health care", "healthcare", "epidemiology", "mortality", "morbidity", "nutrition", "illness", "infectious", "ncd", "non-communicable disease", "noncommunicable disease", "communicable disease", "air pollution", "nutrition", "malnutrition", "mental disorder", "stunting")


foo <- readRDS("../data/second_level_pages_climate_full.RDa")
str_subset(foo, regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE))

foo <- readRDS("../data/second_level_cats_climate.RDa")
str_subset(foo, regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE))

foo <- readRDS("../data/second_level_pages_health_full.RDa")
str_subset(foo, regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE))

foo <- readRDS("../data/second_level_cats_health.RDa")
str_subset(foo, regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE))



### focusing events -----------------------------------------------

article <- "Intergovernmental_Panel_on_climate"
language <- "en"
platform <- "all" # "all", "desktop", "mobile-web", "mobile-app"
date_end <- Sys.Date()
date_start <- (Sys.Date() - 700)
date_end_formatted <- date_end %>% str_replace_all("-", "") %>% paste0(., "00")
date_start_formatted <- date_start  %>% str_replace_all("-", "") %>% paste0(., "00")
dat <- article_pageviews(project = paste0(language, ".wikipedia"),
                         article = article,
                         platform = platform,
                         user_type = "user",
                         start = date_start_formatted, end = date_end_formatted,
                         reformat = TRUE)
p <- ggplot(dat, aes(y = views, x = as.Date(date))) +
  geom_line(color = "#00AFBB", size = 1.5) +
  scale_x_date(breaks = "7 days") +
  xlab("") +
  theme_ipsum_rc()
p

unique(dat_gather$page)
dat_one_page<-dat_gather[dat_gather$page=="Lyme_disease",]
library(RAD)
rpca_res <- AnomalyDetection.rpca(not_temp$Temperature, frequency=temp.frequency)
plot(rpca_res$time, rpca_res$X_original, type ='l', main = "Robust PCA Anomalies", xlab = "index", ylab = "value")
lines(rpca_res$time, rpca_res$X_transform)
rpca_res_an <- subset(rpca_res, abs(S_transform) > 0)
points(rpca_res_an$time, rpca_res_an$X_transform, col='red')



### export pageview/co-click totals -----------------------------------------------

clickstream_health_climate_all_df <- read_csv("../data/clickstream_health_climate_df.csv")
clickstream_health_climate_df <- read_csv("../data/clickstream_health_climate_cross_df.csv")

# import article names
load("../data/pageviews_unadj_xts.RData")

# climate
climate_sum_df <-   data.frame(page = names(pageviews_climate_xts),
                               domain = "climate",
                               total_2018 = pageviews_climate_xts[str_detect(index(pageviews_climate_xts), "^2018"),] %>% as.data.frame %>% apply(2, sum) %>% round(),
                               total_2019 = pageviews_climate_xts[str_detect(index(pageviews_climate_xts), "^2019"),] %>% as.data.frame %>% apply(2, sum) %>% round(),
                               total = pageviews_climate_xts %>% as.data.frame %>% apply(2, sum) %>% round(),
                               stringsAsFactors = FALSE)

# health
health_sum_df <-   data.frame(page = names(pageviews_health_xts),
                               domain = "health",
                               total_2018 = pageviews_health_xts[str_detect(index(pageviews_health_xts), "^2018"),] %>% as.data.frame %>% apply(2, sum) %>% round(),
                               total_2019 = pageviews_health_xts[str_detect(index(pageviews_health_xts), "^2019"),] %>% as.data.frame %>% apply(2, sum) %>% round(),
                               total = pageviews_health_xts %>% as.data.frame %>% apply(2, sum) %>% round(),
                               stringsAsFactors = FALSE)

pageviews_total_df <- rbind(climate_sum_df, health_sum_df)
rownames(pageviews_total_df) <- NULL

pagenames_clickstream <- unique(c(clickstream_health_climate_all_df$prev, clickstream_health_climate_all_df$curr)) %>% str_replace_all("_", " ")

length(pagenames_clickstream)
length(pageviews_total_df$page)
setdiff(pagenames_clickstream, pageviews_total_df$page)


# co-click totals
clickstream_health_climate_all_df$year <- str_extract(clickstream_health_climate_all_df$month, "^[:digit:]{4}")
clicks_total_2018_prev_df <- clickstream_health_climate_all_df %>%
                      group_by(pagename = prev) %>%
                      filter(year == "2018") %>%
                      summarize(total_2018 =  sum(n, na.rm = TRUE))
clicks_total_2018_curr_df <- clickstream_health_climate_all_df %>%
  group_by(pagename = curr) %>%
  filter(year == "2018") %>%
  summarize(total_2018 =  sum(n, na.rm = TRUE))
clicks_total_2018_df <- rbind(clicks_total_2018_prev_df, clicks_total_2018_curr_df)
clicks_total_2018_df <- clicks_total_2018_df %>% group_by(pagename) %>%  summarize(total_2018 =  sum(total_2018, na.rm = TRUE))

clicks_total_2019_prev_df <- clickstream_health_climate_all_df %>%
  group_by(pagename = prev) %>%
  filter(year == "2019") %>%
  summarize(total_2019 =  sum(n, na.rm = TRUE))
clicks_total_2019_curr_df <- clickstream_health_climate_all_df %>%
  group_by(pagename = curr) %>%
  filter(year == "2019") %>%
  summarize(total_2019 =  sum(n, na.rm = TRUE))
clicks_total_2019_df <- rbind(clicks_total_2019_prev_df, clicks_total_2019_curr_df)
clicks_total_2019_df <- clicks_total_2019_df %>% group_by(pagename) %>%  summarize(total_2019 =  sum(total_2019, na.rm = TRUE))


pagenames_health_click <- unique(c(clickstream_health_climate_all_df$prev[clickstream_health_climate_all_df$health_to_climate == TRUE | clickstream_health_climate_all_df$health_to_health == TRUE], clickstream_health_climate_all_df$curr[clickstream_health_climate_all_df$climate_to_health == TRUE | clickstream_health_climate_all_df$health_to_health == TRUE]))
pagenames_climate_click <- unique(c(clickstream_health_climate_all_df$prev[clickstream_health_climate_all_df$climate_to_health == TRUE | clickstream_health_climate_all_df$climate_to_climate == TRUE], clickstream_health_climate_all_df$curr[clickstream_health_climate_all_df$health_to_climate == TRUE | clickstream_health_climate_all_df$climate_to_climate == TRUE]))

clicks_total_df <- merge(clicks_total_2018_df, clicks_total_2019_df, by = "pagename", all = TRUE)
clicks_total_df$total <- rowSums(dplyr::select(clicks_total_df, total_2018, total_2019), na.rm = TRUE)
clicks_total_df$pagename_clean <- str_replace_all(clicks_total_df$pagename, "_", " ")
clicks_total_df$article_health <- clicks_total_df$pagename %in% pagenames_health_click
clicks_total_df$article_climate <- clicks_total_df$pagename %in% pagenames_climate_click
clicks_total_df$article_domain <- ifelse(clicks_total_df$article_health == TRUE, "Health", "Climate")
write_csv(clicks_total_df, file = "../data/clicks_total_df.csv")
