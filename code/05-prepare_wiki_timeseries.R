### Simon Munzert

## load packages and functions ------------------
source("packages.r")


## import article names ------------------------

load("../data/pagenames_df.RData")
pagenames_climate_filtered <- filter(pagenames_df, climate_keywords == TRUE & pageviews_downloaded == TRUE) %>% pull(page)
pagenames_health_filtered <- filter(pagenames_df, health_keywords == TRUE & pageviews_downloaded == TRUE) %>% pull(page)
#pagenames_climate_categories_filtered <- filter(pagenames_df, climate_categories == TRUE & pageviews_downloaded == TRUE) %>% pull(page)
#pagenames_health_categories_filtered <- filter(pagenames_df, health_categories == TRUE & pageviews_downloaded == TRUE) %>% pull(page)

### import pageview statistics ----------------------------------

pagenames <- pagenames_climate_filtered[1]
filenames_short <- pagenames #%>% str_replace_all("_", "") %>% str_replace_all(" ", "")  %>% str_replace_all("/", "-")
varnames <- pagenames %>% str_replace_all("_", " ") %>% str_replace_all("/", "-")
filenames <- paste0("../data/pageviews/", filenames_short, ".csv")
# import files
res <- try(lapply(filenames, read.csv))

## function to import pageviews statistics
importPageviewFiles <- function(pagenames) {
  # get filenames
  filenames_short <- pagenames #%>% str_replace_all("_", "") %>% str_replace_all(" ", "")  %>% str_replace_all("/", "-")
  varnames <- pagenames %>% str_replace_all("_", " ") %>% str_replace_all("/", "-")
  filenames <- paste0("../data/pageviews/", filenames_short, ".csv")
  # import files
  res <- try(lapply(filenames, read.csv))
  # select vars
  res <- lapply(res, dplyr::select, date, views)
  # set varnames
  res <- lapply(seq_along(res), function(i) setNames(res[[i]], nm = c("date", varnames[i])))
  # dat variable transformation
  res <- lapply(seq_along(res), function(i) mutate(res[[i]], date = wp_date(date)))
  # join into df
  res_df <- plyr::join_all(res, by = 'date', type = 'full')
  res_df <- dplyr::arrange(res_df, date)
  #res_df <- dplyr::filter(res_df, date >= "2018-01-01")
  # return df
  return(res_df)
}

# import pageview statistics
pageviews_climate <- importPageviewFiles(pagenames_climate_filtered)
pageviews_health <- importPageviewFiles(pagenames_health_filtered)
#pageviews_health_categories <- importPageviewFiles(pagenames_health_categories_filtered)
#pageviews_climate_categories <- importPageviewFiles(pagenames_climate_categories_filtered)


# deal with missings
pageviews_climate[is.na(pageviews_climate)] <- 0
pageviews_health[is.na(pageviews_health)] <- 0
#pageviews_climate_catgories[is.na(pageviews_climate_catgories)] <- 0
#pageviews_health_catgories[is.na(pageviews_health_catgories)] <- 0

# drop pages with missings
pages_with_missings <- apply(pageviews_climate, 2, is.na) %>% colSums() %>% is_greater_than(0)
pageviews_climate <- pageviews_climate[,!pages_with_missings] 
pages_with_missings <- apply(pageviews_health, 2, is.na) %>% colSums() %>% is_greater_than(0)
pageviews_health <- pageviews_health[,!pages_with_missings] 
#pages_with_missings <- apply(pageviews_climate_categories, 2, is.na) %>% colSums() %>% is_greater_than(0)
#pageviews_climate_categories <- pageviews_climate_categories[,!pages_with_missings] 
#pages_with_missings <- apply(pageviews_health_categories, 2, is.na) %>% colSums() %>% is_greater_than(0)
#pageviews_health_categories <- pageviews_health_categories[,!pages_with_missings] 

## deal with seasonality and trending ----------------------------------

# import overall traffic data
# source: https://stats.wikimedia.org/#/en.wikipedia.org/reading/total-page-views/normal|bar|2016-12-10~2026-01-15|~total|monthly
wiki_en_traffic <- read_csv("../data/wikipedia_traffic_en_siteviews.csv")
wiki_en_traffic$month <- parse_date(as.character(wiki_en_traffic$month), "%Y-%m")
wiki_en_traffic_xts_monthly <- xts(wiki_en_traffic$traffic_raw, wiki_en_traffic$month)

# seasonal decomposition of time series by loess
wiki_en_traffic_ts_monthly <- ts(rev(as.numeric(wiki_en_traffic$traffic_raw)), start = c(2017,1), frequency = 12)
wiki_en_traffic_stl <- stats::stl(wiki_en_traffic_ts_monthly, s.window = "periodic")
wiki_en_traffic$trend <- rev(wiki_en_traffic_stl$time.series[,2])
wiki_en_traffic$seasonal <- rev(wiki_en_traffic_stl$time.series[,1])
wiki_en_traffic$trend_norm <- wiki_en_traffic$trend / wiki_en_traffic$trend[wiki_en_traffic$month == ymd("2017-01-01")] # append normalized trend


## function to clean up time series df
timeseriesAdjustDF <- function(df, period = c("daily", "weekly"), stl.adj = FALSE, norm100 = TRUE, min.dailyviewsavg = 50, global.traffic.correction = TRUE, global.traffic.df = "wiki_en_traffic", remove.neg.shocks = TRUE, neg.outlier.factor = 1.5) {
  # functions
  normalize100 <- function(x) (x/(max(x)))*100
  # function to remove negative outliers
  removeNegativeShocks <- function(xts.obj, outlier.factor = 1.5) {
    time_stamp <- Sys.time()
    repeat{
      condition <- lag.xts(xts.obj, -1) > outlier.factor*xts.obj & lag.xts(xts.obj, 1) > outlier.factor*xts.obj
      xts.obj[which(condition)] <- rowMeans(cbind(num(lag.xts(xts.obj, -1)[condition]), num(lag.xts(xts.obj, 1)[condition])))
      if(length(which(condition)) == 0 | Sys.time() > time_stamp + 1) {
        return(xts.obj)
        break
      }
    }
  }
  # impute missing data by carrying valid observations forward and backward
  df[df==0] <- NA
  df <- na.locf(df, na.rm = FALSE)
  df[is.na(df)] <- 0 # set leading NAs (`page did not exist yet') zero
  cols_num <- names(df)[!(names(df) %in% "date")]
  df[cols_num] <- sapply(df[cols_num],as.numeric) 
  # declare date variable
  df$date <- ymd(df$date)  
  # select articles with more than XX views on average
  views_count <- data.frame(pagename = names(apply(df[,-1], 2, sum, na.rm = TRUE)),
                            avg = apply(df[,-1], 2, mean, na.rm = TRUE))
  names_views_count_min <- views_count %>% filter(avg >= min.dailyviewsavg) %>% extract("pagename") %>% unlist %>% as.character()
  df <- df[,c("date", names_views_count_min)]
  if(global.traffic.correction == TRUE) {
    ## use global wikipedia traffic data to correct for trend and seasonality
    traffic_df <- get(global.traffic.df)
    # merge traffic data with wiki article data
    df$year <- year(df$date)
    df$month <- months(df$date)
    traffic_df$year <- year(traffic_df$month)
    traffic_df$month <- months(traffic_df$month)
    df_traffic <- merge(df, traffic_df, by = c("year", "month"), all.x = TRUE) %>% arrange(date)
    # correction: divide raw page views by monthly traffic figures
    df_adj <- df_traffic[,names_views_count_min] / df_traffic$trend_norm %>% round()
    # create data frame with article time series only
    df_red <- df_adj[,names_views_count_min]
  }else{
    df_red <- df[,names_views_count_min]
  }
  ## generate daily or weekly time series
  df_xts_daily <- xts(df_red, df$date)
  nas <- df_red == 0 # generate marker for missing values
  if(period == "weekly"){
    df_weekly <- apply(df_xts_daily, 2, apply.weekly, sum)
    weeks_dates <- apply.weekly(df$date, sum) %>% index()
    df_xts_weekly <- xts(df_weekly, weeks_dates)
    nas <- df_weekly == 0 # generate marker for missing values
  }
  ## seasonal decomposition of time series by LOESS to get rid of seasonal noise
  if(stl.adj == TRUE & period == "weekly"){
    df_ts_weekly <- ts(as.matrix(df_xts_weekly), start=c(2018, 1), frequency = 52) # starting in January 2018, i.e. 1st week
    ts_weekly_stl_list <- alply(df_ts_weekly, 2, function(x) {stats::stl(x, s.window = "periodic", robust = TRUE)})
    ts_weekly_adj_df <- ldply(ts_weekly_stl_list, function(x) {x$time.series[ ,2] + x$time.series[ ,3]}, .id = NULL) %>% dplyr::select(starts_with("V")) %>% t %>% as.data.frame() %>% set_names(names_views_count_min)
  }
  if(stl.adj == TRUE & period == "daily"){
    df_ts_daily <- ts(as.matrix(df_xts_daily), start=c(2018, 1), frequency = 7) # starting in January 2008, i.e. 1st day
    ts_daily_stl_list <- alply(df_ts_daily, 2, function(x) {stats::stl(x, s.window = "periodic", robust = TRUE)})
    ts_daily_adj_df <- ldply(ts_daily_stl_list, function(x) {x$time.series[ ,2] + x$time.series[ ,3]}, .id = NULL) %>% dplyr::select(starts_with("V")) %>% t %>% as.data.frame() %>% set_names(names_views_count_min)
  }  
  # normalize to 0-100 scale
  if(period == "daily" & stl.adj == TRUE & norm100 == TRUE){  ts_daily_adj_norm <- apply(ts_daily_adj_df, 2, normalize100)  }
  if(period == "daily" & stl.adj == FALSE & norm100 == TRUE){  df_xts_daily_norm <- apply(df_xts_daily, 2, normalize100)  }
  if(period == "weekly" & stl.adj == TRUE & norm100 == TRUE){  ts_weekly_adj_norm <- apply(ts_weekly_adj_df, 2, normalize100)  }
  if(period == "weekly" & stl.adj == FALSE & norm100 == TRUE){  df_xts_weekly_norm <- apply(df_xts_weekly, 2, normalize100)  }
  # make this xts object again
  if(period == "daily" & stl.adj == TRUE & norm100 == TRUE){  pageviews_xts <- xts(ts_daily_adj_norm, index(df_xts_daily), format="%Y%m%d")  }
  if(period == "daily" & stl.adj == FALSE & norm100 == TRUE){  pageviews_xts <- xts(df_xts_daily_norm, index(df_xts_daily), format="%Y%m%d")  }
  if(period == "weekly" & stl.adj == TRUE & norm100 == TRUE){  pageviews_xts <- xts(ts_weekly_adj_norm, index(df_xts_weekly), format="%Y%m%d")  }
  if(period == "weekly" & stl.adj == FALSE & norm100 == TRUE){  pageviews_xts <- xts(df_xts_weekly_norm, index(df_xts_weekly), format="%Y%m%d") }  
  if(period == "daily" & stl.adj == TRUE & norm100 == FALSE){  pageviews_xts <- xts(ts_daily_adj_df, index(df_xts_daily), format="%Y%m%d")  }
  if(period == "daily" & stl.adj == FALSE & norm100 == FALSE){  pageviews_xts <- xts(df_xts_daily, index(df_xts_daily), format="%Y%m%d")  }
  if(period == "weekly" & stl.adj == TRUE & norm100 == FALSE){  pageviews_xts <- xts(ts_weekly_adj_df, index(df_xts_weekly), format="%Y%m%d")  }
  if(period == "weekly" & stl.adj == FALSE & norm100 == FALSE){  pageviews_xts <- xts(df_xts_weekly, index(df_xts_weekly), format="%Y%m%d") } 
  # remove negative shocks
  if(remove.neg.shocks == TRUE){
    pageviews_xts <- "[<-"(pageviews_xts, , vapply(pageviews_xts, removeNegativeShocks, FUN.VALUE = numeric(nrow(pageviews_xts)), outlier.factor = 1.50))
  }  
  # impute original NAs with 0
  pageviews_xts[nas] <- 0
  # return
  return(pageviews_xts)
}


### export adjusted time series -----------------------------------------------

pageviews_climate_xts <- timeseriesAdjustDF(pageviews_climate, 
                                                    period = "daily", 
                                                    stl.adj = TRUE, 
                                                    norm100 = FALSE, 
                                                    min.dailyviewsavg = 1, 
                                                    global.traffic.correction = FALSE, 
                                                    remove.neg.shocks = FALSE, 
                                                    neg.outlier.factor = 1.5)

no_correction <- pageviews_climate_xts$`Effects of climate change`
correction <- pageviews_climate_xts$`Effects of climate change`
plot(as.numeric(no_correction), as.numeric(correction))
mean(abs(as.numeric(no_correction) - as.numeric(correction)))


pageviews_health_xts <- timeseriesAdjustDF(pageviews_health, 
                                                    period = "daily", 
                                                    stl.adj = TRUE, 
                                                    norm100 = FALSE, 
                                                    min.dailyviewsavg = 1, 
                                                    global.traffic.correction = FALSE, 
                                                    remove.neg.shocks = FALSE, 
                                                    neg.outlier.factor = 1.5)

save(pageviews_climate_xts, pageviews_health_xts, file = "../data/pageviews_adj_xts.RData")


### export unadjusted time series -----------------------------------------------

pageviews_climate_xts <- timeseriesAdjustDF(pageviews_climate, 
                                            period = "daily", 
                                            stl.adj = FALSE, 
                                            norm100 = FALSE, 
                                            min.dailyviewsavg = 1, 
                                            global.traffic.correction = FALSE, 
                                            remove.neg.shocks = FALSE, 
                                            neg.outlier.factor = 1.5)

no_correction <- pageviews_climate_xts$`Effects of climate change`
correction <- pageviews_climate_xts$`Effects of climate change`
plot(no_correction)
plot(correction)

pageviews_health_xts <- timeseriesAdjustDF(pageviews_health, 
                                           period = "daily", 
                                           stl.adj = FALSE, 
                                           norm100 = FALSE, 
                                           min.dailyviewsavg = 1, 
                                           global.traffic.correction = FALSE, 
                                           remove.neg.shocks = FALSE, 
                                           neg.outlier.factor = 1.5)

save(pageviews_climate_xts, pageviews_health_xts, file = "../data/pageviews_unadj_xts.RData")

