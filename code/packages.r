# install packages from CRAN
p_needed <- c("igraph",
              "readr",
              "haven",
              "tidyr",
              "reshape2",
              "ggplot2",
              "margins",
              "stringr",
              "purrr",
              "janitor",
              "magrittr",
              "gdata",
              "scales",
              "ggthemes",
              "labelled",
              "xts",
              "networkD3",
              "mokken",
              "stargazer",
              "broom",
              "xtable",
              "grid",
              "RColorBrewer",
              "readxl",
              "writexl",
              "AER",
              "lubridate",
              "plyr",
              "dplyr",
              "magick",
              "summarytools",
              "wikipediatrend",
              "WikipediR",
              "pageviews",
              "hrbrthemes",
              "psych",
              "pbapply", # apply with progress bar
              "vroom", # fast input of delimited data
              "parallel"
              )
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install, repos = "http://cran.us.r-project.org")
}
lapply(p_needed, require, character.only = TRUE)



weekday_adapt <- function(x, factor = 1){
  out <-   ifelse(weekdays(x) == "Monday", 1*factor,
                  ifelse(weekdays(x) == "Tuesday", 5*factor,
                         ifelse(weekdays(x) == "Wednesday", 3*factor, 
                                ifelse(weekdays(x) == "Thursday", 6*factor, 
                                       ifelse(weekdays(x) == "Friday", 2*factor,
                                              ifelse(weekdays(x) == "Saturday", 7*factor,
                                                     4*factor))))))
  out
}
