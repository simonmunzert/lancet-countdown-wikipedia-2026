## --------------------------------------------------------------------
## The Lancet Countdown: Tracking Progress on Health and Climate Change
## Indicators of Public Engagement in Health and Climate Change
## Simon Munzert
## --------------------------------------------------------------------

## load packages and functions --------
source("packages.r")
source("functions.r")

## define data directory ------------------ 
data_dir = "../data/"
year <- 2025

## Import page names ------------------
load(paste0(data_dir,"climate_health_keywords.RData"))
load(paste0(data_dir,"pagenames_df.RData"))

# remove ambiguity for articles that are matched in both the climate and health domain; set to climate == FALSE
pagenames_df$climate[pagenames_df$health == TRUE & pagenames_df$climate == TRUE] <- FALSE

pagenames_health_full <- pagenames_df$page[
  pagenames_df$health == TRUE & (
  pagenames_df$health_keywords == TRUE | 
    pagenames_df$links_health_keywords_out == TRUE | 
    pagenames_df$links_health_keywords_in == TRUE)
  ] %>% 
  str_subset(regex(paste0(health_keywords, collapse = "|"), ignore_case = TRUE)) %>% 
  str_replace_all(" ", "_")

pagenames_climate_full <- pagenames_df$page[
  pagenames_df$climate == TRUE & ( 
  pagenames_df$climate_keywords == TRUE | 
    pagenames_df$links_climate_keywords_out == TRUE | 
    pagenames_df$links_climate_keywords_in == TRUE)
  ] %>% 
  str_subset(regex(paste0(climate_keywords, collapse = "|"), ignore_case = TRUE)) %>% 
  str_replace_all(" ", "_")


# number of articles used for clickstream/extended analyses (reported in the paper)

# climate
length(pagenames_climate_full)

# health
length(pagenames_health_full)



## Import subsetted clickstream data ------------------
cs_df <- readRDS(paste0(data_dir,"clickstream_df_subset_", year, ".RDa"))

## climate --> climate, health --> health, health <--> climate
cs_health_climate_df <- filter(cs_df, 
                               (prev %in% c(pagenames_climate_full) & curr %in% c(pagenames_health_full)) |
                                 (prev %in% c(pagenames_health_full) & curr %in% c(pagenames_climate_full)) |
                                 (prev %in% c(pagenames_health_full) & curr %in% c(pagenames_health_full)) |
                                 (prev %in% c(pagenames_climate_full) & curr %in% c(pagenames_climate_full)) 
)  
cs_health_climate_df$health <- ifelse(cs_health_climate_df$prev %in% pagenames_health_full, TRUE, FALSE)
cs_health_climate_df$climate <- ifelse(cs_health_climate_df$prev %in% pagenames_climate_full, TRUE, FALSE)

cs_health_climate_df$date <- as.Date(paste0(as.character(cs_health_climate_df$month), "-01"))
cs_health_climate_df$date_num <- as.numeric(cs_health_climate_df$date)

## health <--> climate
cs_health_climate_cross_df <- filter(cs_df, 
                                     (prev %in% c(pagenames_climate_full) & curr %in% c(pagenames_health_full)) |
                                       (prev %in% c(pagenames_health_full) & curr %in% c(pagenames_climate_full)))
cs_health_climate_cross_df$health <- ifelse(cs_health_climate_cross_df$prev %in% pagenames_health_full, TRUE, FALSE)
cs_health_climate_cross_df$climate <- ifelse(cs_health_climate_cross_df$prev %in% pagenames_climate_full, TRUE, FALSE)
cs_health_climate_cross_df$date <- as.Date(paste0(as.character(cs_health_climate_cross_df$month), "-01"))
cs_health_climate_cross_df$date_num <- as.numeric(cs_health_climate_cross_df$date)

## health --> health
cs_health_df <- filter(cs_df, 
                       (prev %in% c(pagenames_health_full) & curr %in% c(pagenames_health_full))
)
cs_health_df$health <- ifelse(cs_health_df$prev %in% pagenames_health_full, TRUE, FALSE)
cs_health_df$climate <- ifelse(cs_health_df$prev %in% pagenames_climate_full, TRUE, FALSE)
cs_health_df$date <- as.Date(paste0(as.character(cs_health_df$month), "-01"))
cs_health_df$date_num <- as.numeric(cs_health_df$date)

## climate --> climate
cs_climate_df <- filter(cs_df,                          
                        (prev %in% c(pagenames_climate_full) & curr %in% c(pagenames_climate_full)) 
)
cs_climate_df$health <- ifelse(cs_climate_df$prev %in% pagenames_health_full, TRUE, FALSE)
cs_climate_df$climate <- ifelse(cs_climate_df$prev %in% pagenames_climate_full, TRUE, FALSE)
cs_climate_df$date <- as.Date(paste0(as.character(cs_climate_df$month), "-01"))
cs_climate_df$date_num <- as.numeric(cs_climate_df$date)


### Figure 4
## Network plot: climate --> climate, health --> health, health <--> climate ----------------------

### construct edges
df_edge <- 
  cs_health_climate_df %>% 
  filter(year(date) == year) %>%
  group_by(prev, curr, year(date), health, climate) %>%
  dplyr::summarise(weight = sum(n)) %>%
  arrange(curr)

# select articles that have at least 20 links to other articles
frequencies <- as.data.frame(table(c(df_edge$prev, df_edge$curr)), stringsAsFactors = FALSE)
names(frequencies) <- c("pagename", "freq")
pages_nodes <- frequencies$pagename[frequencies$freq >= 20]
df_edge <- filter(df_edge, prev %in% pages_nodes & curr %in% pages_nodes)

# get list of unique articles to construct as nodes 
df_node <- 
  gather(df_edge, `prev`, `curr`, key="where", value="article") %>%
  distinct(article)
df_node <- df_node$article
df_node <- data.frame(df_node) %>%
  distinct(df_node)
names(df_node)[names(df_node) == "df_node"] <- "node"

## categorize nodes as either climate or health articles
df_node$category <- NA
df_node$category[df_node$node %in% pagenames_climate_full] <- "climate"
df_node$category[df_node$node %in% pagenames_health_full] <- "health"

## prepare graph
set.seed(3)
cs_net <- graph_from_data_frame(df_edge, vertices= df_node, directed=F) 
cs_net <- igraph::simplify(cs_net, remove.multiple = T, remove.loops = T) 

# Generate colors based on category
colrs <- brewer.pal(3, "Set1") 
V(cs_net)$color <- colrs[as.numeric(as.factor(V(cs_net)$category))]

# Compute node degrees (#links) and use that to set node size
deg <- degree(cs_net, mode="all")
deg_tf <- sqrt(deg)
V(cs_net)$size <- ((deg_tf - min(deg_tf)) / (max(deg_tf) - min(deg_tf)) * 7) + 1
V(cs_net)$label <- NA

# Set edge width based on weight
E(cs_net)$width <-  log(E(cs_net)$weight)/7
E(cs_net)$edge.color <- "gray80"

# plot
png("../figures/fig-4-nolabels-clickstream_network_graph_health_climate_2025.png", 2000, 1500)
par(oma=c(0,0,0,0) + .5)
par(mar=c(0,0,0,0))
set.seed(123)
plot(cs_net, layout = layout_with_fr) #, layout_with_drl, layout = layout_with_fr)
dev.off()


# plot with labels
most_popular_labels <- tabyl(c(cs_health_climate_cross_df$prev, cs_health_climate_cross_df$curr)) %>% arrange(desc(n)) %>% head(8) %>% pull(1) %>% str_replace_all("_", " ")
most_popular_labels <- most_popular_labels[-6] # drop Air pollution in India
V(cs_net)$label <- ifelse(str_replace_all(V(cs_net)$name, "_", " ") %in% most_popular_labels, str_replace_all(V(cs_net)$name, "_", " "), NA)
V(cs_net)$label <- V(cs_net)$label %>% 
  str_replace("Effects of climate change on human health", "Effects of\nclimate change\non human health") %>%
  str_replace("Climate change mitigation", "Climate change\nmitigation") %>%
  
  str_replace("Effects of climate change on humans", "Effects of\nclimate change\non humans") %>%
  str_replace("Effects of climate change", "Effects of\nclimate change") %>%
  str_replace("Air pollution", "Air\npollution")  %>%
  str_replace("COVID-19 pandemic", "COVID-19\npandemic") %>%
  str_replace("Greenhouse gas emissions", "Greenhouse\ngas emissions") 

vertex_label_degree <- rep(NA, length(V(cs_net)$label))
vertex_label_degree[which(!is.na(V(cs_net)$label))] <- c(-pi/2, pi/2, -pi/2, -pi/2, pi/2, -pi/2, -pi/2)

  png("../figures/fig-4-clickstream_network_graph_health_climate_labels_2025.png", width = 2000, height = 1500, res = 300)
par(oma=c(0,0,0,0) + .5,
    mar = c(0,0,0,0),
    family = "Arial")
set.seed(1)
plot.igraph(cs_net, vertex.label = V(cs_net)$label, vertex.label.cex = .4, vertex.label.dist = .8,  vertex.label.family = "Arial", vertex.label.degree = vertex_label_degree, vertex.label.color = V(cs_net)$color, edge.color = rgb(0.2, 0.2, 0.2, alpha = .25))
legend("bottomleft", title = "Articles related to", legend = rev(c("Health", "Climate change")), box.lwd = 0, pt.cex=1, cex = .75, col='black',pch=21, pt.bg = rev(unique(V(cs_net)$color)))
dev.off()





## Node descriptives ----------------------
### Top nodes by size (total connections, out and in) ----------------------
## all clickstreams

df_node_totals <- 
  cs_health_climate_df %>% 
  filter(year(date) == year) %>%
  group_by(prev, curr, year(date), health, climate) %>%
  dplyr::summarise(weight = sum(n)) %>%
  arrange(curr)


df_node_totals_out <- 
  df_node_totals %>%
  dplyr::select(page = prev, weight) %>%
  group_by(page) %>%
  dplyr::summarise(n_out=sum(weight)) %>%
  arrange(-n_out)

df_node_totals_in <- 
  df_node_totals %>%
  dplyr::select(page = curr, weight) %>%
  group_by(page) %>%
  dplyr::summarise(n_in=sum(weight)) %>%
  arrange(-n_in)

# merge
df_node_totals_both <- full_join(df_node_totals_in, df_node_totals_out)

## cross sector clickstreams only (h->cc or cc->h)
df_node_cross_totals <- 
  cs_health_climate_cross_df %>% 
  filter(year(date) == year) %>%
  group_by(prev, curr, year(date), health, climate) %>%
  dplyr::summarise(weight = sum(n)) %>%
  arrange(curr)

df_node_totals_cross_out <- 
  df_node_cross_totals %>%
  dplyr::select(page = prev, weight) %>%
  group_by(page) %>%
  dplyr::summarise(n_out=sum(weight)) %>%
  arrange(-n_out)

df_node_totals_cross_in <- 
  df_node_cross_totals %>%
  dplyr::select(page = curr, weight) %>%
  group_by(page) %>%
  dplyr::summarise(n_in=sum(weight)) %>%
  arrange(-n_in)

# merge
df_node_totals_cross_both <- full_join(df_node_totals_cross_in, df_node_totals_cross_out)


### Top nodes by connections (unique connections) ----------------------

# unique connections total
df_node_unique_out <- as.data.frame(table(df_node_totals$prev)) %>%
  dplyr::select(page = Var1, connections_out=Freq) %>%
  arrange(-connections_out)

df_node_unique_in <- as.data.frame(table(df_node_totals$curr)) %>%
  dplyr::select(page = Var1, connections_in=Freq) %>%
  arrange(-connections_in)

df_node_unique_both <- full_join(df_node_unique_in, df_node_unique_out, by="page") %>%
  arrange(-connections_in, -connections_out)

# unique cross-sector connections (h->cc or cc->h)
df_node_unique_cross_out <- as.data.frame(table(df_node_cross_totals$prev)) %>%
  dplyr::select(page = Var1, connections_out=Freq) %>%
  arrange(-connections_out)

df_node_unique_cross_in <- as.data.frame(table(df_node_cross_totals$curr)) %>%
  dplyr::select(page = Var1, connections_in=Freq) %>%
  arrange(-connections_in)

df_node_unique_cross_both <- full_join(df_node_unique_cross_in, df_node_unique_cross_out, by="page") %>%
  arrange(-connections_in, -connections_out)



## Network plot: health/climate ----------------------

### construct edges
df_edge <- 
  cs_health_climate_cross_df %>% 
  filter(year(date) == year) %>%
  group_by(prev, curr, year(date), health, climate) %>%
  dplyr::summarise(weight = sum(n)) %>%
  arrange(curr)

# select articles that have at least 2 links to other articles
frequencies <- as.data.frame(table(c(df_edge$prev, df_edge$curr)), stringsAsFactors = FALSE)
names(frequencies) <- c("pagename", "freq")
pages_nodes <- frequencies$pagename[frequencies$freq >= 2]
df_edge <- filter(df_edge, prev %in% pages_nodes & curr %in% pages_nodes)
nrow(df_edge)

# get list of unique articles to construct as nodes 
df_node <- 
  gather(df_edge, `prev`, `curr`, key = "where", value = "article") %>%
  distinct(article)
df_node <- df_node$article
df_node <- data.frame(df_node) %>%
  distinct(df_node)
names(df_node)[names(df_node) == "df_node"] <- "node"
df_node$category <- NA

## categorise nodes as either climate or health articles
df_node$category[df_node$node %in% pagenames_climate_full] <- "climate"
df_node$category[df_node$node %in% pagenames_health_full] <- "health"

## prepare graph
set.seed(3)
cs_net <- graph_from_data_frame(df_edge, vertices = df_node, directed = F) 
cs_net <- igraph::simplify(cs_net, remove.multiple = T, remove.loops = T) 

# Generate colors based on category
colrs <- brewer.pal(3, "Set1") 
V(cs_net)$color <- colrs[as.numeric(as.factor(V(cs_net)$category))]

# Compute node degrees (#links) and use that to set node size
deg <- degree(cs_net, mode = "all")
deg_tf <- sqrt(deg)
V(cs_net)$size <- ((deg_tf - min(deg_tf)) / (max(deg_tf) - min(deg_tf)) * 10) + 1
V(cs_net)$label <- NA

# Set edge width based on weight
weight_tf <- E(cs_net)$weight
E(cs_net)$width <-   ((weight_tf - min(weight_tf)) / (max(weight_tf) - min(weight_tf)) * 10) + 1
E(cs_net)$edge.color <- "gray80"


V(cs_net)$label <- ifelse(V(cs_net)$size >= 1, str_replace_all(V(cs_net)$name, "_", " "), NA)
V(cs_net)$label <- V(cs_net)$label %>% 
  str_replace_all("(([:alpha:]+ ){3})", "\\1\n")
  
# Figure 5
png("../figures/fig-5-clickstream_network_graph_health_climate_cross_2025.png", 2000, 1500, res = 300)
par(oma = c(0,0,0,0) + .5)
par(mar = c(0,0,0,0))
set.seed(2)
plot.igraph(cs_net, vertex.label = V(cs_net)$label, vertex.label.cex = .4,  vertex.label.family = "Arial", vertex.label.color = "black", edge.color = rgb(0.2, 0.2, 0.2, alpha = .25))
legend("bottomright", title = "Articles related to", legend = c("Climate change", "Health"), box.lwd = 0, pt.cex = 1, cex = .75, col = 'black', pch = 21, pt.bg = unique(V(cs_net)$color))
dev.off()



## Network plot: health/health ----------------------

### construct edges
df_edge <- 
  cs_health_df %>% 
  filter(year(date) == year) %>%
  group_by(prev, curr, year(date), health, climate) %>%
  dplyr::summarise(weight = sum(n)) %>%
  arrange(curr)

# select articles that have at least 3 links to other articles
frequencies <- as.data.frame(table(c(df_edge$prev, df_edge$curr)), stringsAsFactors = FALSE)
names(frequencies) <- c("pagename", "freq")
pages_nodes <- frequencies$pagename[frequencies$freq >= 10]
df_edge <- filter(df_edge, prev %in% pages_nodes & curr %in% pages_nodes)
#df_edge <- filter(df_edge, weight > 200)
nrow(df_edge)

# get list of unique articles to construct as nodes 
df_node <- 
  gather(df_edge, `prev`, `curr`, key="where", value="article") %>%
  distinct(article)
df_node <- df_node$article
df_node <- data.frame(df_node) %>%
  distinct(df_node)
names(df_node)[names(df_node) == "df_node"] <- "node"
df_node$category <- NA

## categorise nodes as either climate or health articles
df_node$category[df_node$node %in% pagenames_climate_full] <- "climate"
df_node$category[df_node$node %in% pagenames_health_full] <- "health"

## prepare graph
set.seed(3)
cs_net <- graph_from_data_frame(df_edge, vertices= df_node, directed=F) 
cs_net <- igraph::simplify(cs_net, remove.multiple = T, remove.loops = T) 

# Generate colors based on category
colrs <- brewer.pal(3, "Set1") 
V(cs_net)$color <- colrs[2]

# Compute node degrees (#links) and use that to set node size
deg <- degree(cs_net, mode="all")
V(cs_net)$size <- deg/10
V(cs_net)$label <- NA

# Set edge width based on weight
E(cs_net)$width <- log(E(cs_net)$weight)/10
E(cs_net)$edge.color <- "gray80"

# Labels
V(cs_net)$label <- ifelse(degree(cs_net) > 70, str_replace_all(V(cs_net)$name, "_", " "), NA)
V(cs_net)$label <- V(cs_net)$label %>% 
  str_replace_all("(([:alpha:]+ ){3})", "\\1\n")

# plot
png("../figures/fig-app-clickstream_network_graph_health_2025.png", 1500, 2000)
par(oma=c(0,0,0,0) + .5)
par(mar=c(0,0,0,0))
set.seed(123)
plot.igraph(cs_net, vertex.label = V(cs_net)$label, layout= layout.by.attr(cs_net, wc=1), vertex.label.cex = 3, vertex.label.dist = 1, vertex.label.degree = 0)
dev.off()





## Network plot: climate/climate ----------------------

### construct edges
df_edge <- 
  cs_climate_df %>% 
  filter(year(date) == year) %>%
  group_by(prev, curr, year(date), climate, climate) %>%
  dplyr::summarise(weight = sum(n)) %>%
  arrange(curr)

# select articles that have at least 3 links to other articles
frequencies <- as.data.frame(table(c(df_edge$prev, df_edge$curr)), stringsAsFactors = FALSE)
names(frequencies) <- c("pagename", "freq")
pages_nodes <- frequencies$pagename[frequencies$freq >= 10]
df_edge <- filter(df_edge, prev %in% pages_nodes & curr %in% pages_nodes)
#df_edge <- filter(df_edge, weight > 200)
nrow(df_edge)

# get list of unique articles to construct as nodes 
df_node <- 
  gather(df_edge, `prev`, `curr`, key="where", value="article") %>%
  distinct(article)
df_node <- df_node$article
df_node <- data.frame(df_node) %>%
  distinct(df_node)
names(df_node)[names(df_node) == "df_node"] <- "node"
df_node$category <- NA

## categorise nodes as either climate or climate articles
df_node$category[df_node$node %in% pagenames_climate_full] <- "climate"
df_node$category[df_node$node %in% pagenames_climate_full] <- "climate"

## prepare graph
set.seed(3)
cs_net <- graph_from_data_frame(df_edge, vertices= df_node, directed=F) 
cs_net <- igraph::simplify(cs_net, remove.multiple = T, remove.loops = T) 

# Generate colors based on category
colrs <- brewer.pal(3, "Set1") 
V(cs_net)$color <- colrs[1]

# Compute node degrees (#links) and use that to set node size
deg <- degree(cs_net, mode="all")
V(cs_net)$size <- deg/10
V(cs_net)$label <- NA

# Set edge width based on weight
E(cs_net)$width <- log(E(cs_net)$weight)/10
E(cs_net)$edge.color <- "gray80"

# plot
png("../figures/fig-app-clickstream_network_graph_climate_2025.png", 1500, 2000)
par(oma=c(0,0,0,0) + .5)
par(mar=c(0,0,0,0))
set.seed(123)
plot.igraph(cs_net, vertex.label = ifelse(degree(cs_net) > 25, str_replace_all(V(cs_net)$name, "_", " "), NA), layout= layout.by.attr(cs_net, wc=1), vertex.label.cex = 3, vertex.label.dist = 1, vertex.label.degree = 0)
dev.off()

