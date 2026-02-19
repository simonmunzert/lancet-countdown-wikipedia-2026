## --------------------------------------------------------------------
## The Lancet Countdown: Tracking Progress on Health and Climate Change
## Indicators of Public Engagement in Health and Climate Change
## Simon Munzert
## --------------------------------------------------------------------


## load packages and functions --------
source("packages.r")
source("functions.r")

# install.packages("pageviews_0.5.0.tar.gz", repos = NULL)
# devtools::install_github("petermeissner/wikipediatrend")


# generate and save seed keywords --------
climate_keywords <- c("climate change", "changing climate", "climate emergency", "climate action", "climate crisis", "climate decay", "global warming", "green house", "extreme temperature", "temperature record", "extreme weather", "global environmental change", "climate variability",	"greenhouse", "greenhouse-gas", "low carbon", "ghge", "ghges", "renewable energy",	" carbon emission", "carbon emissions", "carbon dioxide", "carbon-dioxide", "co2 emission"," co2 emissions", "climate pollutant", "climate pollutants",	"decarbonization", "decarbonisation", "carbon neutral", "carbon-neutral",	"carbon neutrality", "climate neutrality", "net-zero", "net zero") # taken out because of poor fit: "temperature" (replaced with "extreme temperature" and "temperature record")

climate_keywords_extended <- c("climate change", "changing climate", "climate emergency", "climate action", "climate crisis", "climate decay", "global warming", "green house", "extreme temperature", "temperature record", "extreme weather", "global environmental change", "climate variability",	"greenhouse", "greenhouse-gas", "low carbon", "ghge", "ghges", "renewable energy",	"carbon emission", "carbon dioxide", "carbon-dioxide", "co2 emission", "climate pollutant", "decarbonization", "decarbonisation", "carbon neutral", "carbon-neutral",	"carbon neutrality", "climate neutrality", "net-zero", "net zero",
"ipcc", "sphere", "climat", "sea ice", "sea level", "co2", "glacial", "ozone", "green new")


health_keywords <- c("malaria", "diarrhoea", "infection", "disease", "diseases", "sars", "measles", 
															 "pneumonia", "epidemic", "epidemics", "pandemic", "pandemics", "epidemiology", 
															 "healthcare", "health", "mortality", "morbidity", "nutrition" , "illness", 
															 "illnesses", "ncd", "ncds", "air pollution", "nutrition", "malnutrition", 
															 "malnourishment", "mental disorder", "mental disorders", "stunting")

health_keywords_extended <- c("malaria", "diarrhoea", "infection", "disease", "diseases", "sars", "measles",  "pneumonia", "epidemic", "epidemics", "pandemic", "pandemics", "epidemiology",  "healthcare", "health", "mortality", "morbidity", "nutrition" , "illness",  "illnesses", "ncd", "ncds", "air pollution", "nutrition", "malnutrition", "malnourishment", "mental disorder", "mental disorders", "stunting",
										 "epidemy", "public health", "health care", "infectious", "non-communicable disease", "noncommunicable disease", "communicable disease", "syndrome", "diagnosis", "psychiatric", "epidemiolog", "disorder", "pediatric", "osis", "itis", "icide", "hunger", "fever", "asthma", "cancer")


gender_keywords <- c("women", "men", "gender", "sex")

covid_keywords <- c("covid", "coronavirus")

save(climate_keywords, 
     climate_keywords_extended,
     health_keywords, 
     health_keywords_extended,
		 gender_keywords,
		 covid_keywords,
     file = "../data/climate_health_keywords.RData")



