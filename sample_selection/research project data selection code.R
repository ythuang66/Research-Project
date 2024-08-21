# packages 
install.packages("rsample")
library(dplyr)
library(tidyverse)
library(rsample)
data <- read.csv("filtered_seychelles_parrots.csv")
# explore the filtered metadata
site_data_counts <- data %>%
  group_by(site_code) %>%
  summarise(Count = n())
print(site_data_counts)

time_counts <- data %>%
  group_by(recording_time)%>%
  summarise(Count = n())
print(time_counts, n = Inf)

# subset the data according to their locations
FP1 <- subset(data, site_code == "FP1")
FP2 <- subset(data, site_code == "FP2")
GN1 <- subset(data, site_code =="GN1")
VDM1 <- subset(data, site_code == "VDM1")

# random sampling 2000 data to be representative for the project, considering site, year, season and time
# use dplyr
# set the year, season and time of the day
data$year <- year(as.Date(data$recording_date))

# derive 'season' from 'recording_date'
data$season <- ifelse(month(data$recording_date) %in% c(1, 2, 3), "quarter1",
                      ifelse(month(data$recording_date) %in% c(4, 5, 6), "quarter2",
                             ifelse(month(data$recording_date) %in% c(7, 8, 9), "quarter3",
                                    "quarter4")))
# categorise the recording time into morning, midday, afternoon
# standarise the time format
data$recording_time <- gsub("^([0-9]):", "0\\1:", data$recording_time)  # Add leading zero to single-digit hours
data$recording_time <- sub(":$", "", data$recording_time)
# Categorize 'recording_time'
data$time_category <- cut(as.integer(substring(data$recording_time, 1, 2)),
                          breaks=c(5, 9, 13, 19),
                          labels=c("Morning", "Midday", "Afternoon"),
                          include.lowest=TRUE)
# export csv first
write.csv(data, "syechelles_parrots_filtered2.csv", row.names = FALSE)
# Function to perform stratified sampling for each site_code
data$strata <- interaction(data$year, data$season, data$time_category)
# Function to perform stratified sampling ensuring site_code is retained
stratified_sampling <- function(df, size) {
  df %>%
    group_by(site_code) %>%
    group_map(~ {
      # Perform the sampling
      sampled <- initial_split(.x, prop = size / nrow(.x), strata = "strata") %>% training()
      # Explicitly ensure site_code is retained
      sampled$site_code <- unique(.x$site_code)
      return(sampled)
    }, .keep = TRUE) %>%  # Ensure group_by column is kept
    bind_rows()
}

# Execute stratified sampling with 500 samples per site_code
sampled_data <- stratified_sampling(data, 500)
# since warning on too little data, check the data 
table(data$strata)

time_c <- sampled_data %>%
  group_by(time_category)%>%
  summarise(Count = n())
print(time_c, n = Inf)

season_c <- sampled_data %>%
  group_by(season)%>%
  summarise(Count = n())
print(season_c, n = Inf)

year_c <-sampled_data %>%
  group_by(year)%>%
  summarise(Count = n())
print(year_c)


write.csv(data, "sampled_syechelles_parrots.csv", row.names = FALSE)

site_c <-sampled_data %>%
  group_by(site_code)%>%
  summarise(Count = n())
print(site_c)

