#' Questions:
#' 1. How does the US compare to South Korea?
#' 2. How does California compare to Alameda County, California?
#' a. Per month, how do the mobility attributes change in both samples?
#' b. Compare activities that are encouraged and discouraged by the pandemic.
#' Source of Data: https://www.google.com/covid19/mobility/

library(tidyverse)
library(gt)
library(lubridate)
library(ggplot2)
library(reshape2)
library(sjmisc)

#Loading in the data
korea_mobility <- read.csv("/Users/mileslee/Desktop/DDD-I21/Region_Mobility_Report_CSVs/2020_KR_Region_Mobility_Report.csv")
united_states_mobility <- read.csv("/Users/mileslee/Desktop/DDD-I21/Region_Mobility_Report_CSVs/2020_US_Region_Mobility_Report.csv")
#Make a copy of the data so you don't override the original.
kr <- korea_mobility
us <- united_states_mobility
#taking a peak at at the individual mobility data sets. 
colnames(kr)
kr[1:3,]
nrow(kr)
colnames(us)
us[1:3,]
nrow(us)

#Check to see if the date data is in the date format. If not, convert to date format.
us$date[1]
class(us$date[1])
kr$date[1]
class(kr$date[1])
us$formatted_date <- as.Date(us$date, format = "%Y-%m-%d")
class(us$formatted_date[1])
kr$formatted_date <- as.Date(kr$date, format = "%Y-%m-%d")
class(kr$formatted_date[1])

#Make sub-dataframes (by rows and cols) and merge dataframes in order to capture the data used to answer the questions above. 
vars_interest <- c("formatted_date", "country_region", "sub_region_1","sub_region_2", "retail_and_recreation_percent_change_from_baseline", 
                   "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", 
                   "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline",
                   "residential_percent_change_from_baseline")
#Data from only specific columns and rows about mobility of US as a country.
us_country_df <- us[vars_interest] %>% filter(us$sub_region_1 == "")
#Data from only specific columns and rows about mobility of California. This excludes data about each county in California. 
ca_state_df <- us[vars_interest] %>% filter(us$iso_3166_2_code == "US-CA")
#Data from only specific columns and rows about mobility of Korea as a country.
kr_country_df <- kr[vars_interest] %>% filter(kr$metro_area == "")
#Data from only specific columns and rows about mobility of Alameda county.
al_county_df <- us[vars_interest] %>% filter(us$census_fips_code == 6001)

#Merge the two counties' data into one data frame. Chose to merge data with dates that are only included in both data sets. This way comparison is more simple.
us_kr_df <- merge(us_country_df,kr_country_df, by = "formatted_date", suffixes = c("_us", "_kr"))
al_ca_df <- merge(al_county_df, ca_state_df, by = "formatted_date", suffixes = c("_al", "_ca"))
#checking success of merge and any unwanted results of the merge. 
ncol(us_kr_df) == 2*length(vars_interest) - 1
ncol(al_ca_df) == 2*length(vars_interest) - 1
nrow(us_kr_df) == nrow(subset(us, sub_region_1 == ""))
nrow(al_ca_df) == nrow(subset(us, sub_region_1 == ""))
#All TRUE results demonstrate that the merged data frames have the correct amount of variables and rows. 

#Missing Data: Identifying, flagging, and handling missing data.
#A removal of columns that aren't significant to the project questions eliminates the need to take care of most missing values in the data frames. 
missing_check_us_df <- sapply(us, function(x) sum(is.na(x)))
missing_check_kr_df <- sapply(kr, function(x) sum(is.na(x)))
missing_check_us_kr_df <- sapply(us_kr_df, function(x) sum(is.na(x)))
missing_check_la_ca_df <- sapply(al_ca_df, function(x) sum(is.na(x)))
#The complete us mobility data set has many missing values and these values cannot reasonably be extrapolated. However, I have only chosen to
#look at data from the us mobility data set which is complete.

#General Cleaning and Formatting: 
#Removing the the sub_region_1 and sub_region_2 cols instead of making so they all contain NA values because the columns don't provide any useful information for the specific data set. 
#sub_region_1 and sub_region_2 cols are primarily used to filter for specific data in the original data.
us_kr_df <- us_kr_df %>% select(-"sub_region_1_us", -"sub_region_2_us", -"sub_region_1_kr", -"sub_region_2_kr")
#I keep the sub_region_1 and sub_region_2 columns in the la_ca_df data frame since they provide information about where in the original data set the current one is a subset. 

#Variable Creation and Coding: factored vars, binary flags, IDs that hold information...
#Creating a row ID
us_kr_df$id <- 1:nrow(us_kr_df)
al_ca_df$id <- 1:nrow(al_ca_df)
#Binary flag for negative change: 1 means neg change, 0 means no or positive change
us_kr_df <- us_kr_df %>% 
            mutate(retail_and_recreation_us_neg_change = ifelse(retail_and_recreation_percent_change_from_baseline_us < 0, 1, 0),
                    grocery_and_pharmacy_us_neg_change = ifelse(grocery_and_pharmacy_percent_change_from_baseline_us < 0, 1, 0),
                    parks_us_neg_change = ifelse(parks_percent_change_from_baseline_us < 0, 1, 0),
                    transit_stations_us_neg_change = ifelse(transit_stations_percent_change_from_baseline_us < 0, 1, 0),
                    workplaces_us_neg_change = ifelse(workplaces_percent_change_from_baseline_us < 0, 1, 0),
                    residential_us_neg_change = ifelse(residential_percent_change_from_baseline_us < 0, 1, 0),
                    retail_and_recreation_kr_neg_change = ifelse(retail_and_recreation_percent_change_from_baseline_kr < 0, 1, 0),
                    grocery_and_pharmacy_kr_neg_change = ifelse(grocery_and_pharmacy_percent_change_from_baseline_kr < 0, 1, 0),
                    parks_kr_neg_change = ifelse(parks_percent_change_from_baseline_kr < 0, 1, 0),
                    transit_stations_kr_neg_change = ifelse(transit_stations_percent_change_from_baseline_kr < 0, 1, 0),
                    workplaces_kr_neg_change = ifelse(workplaces_percent_change_from_baseline_kr < 0, 1, 0),
                    residential_kr_neg_change = ifelse(residential_percent_change_from_baseline_kr < 0, 1, 0))
#Note: using a loop would be more advanced and possibly better, however, would be more difficult and possibly not worth the consideration for this case.

#Dataframe with negative change count per type of mobility and average change. This is for creating a table.
us_kr_neg_pos_df <- data.frame("mobility_type" = c("retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces", "residential"))
us_kr_neg_pos_df$percent_of_negative_days_us <- round(100 * colSums(us_kr_df %>% select(retail_and_recreation_us_neg_change, 
                                                                                        grocery_and_pharmacy_us_neg_change, 
                                                                                        parks_us_neg_change, 
                                                                                        transit_stations_us_neg_change, 
                                                                                        workplaces_us_neg_change, 
                                                                                        residential_us_neg_change)) / nrow(us_kr_df)) 
us_kr_neg_pos_df$percent_of_negative_days_kr <- round(100 * colSums(us_kr_df %>% select(retail_and_recreation_kr_neg_change, 
                                                                                        grocery_and_pharmacy_kr_neg_change, 
                                                                                        parks_kr_neg_change, 
                                                                                        transit_stations_kr_neg_change, 
                                                                                        workplaces_kr_neg_change, 
                                                                                        residential_kr_neg_change)) / nrow(us_kr_df)) 
us_kr_neg_pos_df$percent_of_positive_days_us <- 100 - us_kr_neg_pos_df$percent_of_negative_days_us
us_kr_neg_pos_df$percent_of_positive_days_kr <- 100 - us_kr_neg_pos_df$percent_of_negative_days_kr

#averages and sum of each variable per month. and total change per month too.
us_kr_avg_percent_change_per_month_df <- us_kr_df %>% 
  group_by(year(us_kr_df$formatted_date),month(us_kr_df$formatted_date)) %>% 
  summarise(retail_and_recreation_us = round(mean(retail_and_recreation_percent_change_from_baseline_us)),
            retail_and_recreation_kr = round(mean(retail_and_recreation_percent_change_from_baseline_kr)),
            grocery_and_pharmacy_us = round(mean(grocery_and_pharmacy_percent_change_from_baseline_us)),
            grocery_and_pharmacy_kr = round(mean(grocery_and_pharmacy_percent_change_from_baseline_kr)),
            parks_us = round(mean(parks_percent_change_from_baseline_us)), 
            parks_kr = round(mean(parks_percent_change_from_baseline_kr)), 
            transit_station_us = round(mean(transit_stations_percent_change_from_baseline_us)), 
            transit_station_kr = round(mean(transit_stations_percent_change_from_baseline_kr)), 
            workplaces_us = round(mean(workplaces_percent_change_from_baseline_us)), 
            workplaces_kr = round(mean(workplaces_percent_change_from_baseline_kr)), 
            residential_us = round(mean(residential_percent_change_from_baseline_us)),
            residential_kr = round(mean(residential_percent_change_from_baseline_kr)),
            all_us = round((retail_and_recreation_us + grocery_and_pharmacy_us + parks_us + transit_station_us + workplaces_us + residential_us)/6),
            all_kr = round((retail_and_recreation_kr + grocery_and_pharmacy_kr + parks_kr + transit_station_kr + workplaces_kr + residential_kr)/6)) 
us_kr_avg_percent_change_per_month_df$change_us <- ""
levels(us_kr_avg_percent_change_per_month_df$change_us) <- c(levels(us_kr_avg_percent_change_per_month_df$change_us), "High")
levels(us_kr_avg_percent_change_per_month_df$change_us) <- c(levels(us_kr_avg_percent_change_per_month_df$change_us), "Low")
us_kr_avg_percent_change_per_month_df$change_us <- ifelse(abs(us_kr_avg_percent_change_per_month_df$all_us) > 15, "High", "Low")
us_kr_avg_percent_change_per_month_df$change_kr <- ""
levels(us_kr_avg_percent_change_per_month_df$change_kr) <- c(levels(us_kr_avg_percent_change_per_month_df$change_kr), "High")
levels(us_kr_avg_percent_change_per_month_df$change_kr) <- c(levels(us_kr_avg_percent_change_per_month_df$change_kr), "Low")
us_kr_avg_percent_change_per_month_df$change_kr <- ifelse(abs(us_kr_avg_percent_change_per_month_df$all_kr) > 15, "High", "Low")

al_ca_avg_percent_change_per_month_df <- al_ca_df %>% 
  group_by(year(al_ca_df$formatted_date),month(al_ca_df$formatted_date)) %>% 
  summarise(retail_and_recreation_al = round(mean(retail_and_recreation_percent_change_from_baseline_al)),
            retail_and_recreation_ca = round(mean(retail_and_recreation_percent_change_from_baseline_ca)),
            grocery_and_pharmacy_al = round(mean(grocery_and_pharmacy_percent_change_from_baseline_al)),
            grocery_and_pharmacy_ca = round(mean(grocery_and_pharmacy_percent_change_from_baseline_ca)),
            parks_al = round(mean(parks_percent_change_from_baseline_al)), 
            parks_ca = round(mean(parks_percent_change_from_baseline_ca)), 
            transit_station_al = round(mean(transit_stations_percent_change_from_baseline_al)), 
            transit_station_ca = round(mean(transit_stations_percent_change_from_baseline_ca)), 
            workplaces_al = round(mean(workplaces_percent_change_from_baseline_al)), 
            workplaces_ca = round(mean(workplaces_percent_change_from_baseline_ca)), 
            residential_al = round(mean(residential_percent_change_from_baseline_al)),
            residential_ca = round(mean(residential_percent_change_from_baseline_ca)),
            all_al = round((retail_and_recreation_al + grocery_and_pharmacy_al + parks_al + transit_station_al + workplaces_al + residential_al)/6),
            all_ca = round((retail_and_recreation_ca + grocery_and_pharmacy_ca + parks_ca + transit_station_ca + workplaces_ca+ residential_ca)/6))
al_ca_avg_percent_change_per_month_df$change_al <- ""
levels(al_ca_avg_percent_change_per_month_df$change_al) <- c(levels(al_ca_avg_percent_change_per_month_df$change_al), "High")
levels(al_ca_avg_percent_change_per_month_df$change_al) <- c(levels(al_ca_avg_percent_change_per_month_df$change_al), "Low")
al_ca_avg_percent_change_per_month_df$change_al <- ifelse(abs(al_ca_avg_percent_change_per_month_df$all_al) > 15, "High", "Low")
al_ca_avg_percent_change_per_month_df$change_ca <- ""
levels(al_ca_avg_percent_change_per_month_df$change_ca) <- c(levels(al_ca_avg_percent_change_per_month_df$change_ca), "High")
levels(al_ca_avg_percent_change_per_month_df$change_ca) <- c(levels(al_ca_avg_percent_change_per_month_df$change_ca), "Low")
al_ca_avg_percent_change_per_month_df$change_ca <- ifelse(abs(al_ca_avg_percent_change_per_month_df$all_ca) > 15, "High", "Low")

#Table 1: us_kr_neg_pos_df (US vs KR)
us_kr_neg_pos_df %>% 
  gt(rowname_col = "mobility_type") %>%
  tab_header(title = "Percent of Days (in 12 months) of Positive and Negative Change in Mobility") %>%
  cols_label(percent_of_negative_days_us = "Negative in US", 
             percent_of_negative_days_kr = "Negative in Korea", 
             percent_of_positive_days_us = "Positive in US", 
             percent_of_positive_days_kr = "Positive in Korea") 

#Table 2: us_kr_avg_percent_change_per_month_df (US vs KR)
us_kr_avg_percent_change_per_month_df$date_names <- c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                                                       "Oct", "Nov", "Dec", "Jan")
us_kr_avg_percent_change_per_month_df %>% 
  group_by(`year(us_kr_df$formatted_date)`, date_names) %>% 
  select(all_us, all_kr, change_us, change_kr) %>%
  gt() %>%
  tab_header(title = "Average Percent Change from Baseline of Each Type of Mobility Per Month in US and Korea") %>%
  cols_label(all_us = "US-All",
             all_kr = "Korean-All",
             change_us = "US Overall Change (Qualitative)",
             change_kr = "Korean Overall Change (Qualitative)")

#Table 3: us_kr_avg_percent_change_per_month_df (Al vs CA)
al_ca_avg_percent_change_per_month_df$date_names <- c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                                                      "Oct", "Nov", "Dec", "Jan")
al_ca_avg_percent_change_per_month_df %>% 
  group_by(`year(al_ca_df$formatted_date)`, date_names) %>% 
  select(all_al, all_ca, change_al, change_ca) %>%
  gt() %>%
  tab_header(title = "Average Percent Change from Baseline of Each Type of Mobility Per Month in Alameda County, CA and California") %>%
  cols_label(all_al = "Alameda County-All",
             all_ca = "California-All",
             change_al = "Alameda County Overall Change (Qualitative)",
             change_ca = "California Overall Change (Qualitative)")
  
#Graph 1: 
us_kr_avg_percent_change_per_month_df$date_numbers <- c(2,3,4,5,6,7,8,9,10,11,12,1)
temp_df <- melt(us_kr_avg_percent_change_per_month_df, id.vars = 'date_numbers', variable.name = 'series') %>% 
  filter(series != "year(us_kr_df$formatted_date)") %>%
  filter(series != "month(us_kr_df$formatted_date)") %>%
  filter(series != "change_us") %>%
  filter(series != "change_kr") %>%
  filter(series != "date_names")
temp_df$series <- as.character(temp_df$series)
temp_df$value <- as.integer(temp_df$value)
temp_df$location <- ifelse(grepl("_us", temp_df$series), "US", "South Korea")
ggplot(data = temp_df  %>% filter(series == "retail_and_recreation_us" | series == "retail_and_recreation_kr"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Retail and Recreation Mobility Per Month in US and Korea, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan"))
ggplot(data = temp_df  %>% filter(series == "grocery_and_pharmacy_us" | series == "grocery_and_pharmacy_kr"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Grocery and Pharmacy Mobility Per Month in US and Korea, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan"))
ggplot(data = temp_df  %>% filter(series == "parks_us" | series == "parks_kr"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Parks Mobility Per Month in US and Korea, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan"))
ggplot(data = temp_df  %>% filter(series == "transit_station_us" | series == "transit_station_kr"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Transit Stations Mobility Per Month in US and Korea, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan"))
ggplot(data = temp_df  %>% filter(series == "workplaces_us" | series == "workplaces_kr"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Workplaces Mobility Per Month in US and Korea, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan"))
ggplot(data = temp_df  %>% filter(series == "residential_us" | series == "residential_kr"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Residential Mobility Per Month in US and Korea, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan"))
ggplot(data = temp_df  %>% filter(series == "all_us" | series == "all_kr"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of All Types of Mobility Per Month in US and Korea, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan"))


#Graph 2:
al_ca_avg_percent_change_per_month_df$date_numbers <- c(2,3,4,5,6,7,8,9,10,11,12,1)
temp_df <- melt(al_ca_avg_percent_change_per_month_df, id.vars = 'date_numbers', variable.name = 'series') %>% 
  filter(series != "year(al_ca_df$formatted_date)") %>%
  filter(series != "month(al_ca_df$formatted_date)") %>%
  filter(series != "change_al") %>%
  filter(series != "change_ca") %>%
  filter(series != "date_names")
temp_df$series <- as.character(temp_df$series)
temp_df$value <- as.integer(temp_df$value)
temp_df$location <- ifelse(grepl("_al", temp_df$series), "alameda", "california")
ggplot(data = temp_df %>% filter(series == "retail_and_recreation_al" | series == "retail_and_recreation_ca"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Retail and Recreation Mobility Per Month in Alameda County, CA and California, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan")) 
ggplot(data = temp_df %>% filter(series == "grocery_and_pharmacy_al" | series == "grocery_and_pharmacy_ca"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Grocery and Pharmacy Mobility Per Month in Alameda County, CA and California, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan")) 
ggplot(data = temp_df %>% filter(series == "parks_al" | series == "parks_ca"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Parks Mobility Per Month in Alameda County, CA and California, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan")) 
ggplot(data = temp_df %>% filter(series == "transit_station_al" | series == "transit_station_ca"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Transit Stations Mobility Per Month in Alameda County, CA and California, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan")) 
ggplot(data = temp_df %>% filter(series == "workplaces_al" | series == "workplaces_ca"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Workplaces Mobility Per Month in Alameda County, CA and California, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan")) 
ggplot(data = temp_df %>% filter(series == "residential_al" | series == "residential_ca"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from Baseline of Residential Mobility Per Month in Alameda County, CA and California, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan")) 
ggplot(data = temp_df %>% filter(series == "all_al" | series == "all_ca"), aes(x = date_numbers, y = value, color = location)) + 
  geom_line() +
  xlab("Month") +
  ylab("Percent Change From Baseline") +
  ggtitle("Average Percent Change from All Types of Mobility Per Month in Alameda County, CA and California, 2020-2021") + 
  scale_x_discrete(name = "Month",
                   limits = c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec", "Jan")) 
