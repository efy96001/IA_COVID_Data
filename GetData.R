library(dplyr)
#library(gghighlight)
library(ggplot2)
#library(ggrepel)
library(lubridate)
#library(purrr)
library(readr)
library(scales)
library(sf)
library(tidycensus)
library(readr)

#Raw Data URL's from Github
url.county <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
url.state  <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

#Load Data
county_data <- read_csv(url.county)#%>%
  #mutate(date=as.Date(date),
   #      GEOID=as.character(fips))
state_data <- read_csv(url.state)

#Dates
current.filter <- max(county_data$date)
currentThrough <- max(county_data$date)
currentThrough <- format(currentThrough,"%B %d, %Y")

# currentCases <- county_data %>%
#   filter(date==current.filter)%>%
#   group_by(state)%>%
#   summarize(Cases=sum(cases),
#             Deaths=sum(deaths))%>%
#   arrange(-Cases)

#Iowa Area
iowa <- county_data %>%
  filter(state=='Iowa')%>%
  group_by(date)%>%
  summarize(confirmed=sum(cases),
            fatalities=sum(deaths))
ggplot(iowa, aes(date,confirmed))+
  geom_line()
ggplot(iowa,aes(date,fatalities))+
  geom_line()
##
# add rates
# get population data
## county populations from ACS for IA
county_pop <- get_acs(geography = "county",  year = 2018,
                      variables = "B01001_001") %>%
  select(GEOID, estimate) %>%
  rename(total_pop = estimate)

# join population to counts and calculate rate
county_data <- county_data %>%
  left_join(county_pop, by = c("fips" = "GEOID")) %>%
  mutate(
    confirmed_rate = cases/total_pop*1000,
    mortality_rate = deaths/total_pop*1000,
    case_fatality_rate = deaths/cases*100) 

# create days from 10th confirmed infection data, county-level data
county_confirmed_days <- county_data %>%
  filter(cases >= 10) %>%
  filter(county != "Unassigned") %>%
  arrange(date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(date-first_date+1)) %>%
  select(day, date, fips, county, state,  
         cases, confirmed_rate) %>%
  arrange(state, county, day) 

county_confirmed_days %>% mutate(NM=paste(county,state))%>%
  ggplot(aes(date,cases,group=NM))+
  geom_line()

#Population vs. Rate
county_data %>% filter(date==current.filter)%>%
  ggplot(aes(confirmed_rate,total_pop))+
  geom_point(aes(color=state))+
  geom_smooth()+
  scale_y_continuous(labels = comma)


# create days from 5th death data, county-level data
county_death_days <- county_data %>%
  filter(deaths >= 10) %>%
  filter(county != "Unassigned") %>%
  arrange(date) %>%
  group_by(state, county) %>%
  mutate(first_date = first(date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, geoid, county, state, last_update, deaths, 
         mortality_rate, case_fatality_rate) %>%
  arrange(state, county, day)

# create days from 10th confirmed infection data, state-level data
state_data %>%
  filter(confirmed >= 10) %>%
  arrange(report_date) %>%
  group_by(state) %>%
  mutate(first_date = first(report_date)) %>%
  ungroup() %>%
  mutate(day = as.numeric(report_date-first_date+1)) %>%
  select(day, report_date, state, last_update, 
         confirmed, confirmed_rate) %>%
  arrange(state, day) -> state_confirmed_days

# create days from fifth death data, state-level data
# state_data %>%
#   filter(deaths >= 10) %>%
#   arrange(report_date) %>%
#   group_by(state) %>%
#   mutate(first_date = first(report_date)) %>%
#   ungroup() %>%
#   mutate(day = as.numeric(report_date-first_date+1)) %>%
#   select(day, report_date, state, last_update, deaths, 
#          mortality_rate, case_fatality_rate) %>%
#   arrange(state, day) -> state_death_days
# 
# # clean-up
# rm(state_pop)
# 
# # export
# write_csv(state_data, "data/state/state_full.csv")
# write_csv(state_confirmed_days, "data/state/state_confirm.csv")
# write_csv(state_death_days, "data/state/state_death.csv")
# 
# write_csv(county_data, "data/county/county_full.csv")
# write_csv(county_confirmed_days, "data/county/county_confirm.csv")
# write_csv(county_death_days, "data/county/county_death.csv")
