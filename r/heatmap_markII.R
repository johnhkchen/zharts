library(ggplot2)
library(maps)
library(dplyr)
library(mefa4)

setwd("C:/Users/nssch/OneDrive/Documents/JN Project/")

#General Processing
virus_cases_counties <- read.csv("us-counties.csv")
virus_cases_counties$county <- tolower(virus_cases_counties$county)
virus_cases_counties$state <- tolower(virus_cases_counties$state)
virus_cases_counties$date <- as.Date(virus_cases_counties$date)

#Matching column names to virus data set
usa_map <- map_data("county")
names(usa_map)[names(usa_map)=="subregion"] <- "county"
names(usa_map)[names(usa_map)=="region"] <- "state"

#Adding in FIPS
usa_map$polyname <- as.character(paste(usa_map$state,usa_map$county,sep = ","))
usa_map <- left_join(usa_map,county.fips,by="polyname")

#Variables used to control date range
first_day <- min(virus_cases_counties$date)
last_day <- max(virus_cases_counties$date)
date_range <- seq.Date(first_day,last_day,by="days")
colnames(date_range) <- "date"

#Basic growth calculation function
growth_calculate <- function(time_1,time_0,interval){
  if(is.nan(time_1/time_0)==FALSE){
    growth <- as.numeric((time_1/time_0)^(1/interval)-1)
    return(growth)} else {
      growth <- as.numeric(0)
      return(growth)
    }
}

#Function which passes values to growth_calculate which also takes care of missing values in the data.
growth_process <- function(fip_code,date_1){
  #Thus far, the interval is always 1. I have left off the more eleaborate solution in the name of time.
  interval <- 1
  if(date_1 %notin% cases_working_set$date[cases_working_set$fips == fip_code]){
    growth_rate <- 0
  } else {
          if((date_1-1) %in% cases_working_set$date[cases_working_set$fips == fip_code]){
          growth_rate <- growth_calculate(cases_working_set$cases[cases_working_set$date == date_1 & cases_working_set$fips == fip_code],
                                    cases_working_set$cases[cases_working_set$date == (date_1 - 1) & cases_working_set$fips == fip_code],
                                    interval)
          } else {
                  growth_rate <- 0
                  }
          }
  return(growth_rate)
}

#Function to calculate the 4 day moving average; the function heavily relies upon the structure of cases_working_set
growth_4day_average <- function(fip_code,date_1,growth_rate){
  if(date_1 %notin% cases_working_set$date[cases_working_set$fips==fip_code]){
    fourdayaverage <- 0
    return(fourdayaverage)
  } else {
      if((date_1-3) %in% cases_working_set$date[cases_working_set$fips==fip_code]){
        fourdayaverage <- mean(cases_working_set$growth[cases_working_set$fips==fip_code & cases_working_set$date == date_1],
                               cases_working_set$growth[cases_working_set$fips==fip_code & cases_working_set$date == (date_1-1)],
                               cases_working_set$growth[cases_working_set$fips==fip_code & cases_working_set$date == (date_1-2)],
                               cases_working_set$growth[cases_working_set$fips==fip_code & cases_working_set$date == (date_1-3)])
        return(fourdayaverage)
      } else {
          fourdayaverage <- cases_working_set$growth[cases_working_set$fips==fip_code & cases_working_set$date == date_1]
          return(fourdayaverage)
      }
    }
}

#Name desired counties here, lower case
desired_counties_char <- as.character(c("san francisco","marin","sonoma","napa","contra costa","san mateo","solano","alameda","santa clara"))
desired_counties <- c(desired_counties,unique(usa_map$fips[usa_map$county %in% desired_counties_char]))



#For entire state, use this one.
desired_state <- "california"
desired_counties <- unique(usa_map$fips[usa_map$state==desired_state])

#These sets will be used for the maps
cases_working_set <- data.frame(na.omit(virus_cases_counties[virus_cases_counties$fips %in% desired_counties,]))
cases_working_set <- data.frame(cases_working_set,row.names = NULL)
cases_working_set <- cases_working_set[order(cases_working_set$fips,decreasing = TRUE),]
map_working_set <-  usa_map[usa_map$fips %in% desired_counties,]
map_working_set <- map_working_set[order(map_working_set$fips,decreasing=TRUE),]

#Adding in the growth rate data
cases_working_set$growth <- mapply(growth_process,cases_working_set$fips,cases_working_set$date)
cases_working_set$average <- mapply(growth_4day_average, cases_working_set$date,cases_working_set$growth,cases_working_set$fips)


daily_heatmaps_growth <- vector('list',length = length(date_range))
daily_heatmaps_average_growth <- vector('list',length = length(date_range))
for (i in 1:length(date_range)) {
  daily_heatmaps_growth[[i]] <- local({
                          i <- i
                          ggplot(data=map_working_set)+
                          geom_polygon(aes(x=map_working_set$long,y=map_working_set$lat,group=map_working_set$fips,
                                           fill=sapply(map_working_set$fips, 
                                                       growth_process, 
                                                       date_1=date_range[i])),
                                       color="black")+
                          scale_fill_continuous(name="Infection Growth Rate", low = "white", high = "darkred",limits = c(0,2))+
                          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                                        axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
                            ggtitle(paste("Growth Rate on", toString(date_range[i]),sep=" "))
                              })
  daily_heatmaps_average_growth[[i]] <- local({
                          i <- i
                          ggplot(data=map_working_set)+
                          geom_polygon(aes(x=map_working_set$long,y=map_working_set$lat,group=map_working_set$fips,
                                           fill=mapply(growth_4day_average,date_1=date_range[i],
                                                       map_working_set$fips,
                                                       cases_working_set$growth)),
                                          color="black")+
                          scale_fill_continuous(name="Infection Growth Rate Four Day Average", low = "white", high = "darkred",limits = c(0,2))+
                          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                                axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
                            ggtitle(paste("Four Day Average Growth Rate on", toString(date_range[i]),sep=" "))
  }) 
}



