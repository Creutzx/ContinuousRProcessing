library(tidyverse)
library(lubridate)
library(openxlsx)

filepath <- file.choose()

sumstat.file <- read.csv(filepath, stringsAsFactors = FALSE) 

filtered.means <- sumstat.file %>%
  filter(CharID == "DO",
         StatisticalBasis == "Daily Mean",
         ORDEQ_DQL == "A" | ORDEQ_DQL == "B"
         )
#create list for getting data out of loop
DO_30_mean <- list()


# 30 day means ------------------------------------------------------------

for (i in 1:length(unique(filtered.means$SiteID))){
  
  station <- unique(filtered.means$SiteID)[[i]]
  
  filtered.means.station <- filtered.means %>%
    filter(SiteID == station) %>%
    mutate(startdate7 = as.Date(mdy(ActStartDate)) - 7,
      startdate30 = as.Date(mdy(ActStartDate)) -30)
  
  
  for(l in 1:nrow(filtered.means.station)){
    
    
    start30 <- filtered.means.station$startdate30[l]
    end30 <- mdy(filtered.means.station$ActStartDate[l])
    
    station_30day <- filtered.means.station %>%
      filter(mdy(ActStartDate) <= end30 & mdy(ActStartDate) >= start30) 
    
    ma.mean30 <- ifelse(length(unique(station_30day$ActStartDate)) >= 29, mean(station_30day$Result), NA )
    new.dql <- max(station_30day$ORDEQ_DQL)
    new.cmt <- first(station_30day$DEQ_RsltComment[station_30day$ORDEQ_DQL == max(station_30day$ORDEQ_DQL)])
    
    
    filtered.means.station[l,"new.Res"] <-  ifelse(l >= 30, ma.mean30, NA)
    filtered.means.station[l,"new.dql"] <- ifelse(l >= 30, new.dql, NA)
    filtered.means.station[l,"new.cmt"] <- ifelse(l >= 30, new.cmt, NA)
    
  } #end of 30day loop

  DO_30_mean[[i]] <- filtered.means.station
}



# 7 day means -------------------------------------------------------------

DO_7_mean <- list()
for (i in 1:length(unique(filtered.means$SiteID))){
  
  station <- unique(filtered.means$SiteID)[[i]]
  
  filtered.means.station <- filtered.means %>%
    filter(SiteID == station) %>%
    mutate(startdate7 = as.Date(mdy(ActStartDate)) - 7,
           startdate30 = as.Date(mdy(ActStartDate)) -30)


for(j in 1:nrow(filtered.means.station)){
  
  
  start7 <- filtered.means.station$startdate7[j]
  end7 <- mdy(filtered.means.station$ActStartDate[j])
  
  station_7day <- filtered.means.station %>%
    filter(mdy(ActStartDate) <= end7 & mdy(ActStartDate) >= start7) 
  
  ma.mean7 <- ifelse(length(unique(station_7day$ActStartDate)) >= 6, mean(station_7day$Result), NA )
  new.dql <- max(station_7day$ORDEQ_DQL)
  new.cmt <- first(station_7day$DEQ_RsltComment[station_7day$ORDEQ_DQL == max(station_7day$ORDEQ_DQL)])
  
  
  filtered.means.station[j,"new.Res"] <- ifelse(j >= 7, ma.mean7, NA )
  filtered.means.station[j,"new.dql"] <- ifelse(j >= 7,  new.dql, NA)
  filtered.means.station[j,"new.cmt"] <- ifelse(j >= 7, new.cmt, NA)
  
} #end of 30day loop
  
  DO_7_mean[[i]] <- filtered.means.station

}




# 7 day minimums ----------------------------------------------------------


filtered.mins <- sumstat.file %>%
  filter(CharID == "DO",
         StatisticalBasis == "Daily Minimum",
         ORDEQ_DQL == "A" | ORDEQ_DQL == "B"
  )

DO_mins <- list()

for (i in 1:length(unique(filtered.mins$SiteID))){
  
  station <- unique(filtered.mins$SiteID)[[i]]
  
  filtered.mins.station <- filtered.mins %>%
    filter(SiteID == station) %>%
    mutate(startdate7 = as.Date(mdy(ActStartDate)) - 7,
           startdate30 = as.Date(mdy(ActStartDate)) -30)
  
  for(j in 1:nrow(filtered.mins.station)){
    
    
    start7 <- filtered.mins.station$startdate7[j]
    end7 <- mdy(filtered.mins.station$ActStartDate[j])
    
    station_7day <- filtered.mins.station %>%
      filter(mdy(ActStartDate) <= end7 & mdy(ActStartDate) >= start7) 
    
    ma.mean7 <- ifelse(length(unique(station_7day$ActStartDate)) >= 6, mean(station_7day$Result), NA )
    new.dql <- max(station_7day$ORDEQ_DQL)
    new.cmt <- first(station_7day$DEQ_RsltComment[station_7day$ORDEQ_DQL == max(station_7day$ORDEQ_DQL)])
    
    
    filtered.mins.station[j,"new.Res"] <- ifelse(j >= 7, ma.mean7, NA )
    filtered.mins.station[j,"new.dql"] <- ifelse(j >= 7,  new.dql, NA)
    filtered.mins.station[j,"new.cmt"] <- ifelse(j >= 7, new.cmt, NA)
    
  } #end of 30day loop
  
  DO_mins[[i]] <- filtered.mins.station
  
}
  

AWQMS_reformat <- function(looplist, stat_base, time_base){

df <- bind_rows(looplist) %>%
  filter(!is.na(new.Res)) %>%
  select(X, CharID, new.Res, Unit, Method,
         RsltType ,new.dql, StatisticalBasis, RsltTimeBasis,
         new.cmt, ActivityType, SiteID, SmplColMthd, SmplColEquip, 
         SmplDepth, SmplDepthUnit, SmplColEquipComment, Samplers, SmplEquipID, Project,
         ActStartDate, ActStartTime, ActStartTimeZone, ActEndDate, ActEndTime, ActEndTimeZone, 
         AnaStartDate, AnaStartTime, AnaStartTimeZone, AnaEndDate, AnaEndTime, AnaEndTimeZone, ActComment) %>%
  mutate(StatisticalBasis = stat_base,
         RsltTimeBasis = time_base,
         new.Res = round(new.Res, 2)) %>%
  rename(Result = new.Res,
         ORDEQ_DQL = new.dql,
         DEQ_RsltComment = new.cmt)
return(df)

}

DO_7d_mins <- AWQMS_reformat(DO_mins, "7DMADMin", "7 Day")
DO_7d_means <- AWQMS_reformat(DO_7_mean, "7DMADMean", "7 Day")
DO_30d_means <- AWQMS_reformat(DO_30_mean, "30DMADMean", "30 Day")


new.file <- sumstat.file %>%
  filter(StatisticalBasis != "30DMADMin") %>%
  bind_rows(DO_7d_mins) %>%
  bind_rows(DO_7d_means) %>%
  bind_rows(DO_30d_means) %>%
  arrange(SiteID, ActStartDate, ActStartTime)

write.xlsx(new.file,paste0(tools::file_path_sans_ext(filepath),"-corrected.xlsx"))
