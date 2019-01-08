
require(rgdal)
require(RODBC)
library(tidyverse)
library(openxlsx)
library(readxl)
library(zoo)

AWQMSOrganization = "ODA(NOSTORETID)"

#disable scientific notation 
options(scipen = 999)



# Choose file to of stations 
filepath = file.choose()
stations_to_import = read.csv(filepath, stringsAsFactors = FALSE)

stations_to_import <- stations_to_import %>%
  mutate(org_id = AWQMSOrganization) %>%
  select(org_id, Station)




# Get stations in AWQMS ---------------------------------------------------

awqms.sql <- odbcConnect("AWQMS")

AWQMS_stations <- sqlQuery(awqms.sql, "SELECT        organization.org_id, monitoring_location.mloc_id
FROM            monitoring_location INNER JOIN
                         organization ON monitoring_location.org_uid = organization.org_uid")

odbcClose(awqms.sql)


toAdd <- stations_to_import %>%
  anti_join(AWQMS_stations, by = c("org_id", "Station" = "mloc_id"))



# Get station info from stations database ---------------------------------

sta.sql = odbcConnect('Stations')

# Connect to stations database --------------------------------------------

#pull in stations table
Stations = sqlFetch(sta.sql, "VWStationsFinal") 
odbcClose(sta.sql)


Add = toAdd %>%
  left_join(Stations, by = c("Station"="MLocID")) %>%
  rename(monloc = Station) %>%
  mutate(Country = "US") %>%
  #HUC12 = as.character(HUC12),
  #Reachcode = as.character(Reachcode)) %>%
  mutate(desc = "") %>%
  select(monloc,OrgID,StationDes,desc,MonLocType,COUNTY,STATE,Country,HUC8,HUC12,
         TribalLand,TribalName,Created_Date,T_R_S,Lat_DD,Long_DD,Datum,CollMethod,MapScale,Comments,WellType,
         WellFormType,WellAquiferName,WellDepth,WellDepthUnit,AltLocID,AltLocName,EcoRegion3,
         EcoRegion4,Reachcode,GNIS_Name,AU_ID)
class(Add$HUC12) <- c("NULL", "number")
class(Add$Reachcode) <- c("NULL", "number")

### write to folder, clean-up data file and use the import config 
#write.csv(Add,"IVSWCD_5sept18.csv",row.names = FALSE, na = "")
write.xlsx(Add,"V:/ODA/2017/LincolnSWCD/Routput/ODALincolnSites.xlsx")

