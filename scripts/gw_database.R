### Script by Linda Esteli Mendez Barrientos ###
###  Groundwater database development project ###

library(raster)
library(rgdal)
library(sp)
library(plyr)
library(dplyr)


### Getting hold of all of the water agencies in the state to create a database with all of the agencies in it

watdist <-shapefile('data/WaterDistricts/WaterDistricts.shp')
servicearea <-shapefile('data/service_areas_WBT/service_areas.shp')
watboundary <-shapefile('data/CES20_drinkingwater_boundaries/CES20_DrinkingWaterIndicator.shp')

write.csv(watdist, file = "watdist.csv",row.names=TRUE)
write.csv(servicearea, file = "servicearea.csv",row.names=TRUE)
write.csv(watboundary, file = "watboundary.csv",row.names=TRUE)

SDWIS <- read.csv('data/SDWIS Public Water System Extract 4-8-2016.csv')
SDWIS <- as.data.frame(SDWIS)
watbound <- read.csv('data/watboundary.csv')
servarea <- read.csv('data/servicearea.csv')
wd <- read.csv('data/watdist.csv')

#Getting rid of the CA before the ID code
SDWIS$PWSID <- gsub("CA", "", SDWIS$PWSID)

#Now with the same codes, maybe we can merge both databases
# Merging all PWS

# THESE CODE BELOW DID NOT WORK.

#SDWIS <- SDWIS[SDWIS$AREA %in% c("Institution","Wholesaler (Sells Water)","Interstate Carrier", 
 #                             "Service Station", "Industrial/Agricultural","Municipality")]

#SDWIS <- SDWIS %>%
 #  filter(SOURCE !="Surface Water",
  #        SOURCE !="Surface Water Purchased",     
   #       SOURCE !="Unknown"),
    #      AREA != "Mobile Home Park",
     #     AREA != "Other Area",
      #    AREA != "Other Transient Area",
       #   AREA != "Restaurant",
        #  AREA != "Secondary Residences",
         # AREA != "Day Care Center",
          #AREA != "Hotel/Motel",
          #AREA != "Mobile Home Park,Princ. Res.",
          #AREA != "Other Non-Transient Area",
      #    AREA != "Retail Employees",
      #    AREA != "Highway Rest Area",
       #   AREA != "Medical Facility",
        #  AREA != "Other Residential Area",
         # AREA != "Residential Area",
      #    AREA != "School",
       #   AREA != "Summer Camp") %>% 
#  select(PWSID, PWS_NAME, SOURCE, AREA)


SDWIS <- SDWIS %>%
  filter(SOURCE !="Surface Water") %>% 
  select(PWSID, PWS_NAME, SOURCE, AREA) #this worked, database diminished from 8302 to 7641 obs.

SDWIS <- SDWIS %>%
  filter(SOURCE !="Surface Water Purchased") %>% 
  select(PWSID, PWS_NAME, SOURCE, AREA) # now 7223 obs.

SDWIS <- SDWIS %>%
  filter(SOURCE !="Unknown") %>% 
  select(PWSID, PWS_NAME, SOURCE, AREA) #now 6821 obs.


# THIS CODE HASN'T WORKED. IT APPEARS THAT AREA IS THE ISSUE?

# AREA <- subset(SDWIS, AREA=="Institution" | AREA=="Wholesaler (Sells Water)")

#SDWIS <- SDWIS %>%
 # filter(AREA != "Mobile Home Park") %>% 
 # select(PWSID, PWS_NAME, SOURCE, AREA) # ATTENTION: NO CHANGE IN OBS.

#SDWIS <- SDWIS %>%
 # filter(AREA != "Restaurant") # ATTENTION: NO CHANGE IN OBS.

#SDWIS <- SDWIS %>% 
  #filter(AREA = "Institution", "Wholesaler (Sells Water)", "Interstate Carrier", "Service Station",
    #     "Industrial/Agricultural", "Municipality")
  
# Wanna keep the following categories in AREA:
#AREA ="Institution", "Wholesaler (Sells Water)", "Interstate Carrier", "Service Station",
# "Industrial/Agricultural", "Municipality")  

#levels.default(SDWIS$AREA)
#[1] ""                               "Day Care Center               " "Highway Rest Area             "
#[4] "Homeowners Association        " "Hotel/Motel                   " "Industrial/Agricultural       "
#[7] "Institution                   " "Interstate Carrier            " "Medical Facility              "
#[10] "Mobile Home Park              " "Mobile Home Park,Princ. Res.  " "Municipality                  "
#[13] "Other Area                    " "Other Non-Transient Area      " "Other Residential Area        "
#[16] "Other Transient Area          " "Recreation Area               " "Residential Area              "
#[19] "Restaurant                    " "Retail Employees              " "School                        "
#[22] "Secondary Residences          " "Service Station               " "Summer Camp                   "
#[25] "Wholesaler (Sells Water)      "     


servarea <- servarea %>% 
  select(pwsid, pwsname)

watbound <- watbound %>% 
  filter(WaterUsed != "Surface") %>% 
  select(PWSID, PWSName, WaterUsed)

  
agen1 <- merge(servarea, watbound, by.x=c("pwsid"), by.y=c("PWSID")) # only 1868 agencies
agen1 <- merge(servarea, watbound, by.x=c("pwsid"), by.y=c("PWSID"), all=TRUE) 
#4042 agencies but is not the sum of the two cause that will be 2642 + 2980 [1] 5622

agen2 <- merge(agen1, SDWIS, by.x=c("pwsid"), by.y=c("PWSID"), all=TRUE)
#12344 observations which is literally the sum of agen1 + SDWIS


#Uploading shapefiles from DWR database

adju <- shapefile('data/Adjudicated_Groundwater_Basins/Adjudicated_Areas.shp')
gmp <- shapefile('data/Groundwater_Management_Plan/Groundwater_Management_Plan.shp')
casgem <- shapefile('data/CASGEM_Groundwater_Basin_Prioritization/CASGEM_Groundwater_Basin_Prioritization.shp')
b118 <- shapefile('data/Bulletin_118_Groundwater_Basins_(2016)/i08_B118_CA_GroundwaterBasins.shp')
county <- shapefile('data/County_Boundary/County_Boundaries.shp')
elev_f15 <- shapefile('data/Fall_2015_Elevation_Points/F2015_WSEL_Points_20160111_014452.shp')

gw <- read.csv('data/gw.csv')

hist <- read.csv('data/hist.csv')

View(adju) #it worked!!! I can see the attributes table in R so now I can create my own database

### Creating own database - merging different variables from attribute tables

# 1. Rename variables names from database gw

gw <- rename(gw, ID = Basin.Subbasin.ID,
             basin.name = Basin.Name,
             subbasin.name = Subbasin.Name,
             hydro = Hydrologic.Name,
             area = Basin...Subbasin.Area..acre.,
             score = Total.Rank.Scoring,
             priority = Final.Priority,
             pop = Population..2010.,
             popgrow = Population.Growth..,
             well = Total.Wells,
             pubwell = Total.Public.Supply.Wells,
             gw.ac.ft = Groundwater.Volume..Ac.Ft.,
             percent.gw.sup = X..of.total.water.supply.supplied.by.groundwater,
             percent.sf.sup = X..of.total.water.supply.supplied.by.surface.water,
             irr.ac = Irrigated.Acreage)
        
             
# 2. Selecting variables for new database

data <- gw %>% 
  select(ID, basin.name, subbasin.name, hydro, area, score, priority, pop, popgrow, well, pubwell, 
             gw.ac.ft, percent.gw.sup, percent.sf.sup, irr.ac)

# 3. Create new variable 'priority' as a numeric variable with ordinal levels 1-4
# Very low 1
# Low 2
# Medium 3
# High 4

data$prior <- revalue(data$priority,
                      c("High"= "4", "Medium" = "3", "Low" = "2", "Very Low" = "1"))

levels(data$prior) <- c(4,3,2,1)

# Create georegion variable using hydro

data$georegion<-revalue(data$hydro, 
                        c("Central Coast"="1",
                          "Colorado River"="2",
                          "North Coast" = "3",
                          "Sacramento River"="4",
                          "San Francisco Bay" = "5",
                          "San Joaquin River" = "6",
                          "South Coast" = "7",
                          "South Lahontan" = "8",
                          "Tulare Lake"= "9"))

data$georegion <-as.factor(data$georegion)

# 4. Combine multiple databases --pull variables and match to 'data'

agency <- merge(data, hist, by="ID")

agency <- agency %>% 
  select(agency.name, ID, basin.name, subbasin.name, hydro, georegion,score, priority, prior, area,  pop, popgrow, 
         well, pubwell,gw.ac.ft, percent.gw.sup, percent.sf.sup, irr.ac, ab3030, abtoc, abmonth, 
         sb1938, sbtoc, sbmonth, irwmp, irwmtoc, irwmporgs)

agen <-merge(agency, gmp, by.x=c("agency.name"),
             by.y=c("Agency_Nam"), all=TRUE)

# When not including all=TRUE, there are only 72 agencies in this dataframe compared 
# to agency which has 147 and the gmp database which has 131.

# When run including all, there are 234 agencies in new database.87 new agencies that are either repetitive or new.

# Lots of repetition of agencies due to different ways to name the same agency such as Dept versus Department
# Export agen into csv to manually fill information in excell
# Write CSV in R

write.csv(agen, file = "agen.csv",row.names=FALSE)





