library(tidyverse)
library(sf)
library(leaflet)


library(raster)
library(data.table)
library(rjson)
library(jsonlite)
library(hablar)
library(rgdal)
library(rgeos)
library(dplyr)

# IMPORTANT:
#   cost_FY18_Q..: Calculated from Daily Tasks, the average sum of hours spent servicing a park 
#                  multiplied by the base hourly pay for each job title recorded as providing service
#   wo_total_FY18_Q..:The sum of hours spent maintaining a park by specialized staff multiplied by the 
#                     base hourly or overtime rate, and the costs of parts and materials used for this 
#                     maintenance.

#setting new column names
colnams = c('number','borough_code', 'sign_name', 'category', 'site_amenities', 'sector_name', 
'sector_desc', 'functional_acreage', 'total_acreage', 'Hrs_FY18_Q1', 'Hrs_FY18_Q2', 'Hrs_FY18_Q3',
'Hrs_FY18_Q4', 'cost_FY18_Q1', 'cost_FY18_Q2', 'cost_FY18_Q3', 'cost_FY18_Q4', 'visit_FY18_Q1', 
'visit_FY18_Q2', 'visit_FY18_Q3', 'visit_FY18_Q4', 'fixed_FY18_Q1','fixed_FY18_Q2', 
'fixed_FY18_Q3', 'fixed_FY18_Q4', 'wo_total_FY18_Q1', 'wo_total_FY18_Q2', 'wo_total_FY18_Q3', 
'wo_total_FY18_Q4')

#load dataframe, delete extraneous columns
maint <- read.csv('./Parks/park_maintenance18.csv', sep = ',' )
maint <- maint[,colSums(is.na(maint))<nrow(maint)]

#set old column names
oldcol <- colnames(maint)


#replace old column names with new column names
maint <- maint %>% setnames(old = oldcol, new = colnams)

#prepare number column for data join with sf file
maint$number <- sub(" .+", "", maint$number)

#remove na's from monetary columns
maint$wo_total_FY18_Q1[is.na(maint$wo_total_FY18_Q1)] <- 0
maint$wo_total_FY18_Q2[is.na(maint$wo_total_FY18_Q2)] <- 0
maint$wo_total_FY18_Q3[is.na(maint$wo_total_FY18_Q3)] <- 0
maint$wo_total_FY18_Q4[is.na(maint$wo_total_FY18_Q4)] <- 0

maint$cost_FY18_Q1[is.na(maint$cost_FY18_Q1)] <- 0
maint$cost_FY18_Q2[is.na(maint$cost_FY18_Q2)] <- 0
maint$cost_FY18_Q3[is.na(maint$cost_FY18_Q3)] <- 0
maint$cost_FY18_Q4[is.na(maint$cost_FY18_Q4)] <- 0

# add average weekly quarterly costs X 13 weeks to approximate total  yearly costs for daily tasks
maint$daily_cost_year <- maint$cost_FY18_Q1*13 + maint$cost_FY18_Q2*13 +
  maint$cost_FY18_Q3*13 + maint$cost_FY18_Q4*13

# calculate year total of work order tasks
maint$work_order_total <- maint$wo_total_FY18_Q1 +
  maint$wo_total_FY18_Q2 + maint$wo_total_FY18_Q3 + maint$wo_total_FY18_Q4

#add average weekly quarterly costs X 13 weeks to approximate total  yearly costs
maint$wo_total_parks_maint_cost <-  maint$daily_cost_year + maint$work_order_total

#calculate annual maintenance cost per acre
maint$cost_per_acre <- round(maint$wo_total_parks_maint_cost/as.numeric(maint$functional_acreage),2)

#read in sf file
#remove extra columns
#rename number column for join with dataframe
park_shapes_maint <- st_read("G:/DATABASE/City_Council/Public_Advocate/Parks/parks_data/Parks_Properties/geo_export_11cd0a61-e45b-447b-99f0-d4ccbde643ce.shp")
st_transform(park_shapes_maint,'+proj=longlat +datum=WGS84')
st_crs(park_shapes_maint) <-4326
keeps <- c("acres", 'gispropnum',"borough",'class','communityb','jurisdicti','signname', 'subcategor',
           'typecatego','waterfront')
park_shapes_maint<-park_shapes_maint[keeps]
park_shapes_maint <- park_shapes_maint %>% setnames(old = 'gispropnum', new = 'number')

#join files
park_maint <- left_join(maint, park_shapes_maint)

#convert df to sf
park_maint <- st_as_sf(park_maint)
st_transform(park_maint, 4326)

#set a palette
pal <- colorNumeric(
  palette = "Reds",
  domain = log(park_maint$cost_per_acre+ .000001))

str(park_maint$cost_per_acre)

#name, cost column
park_maint$label <- paste(park_maint$sign_name, park_maint$cost_per_acre, sep = ' ', collaple = '')

#set up leaflet map
maint_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = park_maint, weight =1,
              color = ~pal(log(cost_per_acre + .000001)),
              stroke = FALSE,
              smoothFactor = .2,
              fillOpacity = 1,
              label = ~label,
              labelOptions = labelOptions(noHide = F,
                                          direction = 'auto'))
maint_map

hist(park_maint$cost_per_acre, breaks = 100)

summary(park_maint$cost_per_acre)
