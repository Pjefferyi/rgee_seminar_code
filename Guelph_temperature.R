# This is an example of an analyis peformed using rgee and Google Earth Engine

# load libraries
lapply(X = c("terra", "tidyverse", "sf", "anytime", "rgee", "viridis"), FUN = library, 
       character.only = TRUE)

# set working directory
setwd("C:/Users/Jelan/OneDrive/Desktop/University/University of Guelph/Other/Remote_sensing_seminar/rgee_examples/rgee_seminar_code")

# Set python environment
earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
Sys.setenv(RETICULATE_PYTHON = earthengine_python)

# Initialize rgee and check credentials, this requires a bit more work because ee$Initialize() malfunctions
ee$Authenticate(auth_mode='notebook')
ee$Initialize(project='jelany-project-data')

# Check that everything is working properly
ee_check()

# Let's load our spatial datasets

# First the dataset with temperature data  
# it's full name is MOD11A1.061 Terra Land Surface Temperature and Emissivity Daily Global 1km
# We extract it as an image collection 
temp.gee <- ee$ImageCollection("MODIS/061/MOD11A1")

# Second we're going to need a polygon in the shape of canada that we will use as a mask 
# We'll use a a dataset on the Google earth Engine Catalog called ""
# mask <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017')$
#   filter(ee$Filter$eq('country_na', 'Canada'))

sf_use_s2(FALSE)
mask <- st_read("Property/Property.shp") 
g.crs <-crs(mask) 
mask <- mask %>%
  st_as_sf() %>%
  st_union() %>%
  sf_as_ee()

# we can make out map 
Map$addLayer(mask)

# Let's apply the mask to our spatial dataset. We'll discard all the data in there except the one in Canada
temp.gee <- temp.gee$map(function(img){
  img <- img$clip(mask)
})

# What were the mean temperatures across canada during 2023?

# We use filter our MODIS feature collection and extract the mean temperature durign 2023
temp23 <- temp.gee$filter(ee$Filter$date('2023-01-01', '2023-12-31'))$select('LST_Day_1km')$mean()

temp23scaled <- temp23$expression("temp.k * 0.02- 273.15", list("temp.k" = temp23$select('LST_Day_1km')))
                                                                                                              

# Let's visualize this 
# we create a palette first 
temp23.palette <- list(
  min = 13000,
  max = 16500,
  palette = turbo(100)
)

# we can make out map 
Map$setCenter(-80.24, 43.54, 10)
Map$addLayer(temp23, temp23.palette)

# # We can convert the image we created to a file, we'll export it to Google Drive
# task <- ee_image_to_drive(
#   image = temp23,
#   scale = 1000, # resolution of 1 pixel per 10 kilometers 
#   folder= "GEE_imports",
#   fileFormat = "GEO_TIFF",
#   region = mask)
# 
# task$start()
# ee_monitoring()

# Now we can read this file with the terra package 
x <- terra::rast("rgee_guelph_temp.tif")
plot(x)

#Let's rescale the temperature to see it in celcius 
x <- x*0.02- 273.15
plot(x)

# Now we can do a more advanced analysis

years <- ee$List$sequence(2001 , 2023)
months <- ee$List$sequence(01 , 12)

# monthly.temp <- ee$ImageCollection$fromImages(years$map(ee_utils_pyfunc(function(year){
#   return(months$map(ee_utils_pyfunc(function(month){
#     return(temp.gee$filter(ee$Filter$calendarRange(year, year, "year"))$
#              filter(ee$Filter$calendarRange(month, month, "month"))$
#              select('LST_Day_1km')$mean()$
#              set("year", year)$
#              set("month", month))
#   })))
# }))$flatten())


monthly.temp <- ee$FeatureCollection(years$map(ee_utils_pyfunc(function(year){
  return(months$map(ee_utils_pyfunc(function(month){
    
    monthly.img <- temp.gee$filter(ee$Filter$calendarRange(year, year, "year"))$
             filter(ee$Filter$calendarRange(month, month, "month"))$
             select('LST_Day_1km')$mean()
    
    monthly.temp <- monthly.img$clip(mask)$reduceRegion(reducer = ee$Reducer$mean())
    
    return(ee$Feature(NULL, list("month_temp" = monthly.temp$get("LST_Day_1km"), 
                                 'year' = year, 
                                 "month" = month)))
    
  })))
}))$flatten())


# let's extract daily mean temperatures between 2000 and 2023 
# temp.daily <- temp.gee$filterDate("2000-01-01" , "2023-12-31") %>%
#  ee$ImageCollection$filter(ee$Filter$calendarRange(1, 365, 'DAY_OF_YEAR'))

# mean.temp.daily <- temp.daily$map(function(img){
#  
#    mean.temp <- img$select('LST_Day_1km')$reduceRegion(reducer = ee$Reducer$mean())
#    datetime <- img$metadata('system:index')
#    #feat <- ee$Feature(NULL, list("Mean.temp" = mean.temp, "DateTime" = date))
#    img <- img$set(list("Mean_temp" = mean.temp, "date" = datetime))
# }) 


#export the output to Google Drive
# By default, only properties that are non-NUll in the first image are exported. They might not include the output.
task <- ee_table_to_drive(
  collection = monthly.temp,
  description = "monthly_temp_mean_canada",
  folder = "GEE_imports",
  fileFormat = "CSV"
)
task$start()
ee_monitoring()

x$month_temp <- x$month_temp*0.02- 273.15

ggplot(data = x, aes(y = month_temp, x = system.index)) + 
  geom_line()+
  geom_smooth(method='lm', formula= y~x)

mod <- lm(month_temp ~ system.index, data = x)
summary(mod)
