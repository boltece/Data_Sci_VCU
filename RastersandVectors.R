
#### Exercise 1 ####
## Exploring spatial data - Data Carpentry Course

# install packages if necessary
# install.packages ("raster"); install.packages("rgdal")

# load libraries
library(raster)
library(rgdal)
library(ggplot2)

## 1

# Import DSM (Digital Surface Model) for the Harvard site
dsm_harv <- raster("data/NEON-airborne/HARV_dsmCrop.tif")
dsm_harv_df <- as.data.frame(dsm_harv, xy= TRUE)


#Import DTM for the Harvard Forest
dtm_harv <- raster("data/NEON-airborne/HARV_dtmCrop.tif")
dtm_harv_df <- as.data.frame(dtm_harv, xy=TRUE)

# Create a Canopy height model for the Harvard Forest
chm_harv <- dsm_harv - dtm_harv


# Import DSM (Digital Surface Model) for the San Joaquin Exp. Range
dsm_sjer <- raster("data/NEON-airborne/SJER_dsmCrop.tif")
dsm_sjer_df <- as.data.frame(dsm_sjer, xy= TRUE)


#Import DTM for the Harvard site
dtm_sjer <- raster("data/NEON-airborne/SJER_dtmCrop.tif")
dtm_sjer_df <- as.data.frame(dtm_sjer, xy=TRUE)

# Create a Canopy height model for the Harvard site
chm_sjer <- dsm_sjer - dtm_sjer

## 2

#Convert canopy height models into data frames
chm_harv_df <- as.data.frame(chm_harv, xy=TRUE)
chm_sjer_df <- as.data.frame(chm_sjer, xy=TRUE)

head(chm_harv_df)

#histogram of raster values for Harvard Forest- Canopy Height Model
ggplot() +
  geom_histogram(data= chm_harv_df, aes(x=x))

# plot of Harvard Forest- Canopy Height Model
ggplot() +
  geom_raster(data = chm_harv_df, 
              aes(x = x, y = y, fill = layer)) +
  coord_quickmap()

#histogram of raster values for San Joaquin Exp. Range- Canopy Height Model
ggplot() +
  geom_histogram(data = chm_sjer_df, aes(x=x))

# plot of San Joaquin Exp. Range- Canopy Height Model
ggplot() +
  geom_raster(data = chm_sjer_df, 
              aes(x = x, y = y, fill = layer)) +
  coord_quickmap()

## 3

## Add plot location information to Harvard Forest- CHM plot
plots_harv <- readOGR("data/NEON-airborne/plot_locations", 
                      "HARV_plots")

# Align coordinate reference system of plots_harv to the canopy height model's reference system
plots_harv_utm <- spTransform(plots_harv, crs(chm_harv))
plots_harv_utm_df = as.data.frame(plots_harv_utm)

#plot it!
ggplot() +
  geom_raster(data = chm_harv_df, 
              aes(x = x, y = y, fill = layer)) +
  geom_point(data = plots_harv_utm_df, 
             aes(x = coords.x1, y = coords.x2), color = "yellow")


## Add plot location information to San Joaquin Exp. Range- CHM plot
plots_sjer <- readOGR("data/NEON-airborne/plot_locations", 
                      "SJER_plots")

# Align coordinate reference system of plots_harv to the canopy height model's reference system
plots_sjer_utm <- spTransform(plots_sjer, crs(chm_sjer))
plots_sjer_utm_df = as.data.frame(plots_sjer_utm)

#plot it!
ggplot() +
  geom_raster(data = chm_sjer_df, 
              aes(x = x, y = y, fill = layer)) +
  geom_point(data = plots_sjer_utm_df, 
             aes(x = coords.x1, y = coords.x2), color = "yellow")

## 4

#plots_chm_harv <- extract(chm_harv, plots_harv_utm)
#plots_harv_utm$plot_id
#plots_chm_harv <- data.frame(plot_num = plots_harv_utm$plot_id, plot_value = plots_chm_harv)

plots_chm_harv <- extract(chm_harv, plots_harv_utm, buffer = 10, fun = max)
View(plots_chm_harv)

plots_chm_sjer <- extract(chm_sjer, plots_sjer_utm, buffer = 10, fun = max)
View(plots_chm_sjer)

harv_sjer_plots_CHM_df <- data.frame(CHM_HARV = plots_chm_harv, CHM_SJER = plots_chm_sjer)
View(harv_sjer_plots_CHM_df)


#### Exercise 2 ####

# install packages if necessary
# install.packages ("raster"); install.packages("rgdal")

# load libraries
library(raster)
library(rgdal)
library(ggplot2)

##1

#stack HARV rasters
harv_files <- list.files("data/HARV_NDVI/",
                        full.names = TRUE,
                        pattern = "HARV_NDVI.*.tif")
harv_rasters <- stack(harv_files)
#count number of layers
nlayers(harv_rasters)

#stack SJER rasters
sjer_files <-  list.files("data/SJER_NDVI/",
                          full.names = TRUE,
                          pattern = "SJER_NDVI.*.tif")
sjer_rasters <- stack(sjer_files)

# calculate the mean 

harv_avg <- cellStats(harv_rasters, mean)

sjer_avg <- cellStats(sjer_rasters, mean)

# Create a vector of sampling periods
samp_period <- c(1:length(harv_avg), 1:length(sjer_avg))
View(samp_period)

# Create a vector of site names
site_name <- c(rep("harv", length(harv_avg)), rep("sjer", length(sjer_avg)))
View(site_name)

# Make a data frame that includes columns for site name, sampling period, and the average NDVI values
complete_df <- data.frame(Site_name= site_name,Sampling_period= samp_period, NDVI_avg = c(harv_avg,sjer_avg))
View(complete_df)

# graph the trends through time
ggplot() +
  geom_point(data = complete_df, 
              aes(x = Sampling_period, y = NDVI_avg, color = Site_name)) 


## 2

##HARV
## Add plot location information to Harvard Forest- CHM plot
plots_harv <- readOGR("data/NEON-airborne/plot_locations", 
                      "HARV_plots")

# Align coordinate reference system of plots_harv to the canopy height model's reference system
plots_harv_utm <- spTransform(plots_harv, crs(chm_harv))


#stack HARV rasters
harv_files <- list.files("data/HARV_NDVI/",
                         full.names = TRUE,
                         pattern = "HARV_NDVI.*.tif")
harv_rasters <- stack(harv_files)

#Extract all values from the HARV stack by plot
harv_extracted_byPLOT <- extract(harv_rasters, plots_harv_utm)
View(harv_extracted_byPLOT)

#transpose the matrix
transposed_harv<- t(harv_extracted_byPLOT)
View(transposed_harv)

#turn into a data frame
harv_extracted_byPLOT_df <- as.data.frame(transposed_harv)
#turn the rownames into a column
harv_extracted_df <- tibble::rownames_to_column(harv_extracted_byPLOT_df, var = "date")

View(harv_extracted_df)

## SJER
## Add plot location information to San Joaquin Exp. Range- CHM plot
plots_sjer <- readOGR("data/NEON-airborne/plot_locations", 
                      "SJER_plots")

# Align coordinate reference system of plots_harv to the canopy height model's reference system
plots_sjer_utm <- spTransform(plots_sjer, crs(chm_sjer))

# stack SJER rasters
sjer_files <-  list.files("data/SJER_NDVI/",
                          full.names = TRUE,
                          pattern = "SJER_NDVI.*.tif")
sjer_rasters <- stack(sjer_files)

#Extract all values from the SJER stack by plot
sjer_extracted_byPLOT <- extract(sjer_rasters, plots_sjer_utm)
View(sjer_extracted_byPLOT)
#transpose the matrix
transposed_sjer<- t(sjer_extracted_byPLOT)
View(transposed_sjer)

#turn into a data frame
sjer_extracted_byPLOT_df <- as.data.frame(transposed_sjer)
#turn the rownames into a column
sjer_extracted_df <- tibble::rownames_to_column(sjer_extracted_byPLOT_df, var = "date")

View(sjer_extracted_df)


#### Exercise 3 ####

##1
# Get GBIF data for Dipodomys spectabilis
install.packages("spocc")
library(spocc)

dipo_df = occ(query = "Dipodomys spectabilis", 
              from = "gbif",
              limit = 1000,
              has_coords = TRUE)
dipo_df = data.frame(dipo_df$gbif$data)
View(dipo_df)
##2
library(dplyr)

# Rename the second and third column to simple longitude and latitude
dipo_df_v2 <- rename(dipo_df, longitude =Dipodomys_spectabilis.longitude, latitude = Dipodomys_spectabilis.latitude  )
View(dipo_df_v2)

# Filter data for only Preserved Specimen of the US
dipo_df_filtered <- dipo_df_v2 %>%
   filter(Dipodomys_spectabilis.basisOfRecord == "PRESERVED_SPECIMEN") %>%
   filter(Dipodomys_spectabilis.countryCode == "US")
View(dipo_df_filtered)

#Remove points with values of 0 for latitude or longitude
dipo_df_filtered_v2 <- dipo_df_filtered %>% 
    filter(latitude > "0") %>% 
    filter(longitude < "0")
#select only the lat and long columns
dipo_df_lat_long <- select(dipo_df_filtered_v2, latitude, longitude)
write.csv(dipo_df_lat_long, "data/Species_Occurrences_Map_kangaroos in the US.csv")
#View top few rows of the cleaned dataset. 
head(dipo_df_lat_long)


##3
#Get data for a US map
usmap = map_data("usa")

usmap <- as.data.frame(usmap)

#plot us map with kangaroo coordinates
#Not sure why fill= "white" is shoing up as salmon on my laptop?????
ggplot() +
  geom_polygon(data= usmap, aes(x= long, y= lat, group = group, fill= "", color= "black")) +
  geom_point(data=dipo_df_lat_long, aes(x=longitude, y=latitude) ) +
  coord_quickmap()


#### Exercise 4 ####

## 1
library(raster)

#Get elevation data for the US
elevation = getData("alt", country = "US")
#Select the first column
elevation = elevation[[1]]
summary(elevation)
#Make a data frame of this elevation data
elevation_df <- as.data.frame(elevation, xy=TRUE)

#Call in Species Occurrence records then turn into a data frame
dipo_df_lat_long <- read.csv("data/Species_Occurrences_Map_kangaroos in the US.csv")
dipo_df_lat_long_df <- as.data.frame(dipo_df_lat_long)

#Plot elevation raster with species occurences as points
ggplot() +
  geom_raster(data= elevation_df, aes(x = x, y = y, fill= USA1_msk_alt)) +
  geom_point(data= dipo_df_lat_long_df, aes(x=longitude, y=latitude)) +
  coord_quickmap()

##2
crs((elevation))
#CRS arguments: +proj=longlat +ellps=WGS84
#Create crs that matches the elevation data set
points_crs <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#Turn dipo_df_lat_long into a Spatial points data frame
points_dipo <- SpatialPointsDataFrame(
  dipo_df_lat_long[c('longitude', 'latitude')], 
  dipo_df_lat_long, 
  proj4string = points_crs)

#extract elevation for lat long of species occurrences
dipo_elevation <- extract(elevation, points_dipo)
#turn those elevation values for lat long into a data frame
dipo_elevations_df <- as.data.frame(dipo_elevation)

#Plot a histogram of species occurrence elevations
ggplot() +
  geom_histogram(data= dipo_elevations_df, aes(x=dipo_elevation))

##3
#compare histograms/distributions of all elevation to elevation of species occurrences
ggplot() +
  geom_histogram(data = elevation_df, aes(x = USA1_msk_alt, y = ..density.., fill='gray'), alpha = 0.3) +
  geom_histogram(data= dipo_elevations_df, aes(x= dipo_elevation, y= ..density.., fill='red'),  alpha=0.3)
                 
                 
                 
