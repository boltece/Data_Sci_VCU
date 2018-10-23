
#### Exercise 1 ####

## Write a function that converts pounds to grams 
## (there are 453.592 grams in one pound). 

calc_grams <- function (pounds) {
  grams <- pounds * 453.592
  return(grams)
}

mass <- calc_grams(3.75)
print(mass)


#### Exercise 2 ####

## 1
## To find the mass of theropoda, all you need is the input "length" in METERS
## Length is then exposed to exponent 3.63 and the result is then multiplied by 0.73.
## to obtain mass in kg

get_mass_from_length_theropoda <- function(length){
  mass <- 0.73 * length ** 3.63
  return(mass)
}

## 2
Spinosaurus_mass <- get_mass_from_length_theropoda(16)
print(Spinosaurus_mass)

## 3
get_mass_from_length <- function(a, b, length){
  mass <- a * length ** b
  return(mass)
}

## estimate the mass of a Sauropoda (a = 214.44, b = 1.46) that is 26 m long.
Sauropoda_mass <- get_mass_from_length(a= 214.44, b= 1.46, length = 26)
print(Sauropoda_mass)


#### Exercise 3 ####

## function that converts kilograms into pounds (there are 2.205 pounds in a kilogram)
## In Stegosauria, a = 10.95 and b = 2.64

calc_pounds <- function(kg) {
  pounds <- kg * 2.205
  return(pounds)
}

get_mass_from_length <- function(a, b, length){
  mass <- a * length ** b
  return(mass)
}

Stegosaurus_kg <- get_mass_from_length(a= 10.95, b=2.64, length=12)
print(Stegosaurus_kg)
lbs <-calc_pounds(Stegosaurus_kg)
print(lbs)



#### Exercise 4 ####

w <- 10.2
x <- 1.3
y <- 2.8
z <- 17.5
dna1 <- "attattaggaccaca"
dna2 <- "attattaggaacaca"
colors <- c("green", "pink", "red")

#1
w > 10
#2
"green" %in% colors
#3
x > y
#4
2 * x + 0.2 == y
#5
dna1 == dna2
#6
dna1 != dna2
#7
w > x & y > z
#8
x * w >13.2 & x * w < 13.5  
#9
dna1 > nchar(5) | z < w * x


#### Exercise 5 ####

if ("thesis_data.csv" %in% list.files("~/Desktop/DataScience_R")) {
  data <- read.csv()
} else {
  print("OMG MY THESIS DATA IS MISSING. NOOOO!!!!")
  data <- NA
}



#### Exercise 6 ####

get_mass_from_length_by_name <- function (length, name) {
  if (name == "Stegosauria") {
    mass <- 10.95 * length ** 2.64
  } else if (name =="Theropoda") {
    mass <- 0.73 * length ** 3.63
  }else { name= "Sauropoda"
    mass <- 214.44 * length ** 1.46
  }
  return(mass)
}

#1
#A Stegosauria that is 10 meters long.
get_mass_from_length_by_name(length=10, name="Stegosauria")

#2
#A Theropoda that is 8 meters long.
get_mass_from_length_by_name(length = 8, name="Theropoda")

#3
#A Sauropoda that is 12 meters long.
get_mass_from_length_by_name(length=12, name="Sauropoda")



#### Exercise 7 ####

dna_or_rna <- function(sequence) {
  if (grepl("u", sequence, ignore.case = TRUE)) {
    print("RNA")
  } else if (grepl("t", sequence, ignore.case = TRUE)) {
    print("DNA")
  } else {
    print("UNKNOWN")
  }
}

seq1 <- "ttgaatgccttacaactgatcattacacaggcggcatgaagcaaaaatatactgtgaaccaatgcaggcg"
seq2 <- "gauuauuccccacaaagggagugggauuaggagcugcaucauuuacaagagcagaauguuucaaaugcau"
seq3 <- "gaaagcaagaaaaggcaggcgaggaagggaagaagggggggaaacc"

dna_or_rna(seq1)
dna_or_rna(seq2)
dna_or_rna(seq3)


#### Exercise 8 ####

library(raster)
library(dplyr)
library(spocc)
library(ggplot2)

## Occurrence data and plots for Quercus alba, Picea glauca, and Ceiba pentandra.

get_occurr_and_climate <- function(name) {
  worldclimate <- getData('worldclim', var = 'bio', res = 10)
  climate <- dropLayer(worldclimate, c(2:11,13:19))
  climate_df <- na.omit(as.data.frame(climate*0.1, xy=TRUE))
  sp_points <- occ(query = "Quercus alba", 
              from = "gbif",
              limit = 1000,
              has_coords = TRUE)
  sp_points<- data.frame(sp_points$gbif$data)
  sp_points_lat_long <- select(sp_points, longitude=2, latitude=3)
  points_crs <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  sp_points_df <- SpatialPointsDataFrame(
    sp_points_lat_long[c(1:2)], 
    sp_points_lat_long, 
    proj4string = points_crs)
  ext_clim <- extract(climate, sp_points_df)
  ext_clim_df <- na.omit(as.data.frame(ext_clim*0.1, xy=TRUE))
  return(ext_clim_df)
}



Q_alba <- get_occurr_and_climate(name= "Quercus alba")
P_glauca <- get_occurr_and_climate(name="Picea glauca") 
C_pentandra <- get_occurr_and_climate(name="Ceiba pentandra")


#2
plot_sp <- function(name, color) {
  worldclimate <- getData('worldclim', var = 'bio', res = 10)
  climate <- dropLayer(worldclimate, c(2:11,13:19))
  climate_df <- na.omit(as.data.frame(climate*0.1, xy=TRUE))
  plant_plot <- ggplot() + 
    geom_point(data=sample_n(climate_df, 10,000), aes(x = bio1, y = bio12), alpha = 0.1) + 
    geom_point(data=(get_occurr_and_climate(name)), aes(x=bio1, y= bio12), color=color) + 
    labs(x = expression("Temperature" (degree~C)), y = "Precipitation (cm)")
  return(plant_plot)
}


plot_sp(name= "Quercus alba", color="red")
plot_sp(name= "Picea glauca", color="green")
plot_sp(name="Ceiba pentandra", color="blue")
