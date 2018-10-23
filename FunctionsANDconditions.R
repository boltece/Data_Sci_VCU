#Functions and Conditions
library(dplyr)

sum(1:5)

# Function basics

# Function name <- function(inputs) { 
# output_value <- do_something(inputs)
# return(output_value)
#  }

calc_shrub_vol <- function (length, width, height) {
  volume <- length * width * height
  return(volume)
}

volume <- calc_shrub_vol(0.8, 1.6, 2)

calc_shrub_vol <- function(length=1, width=1,height = 1) {
  volume <- length * width * height
  return(volume)
}


est_shrub_mass <- function(volume){
  mass <- 2.65 * volume^0.9
  return(mass)
}

shrub_mass <- est_shrub_mass(calc_shrub_vol(0.8, 1.6, 2.0))

shrub_mass<- calc_shrub_vol(0.8,1.6,2.0) %>%
  est_shrub_mass()

## Conditional statements

"aang" == "aang"

#Is 3 not equal to 3?
3 != 3

# Is dog in the string of characters?
"dog" %in% c("cat", "dog", "rabbit")


# and (&) versus or (|) in conditional statements
5 > 2 & 6 >=10
5 > 2 | 6 >=10

x <- 1.3
y <- 2.8
x * 2 + 0.2 == y
  #FALSE

round(x * 2 + 0.2, 1) == y
  #TRUE

veg_type <- "tree" 
veg_type <- "shrub"
volume <- 16.08
if (veg_type == "tree") {
  mass <- 2.65 * volume^0.9
} else {
  print("I don't know how to calculate mass for this type of veg")
  mass <- NA
}
print(mass)


if (veg_type == "tree") {
  mass <- 2.65 * volume^0.9
} else if (veg_type =="shrub") {
  mass <- 0.65 * volume^1.2
}else {
  print("I don't know how to calculate mass for this type of veg")
  mass <- NA
}
print(mass)

est_mass <- function(volume, veg_type) {
  if (veg_type == "tree") {
        mass <- 2.65 * volume^0.9
  }else if (veg_type == "shrub") {
        mass <- 0.65 * volume^1.2
  }else {
        print("I can't convert volume to mass for the veg type")
        mass <- NA
  }
  return(mass)
}

est_mass(1.6, "tree")
est_mass(1.6, "shrub")
est_mass(1.6, "grass")


est_mass <- function(volume, veg_type, age) {
  if (veg_type == "tree") {
    if(age < 5) {
      mass <- 2.65 * volume^0.9
    } else {
      mass <- 1.6 *volume^0.9
    }
   
  }else if (veg_type == "shrub" | veg_type == "thicket") {
    mass <- 0.65 * volume^1.2
  }else {
    print("I can't convert volume to mass for the veg type")
    mass <- NA
  }
  return(mass)
}
est_mass(1.6, "tree", 2)
est_mass(1.6, "tree", 5)
est_mass(1.6, "shrub", 40)
est_mass(1.6, "thicket", 20)