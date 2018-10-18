##Imporing the required libraries and datasets
library(ggmap)
library(mapview)
library(sp)
crops_data <- read.csv("TrimmedAgro.csv")

#Selecting only data for the year 2000 from the dataset 
heat_map_data <- crops_data[ which(crops_data$Crop_Year==2000),]

#Selecting the required columns for computations
heat_map_data <- heat_map_data[c("District_Name","Crop")]

#Finding all the district names which were involved in cultivation for the year 2000 
districts <- unique(heat_map_data$District_Name)
district_coords <- data.frame("District_Name" = districts)

#Finding and storing the coordinates for all districts in dataset district_coords
for(i in 1:nrow(district_coords)){
  coords <- as.numeric(geocode(location = as.character(district_coords$District_Name[i]), source = "google"))
  district_coords$reclat[i] <- coords[2]
  district_coords$reclong[i] <- coords[1]
}

#Removing stray coordinates returned by api, keeping them to latitude and longitude extremes only for India
district_coords <- district_coords[ which(district_coords$reclat>6.746 & district_coords$reclat<37.086),]
district_coords <- district_coords[which(district_coords$reclong>68.03 & district_coords$reclong<97.39),]

#Attaching the coordinates for each corresponding district name from the district_coords data
heat_map_data <- merge(heat_map_data,district_coords,by="District_Name")

#Obtaining the coordinates for India from the google api
india_coords = as.numeric(geocode("India"))

#Getting the google map of India with obtained coordinates 
IndiaMap = ggmap(get_googlemap(center=india_coords, scale=2, zoom=4), extent="normal")

#This will plot a map of India with red dots representing the districts of India involved in cultivation in the year 2000
#Saved as file name Farming_Districts_2000 file.
IndiaMap + 
  geom_point(aes(x=reclong, y=reclat), data=district_coords, col="red", alpha=0.8)

#Selecting only the top 10 crops in India according to production
# Top 10 crops based on production: Wheat, rice, maize, onion, banana, jowar, groundnut, gram, potato, sugarcane
top10 <- c("Wheat","Rice","Maize","Onion","Banana","Gram","Potato","Groundnut","Sugarcane","Jowar")
top10_data <- heat_map_data[heat_map_data$Crop %in% top10,]

#Using mapview to get a interactive plot for showing distribution of crops across different districts of India for the year 2000.
#User can select individual crop (by checking a checkbox list of crops) and analyse the distribution.

#Converting to spatial data for mapview function for all crops(in heat_map_data) as well as top10 crops separate (maps)
coordinates(heat_map_data) <- ~ reclong + reclat
proj4string(heat_map_data) <- "+init=epsg:4326"

coordinates(top10_data) <- ~ reclong + reclat
proj4string(top10_data) <- "+init=epsg:4326"

#Creating mapview data and saving it to a file
m <- mapview(heat_map_data, zcol = "Crop", burst = TRUE, homebutton = FALSE, legend = TRUE)
mapshot(m, url = paste0(getwd(), "/India_Map_Crops_2000.html"))
