#Reading in the dataset (trimmed and cleaned one)
ProdTrim<-read.csv("TrimmedAgro.csv")

#Selecting required columns for the computation
CropProduction<-ProdTrim[c('Crop','Production')]

#Adding up production value for each crop type
Result1<-aggregate(Production ~ Crop,CropProduction,FUN = sum)

#Arranging in descending order according to production of each crop
Result1<-Result1[order(Result1$Production,decreasing = TRUE),]

#Listing top 10 most produced crops
print (Result1[1:10,]$Production)

#Result1 - sort in descending order by production, then plot pie chart
piepercent<- round(100*Result1[1:10,]$Production/sum(Result1[1:10,]$Production), 1)
pie(Result1[1:10,]$Production,labels = Result1[1:10,]$Crop, main = "Top 10 crops based on production", col = rainbow(10), radius = 1)
legend(-1.5,-0.5, legend = Result1[1:10,]$Crop, cex = 0.8,fill = rainbow(10))

#Selecting crops and area column from the dataset
CropArea<-ProdTrim[c('Crop','Area')]

#Aggregating the area for each crop
Result2<-aggregate(Area ~ Crop,CropArea,FUN = sum)
#Ordering the crop according to area in descending  order
Result2<-Result2[order(Result2$Area,decreasing = TRUE),]

#Listing top 10 crops according to land area used
print (Result2[1:10,]$Area)
pie(Result2[1:10,]$Area,labels = Result2[1:10,]$Crop, main = "Top 10 crops based on Area", col = rainbow(10), radius = 1)

#Code to print the best crop for each season
CropSeason<-ProdTrim[c('Crop','Season')]
CropSeason$Planted<-1
Result3<-aggregate(Planted ~ Crop+Season,CropSeason,FUN = sum)
seasons<-unique(Result3$Season)
cat("Best crop to plant in each season")
for(season in seasons){
  Temp<-Result3[Result3$Season==season,]
  Temp<-Temp[order(Temp$Planted,decreasing = TRUE),]
  cat(season,":",as.character(Temp[1,1]),"\n")
}

#Code to print the best crop for each state 
CropState<-ProdTrim[c('Crop','State_Name')]
CropState$Planted<-1
Result4<-aggregate(Planted ~ Crop+State_Name,CropState,FUN = sum)
states<-unique(Result4$State_Name)
cat("Best crop produced in each state")
for(state in states){
  Temp<-Result4[Result4$State_Name==state,]
  Temp<-Temp[order(Temp$Planted,decreasing = TRUE),]
  cat(state,":",as.character(Temp[1,1]),"\n")
}

