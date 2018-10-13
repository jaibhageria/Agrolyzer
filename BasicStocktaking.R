setwd('C:/Users/Guruprasad/Desktop/Sem-5/Data Analytics/Project')
Agro<-read.csv('crop_production.csv')
Agro2<-na.omit(Agro)
Productions<-Agro$Production
class(Productions)
Productions[1]
Productions<-na.omit(Productions)
summary (Productions)
quantile (Productions)
quantile (Productions,c(0.75))
#10% trimmed mean
mean (Productions,trim = 0.1)  #Seems appropriate as it is not too far away from median
#10% Trimmed mean = 5046, Median = 729
N<-length(Productions)
N1<-round(0.1*N)
N2 = N-N1
Indices<-as.vector(seq(N1,N2))
Productions[1:10]
Agro2<-Agro2[order(Agro2$Production),]
ProdTrim<-Agro2[N1:N2,]
summary(ProdTrim)
CropProduction<-ProdTrim[c('Crop','Production')]
Result1<-aggregate(Production ~ Crop,CropProduction,FUN = sum)
Result1<-Result1[order(Result1$Production,decreasing = TRUE),]
#Listing top 10 most produced crops
print (Result1[1:10,])
#Result1 - sort in descending order by production, then plot pie chart
pie(Result1[1:20]$Production,labels = Result1$Crop, radius = 5)

#Jai - what you need to do:
#Result1 has 105 crops and their total production
#Have a suitable visualisation for top 30 or all the crops, whichever in terms of production
#Any doubts, call me until 23:59 hrs tonight
#Write that particular code in a separate R file so that we can push separately
