getwd()
setwd("/home/datta/Documents/5th Sem/DataAnalytics/Project")
library(survival)
# The original dataset with all the values including the NA values.
original_crops <- read.csv('crop_production.csv')

# The trimmed dataset with no NA values.
crops <- read.csv('TrimmedAgro.csv')

# Predictive modelling technique 
# Linear regression to determine the change in a continuous valrible y with the given value of y.
# y = production, x = area.

# Reassigning the data frame in ascending irder of the years to get the change over the years.
crops <- crops[with(crops, order(crops$Crop_Year)),]

# Total productions of all the crops in the corresponing year over all states and all crops.

crop_production <- aggregate(crops$Production ~ crops$District_Name + crops$Crop_Year + crops$Crop + crops$Season + crops$Area, crops, sum)
names(crop_production) <- c("District","Year","Crop","Season","Area","Production")
crop_production <- crop_production[with(crop_production, order(crop_production$Year)),]


library(e1071)  # for skewness function
par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(crop_production$Area), main="Density Plot: Area", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(crop_production$Area), 2)))  # density plot for 'area'

polygon(density(crop_production$Area), col="red")

plot(density(crop_production$Production), main="Density Plot: Production", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(crop_production$Production), 2)))  # density plot for 'production'

polygon(density(crop_production$Production), col="red")
# As we can see from the density plot that the data is right skewed for both the cases.

# The correlation also suggests that it is a positive correlation between the two
cor(crop_production$Area, crop_production$Production)

boxplot(crop_production$Area, crop_production$Production)

# Scatter plot shows that data is compact on one end.
plot(crop_production$Area, crop_production$Production)
# Now when we look at the sctter plot with the best fit line we would see that since the data is very skewed it would bb very cluttered at one end.
scatter.smooth(crop_production$Area, crop_production$Production, main = "Area ~ Production")


# In order to get a model and make more acceptable predictions we look to sample the data and perform transformation and regression on it.
area_transform <- log10(crop_production$Area)
production_transform <- log10(crop_production$Production)

area_transform_sample <- sample(area_transform, 600)
production_transform_sample <- sample(production_transform,600)

# qqplot to confirm that the data has been normalized.
qqplot(area_transform_sample, production_transform_sample)

scatter.smooth(area_transform_sample, production_transform_sample)

boxplot(area_transform_sample, production_transform_sample)

# Linear Regression.

# Now we have seen that the data is not normal we had to performa log transform on the data in order to normalize the data.

# We get to buliding the model of the transformed values 
# Appending the columns of the transformed values to the datset.
crop_production$Area_transform=area_transform
crop_production$Production_transform=production_transform

# ==============================================================================#
# Buliding a model:
# Dividing the dataset into training and testing datsets.

train_sample <- sample.int(n = nrow(crop_production), size = (0.7*nrow(crop_production)), replace = F)
train_data <- crop_production[train_sample,]

test_sample <- sample.int(n = nrow(crop_production), size = (0.3*nrow(crop_production)), replace = F)
test_data <- crop_production[test_sample,]

linearMod <- lm(formula =train_data$Production_transform ~ train_data$Area_transform , data = train_data)

print(linearMod)
# The linearMod variable has the value of the coefficients beta1 and beta2

# Sampling the above dataset using simple random sample.

area_transform_sample <- sample(train_data$Area_transform, 800)
production_transform_sample <- sample(train_data$Production_transform, 800)
plot(area_transform_sample, production_transform_sample, pch = 16, cex = 1.3, col = 'blue', main = "Production Plotted against area", xlab = "Area", ylab = "Production")
abline(0.5520,0.8334)

predicted_values<-((10^(test_data$Area_transform*0.8334 + 0.5520)))

test_data$Predicted_Values <- predicted_values
colnames(test_data)[9] <- "Predicted Production Values"

summary(linearMod)
# Summarizing all the coefficients and the errors in calculation if the model has been presented.

# Now we will analyze the value of p to understand the statistical significance.
# This is to check that the linear model that we have constructed is statistically significant.
# The one way to check is to consider the pre-determined value fo the statistical significance.
# Depending on the NULL and alternate hypothesis we can decide what the p-value would indicate.
# In this very case we will reject the null hypothesis since the value of p is lesser than 0.05.

# RMSE in the dataset
library(Metrics)
rmse(test_data$Production, test_data$`Predicted Production Values`)

# Concluding that the linear model does not give accurate predictions.   