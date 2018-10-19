# Reading the data into a variable.
crops_data<-read.csv('crop_production.csv')

# Removing the NA values.
crops_data<-na.omit(crops_data)

# Getting the length of the dataset.
N<-nrow(crops_data)

# Rounding of the values.
N1<-round(0.1*N)
N2 = N-N1

# Getting the indices by forming a sequence of the length of the dataset.
Indices<-as.vector(seq(N1,N2))

#Productions[1:10]
crops_data<-crops_data[order(crops_data$Production),]

# Divides the data to best fit the classifier.
crops_data<-crops_data[N1:N2,]

library(e1071)

# Using the standard library of naive bayes to make classifications.
Naive_Bayes_Model=naiveBayes(Crop ~ State_Name + Season + Area , data=crops_data)
Naive_Bayes_Model

# Collecting the predictions into a variabls.
NB_Predictions=predict(Naive_Bayes_Model,crops_data)

# The naive bayes classfiers accepts tables and hence converting the data into a table.
table(NB_Predictions,crops_data$Crop)

library(caret)
# Dividing the dataset into training and testing samples.
tr_sample <- sample.int(n = nrow(crops_data), size = (0.7*nrow(crops_data)), replace = F)
naive_train_data <- crops_data[tr_sample,]

te_sample <- sample.int(n = nrow(crops_data), size = (0.3*nrow(crops_data)), replace = F)
naive_test_data <- crops_data[test_sample,]

library(Metrics)

# The next sequence of steps classifies the state name using the naive bayes calssifier and appends to the dataset.
Naive_Bayes_Model_Train=naiveBayes(Crop~Season + Area + Production, data=naive_train_data)
NB_Predictions=predict(Naive_Bayes_Model_Train,naive_test_data)
naive_test_data$Naive_Bayes_prediction <- NULL
naive_test_data$Naive_Bayes_prediction <- NB_Predictions

# Getting the accuracy of classifications.
accuracy(naive_test_data$Crop, naive_test_data$Naive_Bayes_prediction)

