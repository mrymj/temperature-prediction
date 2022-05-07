
# importing dataset 

dataset <- read.csv(file.choose(),header = TRUE)
View(dataset)

#correlation coefficients

cor(dataset$MinTemp, dataset$MaxTemp)

#Line Graph

x <- dataset[100:300,"MaxTemp"]
plot(x, type = 'p', col="red", xlab = "MinTemp", ylabel = "MaxTemp", main = "MinTemp VS MaxTemp")

#divide the data into the training set and the test set

library(caTools)
split = sample.split(dataset$MaxTemp,SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#creating the linear regression model

lin.reg = lm(MaxTemp ~ MinTemp, data = training_set)
abline(lin.reg, col = "blue")
plot(lin.reg)

#predict test results

temp_pred = predict(lin.reg, newdata = test_set)

#visualize the results of the training set

library(ggplot2)
ggplot() +
geom_point(aes(x = training_set$MinTemp,
               y = training_set$MaxTemp), colour = 'red') +
geom_line(aes(x = training_set$MinTemp,
              y = predict(lin.reg, newdata = training_set)), colour = 'blue') +
ggtitle('MinTemp VS MaxTemp (training set - RLS') +
xlab('MinTemp') +
ylab('MaxTemp')

#visualize the results of the test set

ggplot() +
  geom_point(aes(x = test_set$MinTemp,
                 y = test_set$MaxTemp), colour = 'red') +
  geom_line(aes(x = training_set$MinTemp,
                y = predict(lin.reg, newdata = training_set)), colour = 'blue') +
  ggtitle('MinTemp VS MaxTemp (test set - RLS') +
  xlab('MinTemp') +
  ylab('MaxTemp')

