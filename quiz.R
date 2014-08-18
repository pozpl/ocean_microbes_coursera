library("caret")
library("rpart")
library("tree")
library("randomForest")
library("e1071")
library("ggplot2")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

set.seed(415)
seaflow <- read.csv('seaflow_21min.csv');

init_data_size <- nrow(seaflow)

train.idx <- sample(1:nrow(seaflow), as.integer( (4 * init_data_size)/5 ) )

train <- seaflow[train.idx,]
test <- seaflow[- train.idx,]

time_mean <- mean(train$time)
time_mean

qplot(x=pe, y=chl_small,  colour = pop, data=seaflow)

model <- rpart(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small, method="class", data=train)
print(model)
fancyRpartPlot(model)

tpredict <- predict(model, test, type="class")
tsame <- tpredict == test$pop

sum(tsame)/length(tpredict)

model <- randomForest(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small, data=train)
fpredict <- predict(model, test, type="class")
fsame <- fpredict == test$pop
sum(fsame)/length(fpredict)

importance(model)

model <- svm(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small, data=train)
spredict <- predict(model, test, type="class")
ssame <- spredict == test$pop
sum(ssame)/length(spredict)
