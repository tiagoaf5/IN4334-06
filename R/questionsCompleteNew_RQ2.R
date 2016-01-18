library(reshape)
library(ggplot2)
library(corrplot)
library(caret)

source('/Users/tiago/Documents/lab/IN4334-06/R/ml.R')


delim = ","  # or is it "\t" ?
dec = "."    # or is it "," ?
questions <- read.csv("/Users/tiago/Documents/lab/IN4334-06/LargeAssignment/src/main/resources/results/questions_1451932330470_filtered-by_java.csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)
data <- questions[complete.cases(questions),]

data <- data[data$number.of.answers > 0,]
x <- data$max.answer.score - min(data$score) + 1
y <- data$score - min(data$score) + 1


data$ratio = x/y

#answer - score
#x <- data$max.answer.score
#y <- data$score
#data$ratio = x - y


#data$number.of.answers <- as.factor(data$number.of.answers)


numeric.fields <- c(#'days.since.posted',
  'title.length',
  #'view.count',
  'tags.count',
  'max.tag.popularity..no.java.',
  'avg.tag.popularity',
  'min.tag.popularity',
  'local.max.tag.popularity..no.java.',
  'local.avg.tag.popularity',
  'local.min.tag.popularity',
  'total.code..',
  'java..',
  'json..',
  'xml..',
  'stack.traces..',
  'length',
  'words.count',
  'text.speak.count',
  'urls.count',
  'Coleman.Liau.Index',
  'Flesch.Reading.Ease.Score',
  'Flesch.Kincaid.Grade.Level',
  'Automated.Readability.Index',
  'Gunning.Fog.Index',
  'SMOG.Grade',
  #'day.of.week',
  'reputation',
  'intercalations')
#'score',
#'number.of.answers',
#'max.answer.score',
#'avg.answer.score',
#'min.answer.score',
#'number.of.comments',
#'max.answer.length',
#'avg.answer.length',
#'min.answer.length',
#'has.accepted.answer')

numeric <- data[, numeric.fields]

#Cross correlation analysis
cor.table <- cor(numeric, method = "spearman")
corrplot(cor.table, method="circle", addrect = 2, type = "up") #order = "hclust"

#columns to remove to reduce pair-wise correlations.
correlated <- findCorrelation(cor.table, cutoff = 0.75, exact = T, names = T)
print(correlated)

# ML Task : Predict whether a pull request will be merged or not

#model <- as.factor(number.of.answers) ~ days.since.posted + 
model <-ratio ~ title.length + 
  avg.tag.popularity + 
  min.tag.popularity + 
  local.max.tag.popularity..no.java. + 
  java.. + 
  json.. + 
  xml.. + 
  stack.traces.. + 
  length + 
  words.count + 
  text.speak.count + 
  urls.count + 
  Coleman.Liau.Index + 
  Flesch.Kincaid.Grade.Level + 
  Gunning.Fog.Index + 
  SMOG.Grade + 
  reputation + 
  intercalations

# Split dataset into training and testing
samples <- sample(nrow(data), size = 0.25 * nrow(data))
test.data <- data[samples,]
train.data <- data[-samples,]

#samples <- sample(nrow(data), size = 0.95 * nrow(data))
#test.data <- data[samples,]
#train.data <- data[-samples,]

# Initial experiments with rf
#rf <- randomForest(model, data = train.data, importance = T, do.trace = T)
#print(rf)
#plot(rf)
#varImp(rf)
#varImpPlot(rf)

# Make a function for the above and start tuning
plot.rf <- function(rf) {
  print(rf)
  plot(rf)
  varImp(rf)
  varImpPlot(rf)
}

# Perhaps more variables per tree?
rfmodel <- randomForest(model, data = train.data, importance = T, do.trace = T,
                        mtry = 10, ntree = 100)
plot.rf(rfmodel)

predictions <- predict(rfmodel, test.data)
comp = predictions - test.data$ratio

cbind(predicted=predictions, actual = test.data$ratio)
accuracy(f= predictions, x= test.data$ratio)

correct = sum(comp == TRUE)
incorrect = sum(comp == FALSE)

accuracy = correct / nrow(test.data)

print("Accuracy RF")
print(accuracy)

a <- lm(model, data = train.data)
b <- predict(a, test.data)
#b <- round(b)
c <- b == test.data$ratio

c.correct = sum((b == test.data$ratio) == TRUE)
c.incorrect = sum((b == test.data$ratio) == FALSE)

print("Accuracy LR")
print(c.correct / (c.correct + c.incorrect))


run = c()
me = c()
rmse = c()
mae = c()
mpe = c()
mape = c()

classifier = c()

for (i in 1:10) {
  run[i] <- i
  classifier[i] <- "rf"
  
  data2 <- data[sample(nrow(data)),]
  samples <- sample(nrow(data2), size = 0.25 * nrow(data))
  test.data <- data2[samples,]
  train.data <- data2[-samples,]
  
  rfmodel <- randomForest(model, data = train.data, importance = T, do.trace = T,
                          mtry = 10, ntree = 100)
  #plot.rf(rfmodel)
  
  predictions <- predict(rfmodel, test.data)
  
  acc = accuracy(f= predictions, x= test.data$ratio)
  me[i] <- acc[1]
  rmse[i] <- acc[2]
  mae[i] <- acc[3]
  mpe[i] <- acc[4]
  mape[i] <- acc[5]
  print(i)
}

for (i in 1:10) {
  run[i+10] <- i
  classifier[i+10] <- "lr"
  
  data2 <- data[sample(nrow(data)),]
  samples <- sample(nrow(data2), size = 0.25 * nrow(data))
  test.data <- data2[samples,]
  train.data <- data2[-samples,]
  
  a <- lm(model, data = train.data)
  b <- predict(a, test.data)
  
  acc = accuracy(f= b, x= test.data$ratio)
  me[i+10] <- acc[1]
  rmse[i+10] <- acc[2]
  mae[i+10] <- acc[3]
  mpe[i+10] <- acc[4]
  mape[i+10] <- acc[5]
  print(i+10)
}

temps = NULL
temps = data.frame(run= run,
                   classifier = classifier,
                   me = me,
                   rmse = rmse,
                   mae = mae,
                   mpe = mpe,
                   mape = mape)

all.metrics <- rbind(rfmetrics, lrmetrics)
melted.metrics <- melt(temps, id.vars = c('classifier', 'run'))
ggplot(melted.metrics)+
  aes(x = run, y = value, colour = classifier) +
  geom_point(aes(shape = classifier), size = 3) +
  scale_shape(solid = F) +
  geom_line() +
  facet_wrap(~variable, scales="free_y") +
  theme_bw() +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(legend.key = element_blank()) +
  theme(legend.position = "top")

write.table(temps, file="/Users/tiago/Documents/lab/IN4334-06/R/results/Results_rq2/results.csv",sep=";", dec=",")

