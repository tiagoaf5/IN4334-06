library(reshape)
library(ggplot2)
library(corrplot)
library(caret)

source('/Users/tiago/Documents/lab/IN4334-06/R/ml.R') # change to your path


delim = ","  # or is it "\t" ?
dec = "."    # or is it "," ?

#change to your path
answers <- read.csv("/Users/tiago/Documents/lab/IN4334-06/LargeAssignment/src/main/resources/results/answers_1451932330470_filtered-by_java.csv", header=TRUE, sep=delim, dec=dec, stringsAsFactors=FALSE)

data <- answers[complete.cases(answers ),]

cenas <- data$accepted == 1
data$accepted2 <- data$accepted
data$accepted <- as.factor(cenas)

numeric.fields <- c(#'days.since.posted',
                        'first.posted',
                        'same.day.as.question',
                        'question.view.count',
                        'q..tags.count',
                        'q..max.tag.popularity..no.java.',
                        'q..avg.tag.popularity',
                        'q..min.tag.popularity',
                        'q..local.max.tag.popularity..no.java.',
                        'q..local.avg.tag.popularity',
                        'q..local.min.tag.popularity',
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
                        'day.of.week',
                        'reputation',
                        'intercalations')
                        #'score',
                        #'number.of.comments',
                        #'accepted2',
                        #'max.comment.length',
                        #'avg.comment.length',
                        #'min.comment.length')

numeric <- data[, numeric.fields]

cor.table <- cor(numeric, method = "spearman")
corrplot(cor.table, method="circle", addrect = 2, type = "up") #order = "hclust"
 
correlated <- findCorrelation(cor.table, cutoff = 0.75, exact = T, names = T)
print(correlated)

model <- accepted ~ first.posted + 
  same.day.as.question + question.view.count + 
  q..max.tag.popularity..no.java. + q..avg.tag.popularity + 
  q..min.tag.popularity + json.. + 
  xml.. + stack.traces.. + 
  length + words.count + 
  text.speak.count + urls.count + 
  Coleman.Liau.Index + Flesch.Kincaid.Grade.Level + 
  Gunning.Fog.Index + SMOG.Grade + 
  day.of.week + reputation + 
  intercalations


# Split dataset into training and testing
samples <- sample(nrow(data), size = 0.25 * nrow(data))
test.data <- data[samples,]
train.data <- data[-samples,]

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
#rf <- randomForest(model, data = train.data, importance = T, do.trace = T,
#                   mtry = 10, ntree = 100)
#plot.rf(rf)

# Train a random forest model
rf.metrics <- random.forest(model, data)

# Train a binary logistic regression model
blr.metrics <- binary.logistic.regression(model, data)

# Train a naive bayes model
bayes.metrics <- naive.bayes(model, data)

all.metrics <- rbind(rf.metrics, blr.metrics, bayes.metrics)
all.metrics <- all.metrics[, -which(names(all.metrics) %in% c("tnr","tpr", "g.mean", "w.acc", "train.size", "test.size"))]
melted.metrics <- melt(all.metrics, id.vars = c('classifier', 'run'))
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
