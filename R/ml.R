library(ROCR)
library(randomForest)
library(e1071)
library(reshape)
library(foreach)
library(plyr)
library(data.table)

library(doMC)
registerDoMC(1)

printf <- function(...) invisible(print(sprintf(...)))

# calc ML metrics as in: Using Random Forest to Learn Imbalanced Data
classification.perf.metrics <- function(pred.obj,
                                        ground.truth,
                                        cutoff = ceiling(length(pred.obj@cutoffs[[1]]) / 2)) {
  acc <- performance(pred.obj, "acc")
  
  fp <- pred.obj@fp[[1]][cutoff]
  fn <- pred.obj@fn[[1]][cutoff]
  tp <- pred.obj@tp[[1]][cutoff]
  tn <- pred.obj@tn[[1]][cutoff]
  
  tnr <- tn / (tn + fp)
  tpr <- tp / (tp + fn)
  prec <- tp / (tp + fp)
  rec <- tpr
  
  acc <- (tp + tn)/(fp + fn + tp + tn)
  observed.acc <- acc
  
  trues  <- length(ground.truth[ground.truth=='TRUE'])
  falses <- length(ground.truth[ground.truth=='FALSE'])
  expected.acc <- (((tp + fp) * trues) / length(ground.truth) +
                     (fp + fn) * falses / length(ground.truth)) / length(ground.truth)
  
  result = data.frame(
    auc       = as.numeric(performance(pred.obj,"auc")@y.values),
    tnr       = tnr,
    tpr       = tpr,
    g.mean    = (tnr * tpr) ^ 0.5,
    w.acc     = 0.75 * tpr + 0.25 * tnr,
    prec      = prec,
    rec       = rec,
    f.measure = (2 * prec * rec) / (prec + rec),
    acc       = acc,
    k         = (observed.acc - expected.acc) / (1 - expected.acc)
  )
  print(sprintf("AUC %f, ACC %f, K: %f, W.ACC %f, PREC %f, REC %f, F-MEAS %f",
                result$auc, result$acc, result$k, result$w.acc,
                result$prec, result$rec, result$f.measure))
  result
}

binary.logistic.regression <- function(model, data, train.sizes = c(0.9)) {
  dependent <- all.vars(model)[1]
  
  metrics <-
    foreach(s = train.sizes, .combine=rbind) %:%
    foreach(r = c(1:10), .combine=rbind) %do% {
      sample.train <- sample(nrow(data), size = s * nrow(data))
      train.data <- data[sample.train,]
      test.data <- data[-sample.train,]
      
      blrmodel <- glm(model, data = train.data, family = "binomial")
      
      predictions <- predict(blrmodel, newdata = test.data)
      pred.obj <- prediction(predictions, test.data[,dependent])
      result <- classification.perf.metrics(pred.obj, test.data[,dependent])
      result$train.size <- s
      result$test.size <- nrow(test.data)
      result$classifier <- 'binlr'
      result$run <- r
      result
    }
  
  metrics
}

random.forest <- function(model, data, train.sizes = c(0.9)) {
  dependent <- all.vars(model)[1]
  metrics <-
    foreach(s = train.sizes, .combine=rbind) %:%
    foreach(r = c(1:10), .combine=rbind) %dopar% { 
      sample.train <- sample(nrow(data), size = s * nrow(data))
      train.data <- data[sample.train,]
      test.data <- data[-sample.train,]
      
      minority.class <- nrow(subset(train.data, accepted == T))
      rfmodel <- randomForest(model, data = train.data, importance = T,
                              do.trace = T, ntree = 200,
                              sampsize = c('TRUE' = minority.class,
                                           'FALSE' = (2 * minority.class)))
      #rfmodel <- randomForest(model, data = train.data, importance = T, do.trace = T,
      #                        mtry = 10, ntree = 200)
      
      
      predictions <- predict(rfmodel, test.data, type="prob")
      pred.obj <- prediction(predictions[,2], test.data[,dependent])
      result <- classification.perf.metrics(pred.obj, test.data[,dependent])
      result$train.size <- s
      result$test.size <- nrow(test.data)
      result$classifier <- "ranforest"
      result$run <- r
      result
    }
  
  metrics
}

naive.bayes <- function(model, data, train.sizes = c(0.9)) {
  dependent <- all.vars(model)[1]
  metrics <-
    foreach(s = train.sizes, .combine=rbind) %:%
    foreach(r = c(1:10), .combine=rbind) %dopar% {
      
      bayesModel <- naiveBayes(model, train.data)
      print(bayesModel)
      
      predictions <- predict(bayesModel, test.data, type="raw")
      pred.obj <- prediction(predictions[,2], test.data[,dependent])
      result <- classification.perf.metrics(pred.obj, test.data[,dependent])
      result$train.size <- s
      result$test.size <- nrow(test.data)
      result$classifier <- 'naivebayes'
      result$run <- r
      result
    }
  
  metrics
}