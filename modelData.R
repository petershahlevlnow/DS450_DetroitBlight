# Build several models and compare performance 
# 1. Add blight classification column
# 2. split data into training and testing set
# 3. Baseline model logistic regression with K-fold cross validation
# 4. CART with K-fold cross validation

library(caret)
library(ROCR)

# add a classification column for blight
detAll$blight <- as.factor(ifelse(detAll$nBlight > 0, "Yes", "No"))

# split data
set.seed(36924)
perc.split <- 0.5
row.samp <- sample(1:nrow(detAll), perc.split*nrow(detAll))
detAll.train <- detAll[row.samp, ]
detAll.test <- detAll[-row.samp, ]

# select data for the model
detAll.train.trim <- detAll.train %>% select(-nCrime, -n311, -nBlight, -n, -loc.id, - lat, -long, -ng.hood)
detAll.test.trim <- detAll.test %>% select(-nCrime, -n311, -nBlight, -n, -loc.id, - lat, -long, -ng.hood)

# baseline regression

logit.base <- glm(blight ~ ., data = detAll.train.trim, family = "binomial")

logit.base.pred <- predict(logit.base, detAll.test.trim %>% select(-blight))
pred <- prediction(as.numeric(logit.base.pred), as.numeric(detAll.test.trim$blight))

prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))

# k fold glmnet logistic, with constant L1 nad L2

# setup train control
# fitControl <- trainControl(## 5-fold CV
#                             method = "cv",
#                             number = 3,
#                             classProbs = TRUE)
# 
# logit.reg <- train( x = model.matrix(~. -1, detAll.train.trim[, 1:65]),
#                     y = detAll.train.trim[, 66],
#                     method = "glmnet",
#                     family = "binomial",
#                     trControl = fitControl,
#                     nlamda = 20,
#                     alpha = 0.5,
#                     verbose = FALSE)

mod.train <- model.matrix(blight ~ . -1, data = detAll.train.trim)
logit.reg <- cv.glmnet(mod.train, detAll.train.trim$blight, nfolds = 3, family = "binomial", nlambda = 20, alpha = 0.5)


logit.reg.pred <- predict(logit.reg, model.matrix(blight ~. -1, detAll.test.trim))
pred <- prediction(as.numeric(logit.reg.pred), as.numeric(detAll.test.trim$blight))

prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))

# regression tree
library(rpart)
library(rpart.plot)

fitControl <- trainControl(## 5-fold CV
                            method = "cv",
                            number = 10,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)

rpart.model <- train(x = detAll.train.trim[, 1:65],
                     y = detAll.train.trim[, 66],
                     method = "rpart1SE",
                     trControl = fitControl,
                     control = rpart.control(maxdepth = 10),
                     metric = "ROC")

summary(rpart.model$finalModel)
rpart.plot(rpart.model$finalModel)
printcp(rpart.model$finalModel)

rpart.pred <- predict(rpart.model$finalModel, detAll.test.trim %>% select(-blight))
pred <- prediction(as.numeric(rpart.pred[,2]), as.numeric(detAll.test.trim$blight))

prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))

################## redo modelling with frequency features only ############

# select data for the model
detAll.train.trim <- detAll.train %>% select(nCrime, n311, d.price, blight)
detAll.test.trim <- detAll.test %>% select(nCrime, n311, d.price, blight)

# Base
logit.base <- glm(blight ~ ., data = detAll.train.trim, family = "binomial")

logit.base.pred <- predict(logit.base, detAll.test.trim %>% select(-blight))
pred <- prediction(as.numeric(logit.base.pred), as.numeric(detAll.test.trim$blight))

prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))

# regularized logistic

mod.train <- model.matrix(blight ~ . -1, data = detAll.train.trim)
logit.reg <- cv.glmnet(mod.train, detAll.train.trim$blight, nfolds = 10, family = "binomial", nlambda = 20, alpha = 0.5)


logit.reg.pred <- predict(logit.reg, model.matrix(blight ~. -1, detAll.test.trim))
pred <- prediction(as.numeric(logit.reg.pred), as.numeric(detAll.test.trim$blight))

prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))

# tree based 

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

rpart.model <- train(x = detAll.train.trim[, 1:3],
                     y = detAll.train.trim[, 4],
                     method = "rpart1SE",
                     trControl = fitControl,
                     control = rpart.control(maxdepth = 10),
                     metric = "ROC")

summary(rpart.model$finalModel)
rpart.plot(rpart.model$finalModel, extra = 1)
printcp(rpart.model$finalModel)

rpart.pred <- predict(rpart.model$finalModel, detAll.test.trim %>% select(-blight))
pred <- prediction(as.numeric(rpart.pred[,2]), as.numeric(detAll.test.trim$blight))

prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pred, "auc")
print(paste("AUC=", auc@y.values[[1]], sep=""))
