# Machine Learning And Credit Default: An Interactive Analysis In R

#### Author: Patrick Rotter
#### Date: 30/11/2017
#### Description: prediction.R imports data_b.rds (see preparation.R) and creates all necessary models presented in the problem set. 
####              Furthermore, an abundance of alternative models/ implementations are presented.
####              Apart from comments, please see [Info] for further explanations.

##########################
#       packages         ####################################################################################################################
##########################

library(RTutor)

library(glmnet)
library(caret)
library(rpart)
library(randomForest)
library(xgboost)

# To measure the elapsed time
if(!require(tictoc)){
  install.packages("tictoc")
  library(tictoc)
}

# gbm algorithm is based on plyr functions
if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}
library(dplyr)

if(!require(kernlab)){
  install.packages("kernlab")
  library(kernlab)
}

if(!require(pROC)){
  install.packages("pROC")
  library(pROC)
}

if(!require(caTools)){
  install.packages("caTools")
  library(caTools)
}

# # Optional for multi-core support
# if(!require(doMC)){
#   install.packages("doMC")
#   library(doMC)
# }
# registerDoMC(cores = 8)

# Additonal packages for bagging and boosting
# Only required for models which are not featured in the problem set
# (Second part of this script)

# if(!require(party)){
#   install.packages("party")
#   library(party)
# }
# if(!require(gbm)){
#   install.packages("gbm")
#   library(gbm)
# }
# if(!require(ada)){
#   install.packages("ada")
#   library(ada)
# }
# if(!require(adabag)){
#   install.packages("adabag")
#   library(adabag)
# }
# if(!require(e1071)){
#   install.packages("e1071")
#   library(e1071)
# }

# Deal with multiple select statements
select = dplyr::select

##########################
#         Import         ####################################################################################################################
##########################

data = readRDS("data_b.rds")
  
# [Info] A sparse matrix (data = sparse.model.matrix( ~ ., data)) decreases run time and minimum hardware requirements (RAM) to run the 
#        script. However not all implementations which are part of this script support sparse matrices (e.g. randomForest). Hence, the
#        data set has not been converted to a sparse matrix. The same applies for missing values, in general, one might achieve better results, 
#        utilizing data sets customised to a respective machine larning algorithm instead of a generlized one-for-all data set.

##########################
#         Logging        ####################################################################################################################
##########################

if(file.exists("prediction_log.txt"))
  file.remove("prediction_log.txt")
cat("preparation.R log \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat("Import [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")

##########################
#        Indexing        ####################################################################################################################
##########################

# Generate the train_index
train_index <- with.random.seed(createDataPartition(data$default, 
                                                    p = 0.8, list = FALSE), seed = 5690)

# Divide our data set into test (20%) and remaining observations (80%)
test <- data[-train_index,]
left <- data[train_index,]

# Generate the tune_index
tune_index <- with.random.seed(createDataPartition(left$default, 
                                                   p = 0.125, list = FALSE), seed = 5690)

# Split our remaining observations into tune (10%) and training data (70%)
# of total observations
tune <- left[tune_index,]
train <- left[-tune_index,]

# Clear RAM
rm(data, tune_index, train_index)
gc()

# Logging
cat("Settings and Indexing [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")

##########################
#   Logistic Regression  ####################################################################################################################
##########################

# Start counting ############################
tic()

# Logistic regression
# Inspired by Emekter, et al. (2015)
log <- glm(default ~ grade_B + grade_C + grade_D + grade_E + grade_F + grade_G + dti + mean_fico_range + revol_util, family = binomial(link=logit), data = left)

# Predict the outcome utilizing our test data
prediction_log <- predict(log, select(test, -default), type="response")

# Compute the ROC curve
roc_log <- roc(response = test$default, predictor = as.numeric(prediction_log))

############################## End counting #
elapsed_log <- toc()
elapsed_log <- elapsed_log$toc-elapsed_log$tic

# Alternatively, calculate the AUC manually as shown in the problem set:

# Order the probablities decreasingly and maintain the index
prediction_log_ordered <- sort(prediction_log, 
                               decreasing = TRUE, 
                               index.return = TRUE)

# Extract the probabilities and their index separately
probabilities <- as.numeric(prediction_log_ordered$x)
indeces <- prediction_log_ordered$ix

# Adjust the true values to the cecreasing order
true_value <- test$default[indeces]

# Calculate the true and false positive rate
# True positives divided by the sum of false positives and false negatives    
tpr = cumsum(true_value == "Default") / sum(true_value == "Default") 
# False positives divided by the sum of false positive and true negative
fpr = cumsum(true_value == "Paid") / sum(true_value == "Paid")

# Calculate the AUC
# Width: (fpr[2:length(true_value)] - fpr[1:length(true_value)-1])
# Height: tpr[2:length(true_value)]
auc = sum((fpr[2:length(true_value)] - fpr[1:length(true_value)-1]) 
          * tpr[2:length(true_value)])

# [Info] We consider all steps which are required to arrive at
#        the final AUC value, as the times required to train the
#        model and to perform predictions differ significantly 
#        accross models

####################################################################################################################

# Start counting ############################
tic()

# Full rank logistic regression model
log_full <- glm(default ~ ., family = binomial(link=logit), data = left)

# Predict the outcome utilizing our test data
prediction_log_full <- predict(log_full, select(test, -default), type="response")

# Compute the ROC curve
roc_log_full <- roc(response = test$default, predictor = as.numeric(prediction_log_full))

############################## End counting #
elapsed_log_full <- toc()
elapsed_log_full <- elapsed_log_full$toc-elapsed_log_full$tic

# Print AUC 
roc_log$auc  # Area under the curve: 0.7008
roc_log_full$auc # Area under the curve: 0.7339

# Print summary
summary(log)
summary(log_full)

# Logging
cat("Logistic Regression [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_log \n AUC: ", roc_log$auc, "\n TIME: ", elapsed_log, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")
cat(c("auc_log_full \n AUC: ", roc_log_full$auc, "\n TIME: ", elapsed_log_full, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
# [Info] We use save instead of saveRDS, as we bundle multiple objects in a single file
#        You may load any .rdata file with load("yourfilename.rdata")
save(log, log_full, prediction_log, prediction_log_full, roc_log, roc_log_full, elapsed_log, elapsed_log_full, file = "log.rdata")
# [Info] log_ps.rdata omitts log_full, log due to size (GitHub <100MB & RAM concerning the host system)
save(prediction_log, prediction_log_full, roc_log, roc_log_full, elapsed_log, elapsed_log_full, file = "log_ps.rdata") 
rm(log, log_full, prediction_log, prediction_log_full, roc_log, roc_log_full, elapsed_log, elapsed_log_full)
gc()

##########################
#   Logit Reg - glmnet   ####################################################################################################################
##########################

# Start counting ############################
tic()

# Utilize our tune data set to determine our tuning parameters alpha and lambda
log_tuned_param <- with.random.seed(
  train(# Drop the default column from tune, as default is our
    # binary response
    x = select(tune, -default),
    y = tune$default,
    method = "glmnet",
    tuneGrid = expand.grid(alpha = seq(0, 1, .05),
                           lambda = seq(0, 1, .05)),
    metric = "ROC",
    trControl = trainControl(method = "cv",
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE,
                             number = 10)), seed = 5690)

# Estimate our model utilizing our training data set
log_tuned <- glmnet(x = as.matrix(select(train, -default)),
                    y = train$default,
                    # Pass the estimated alpha and lambda stored in
                    # log_tuned_param$bestTune
                    alpha = log_tuned_param$bestTune$alpha,
                    lambda = log_tuned_param$bestTune$lambda,
                    family = "binomial",
                    standardize = FALSE)

# Predict the outcome utilizing our test data set
prediction_log_tuned <- predict(log_tuned, newx = as.matrix(select(test, -default)),
                                type="response")

# Compute the ROC curve
roc_log_tuned <- roc(response = test$default, predictor = as.numeric(prediction_log_tuned))

############################## End counting #
elapsed_log_tuned <- toc()
elapsed_log_tuned <- elapsed_log_tuned$toc-elapsed_log_tuned$tic

# Print AUC
roc_log_tuned$auc # Area under the curve: 0.7339

# Logging
cat("LR - glmnet [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_log_tuned \n AUC: ", roc_log_tuned$auc, "\n TIME: ", elapsed_log_tuned, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(log_tuned_param, log_tuned, prediction_log_tuned, roc_log_tuned, elapsed_log_tuned, file = "log_tuned.rdata")
rm(log_tuned_param, log_tuned, prediction_log_tuned, roc_log_tuned, elapsed_log_tuned)
gc()

##########################
#  Classification Tree   ####################################################################################################################
##########################

# Start counting ############################
tic()

# Grow the tree
tree <- with.random.seed(
        rpart(default ~ grade_B + grade_C + grade_D + grade_E + grade_F + grade_G + dti + mean_fico_range + revol_util,
              # Without tuning the tree, the data set left
              # corresponds to the training data set
              data = left, 
              control = rpart.control(minsplit = 20, 
                                      minbucket = 100, 
                                      cp = 0.0005,
                                      method = "class")), seed = 5690)

# Complexity Levels
tree$cptable

# Predict the outcome utilizing our test data set
prediction_tree <- predict(tree, type = "prob", newdata = select(test, -default))

# Compute the ROC curve
roc_tree <- roc(test$default, prediction_tree[,1])

############################## End counting #
elapsed_tree <- toc()
elapsed_tree <- elapsed_tree$toc-elapsed_tree$tic

####################################################################################################################

# Start counting ############################
tic()

# Pruning
tree_pruned <- prune(tree , cp = 0.00066)

# Predict the outcome utilizing our test data set
prediction_tree_pruned <- predict(tree_pruned, type = "prob", newdata = select(test, -default))

# Compute the ROC curve
roc_tree_pruned <- roc(test$default, prediction_tree_pruned[,1])

############################## End counting #
elapsed_tree_pruned <- toc()
elapsed_tree_pruned <- elapsed_tree_pruned$toc-elapsed_tree_pruned$tic

####################################################################################################################

# Start counting ############################
tic()

# Utilize our tune data set to determine complexity
tree_param <- with.random.seed(
  train(x = select(tune, -default),
        y = tune$default,
        method = "rpart",
        tuneGrid = expand.grid(cp = seq(0.0001, 0.025, 0.0001)),
        metric = "ROC",
        trControl = trainControl(method = "cv",
                                 summaryFunction = twoClassSummary,
                                 classProbs = TRUE,
                                 number = 10)), seed = 5690)

# Optimal Complexity Parameter
tree_param$bestTune

# See https://www.statmethods.net/advstats/cart.html
# Develop the tree utilizing library(rpart)
# [Info] rpart.control parameters are lower than default, as previously, no tree was grown
tree_tuned <- with.random.seed(
  rpart(default ~ .,
        data = train, 
        control = rpart.control(minsplit = 20, 
                                minbucket = 100, 
                                cp = tree_param$bestTune$cp),
        method = "class"), seed = 5690)

# Complexity Levels
tree_tuned$cptable

# Predict the outcome utilizing our test data set
prediction_tree_tuned <- predict(tree_tuned, type = "prob", newdata = select(test, -default))

# Compute the ROC curve
roc_tree_tuned <- roc(test$default, prediction_tree_tuned[,1])

############################## End counting #
elapsed_tree_tuned <- toc()
elapsed_tree_tuned <- elapsed_tree_tuned$toc-elapsed_tree_tuned$tic

# Print AUC
roc_tree$auc
roc_tree_pruned$auc
roc_tree_tuned$auc

# Print summary/tree
summary(tree)
tree
tree$frame

# Bonus
# Plot
plot(tree, uniform=TRUE); text(tree, use.n = TRUE, all = TRUE, cex = .8)

# Plot the pruned tree
plot(tree_pruned, uniform=TRUE); text(tree_pruned, use.n = TRUE, all = TRUE, cex = .8)

# Plot the tuned tree
plot(tree_tuned, uniform=TRUE); text(tree_tuned, use.n = TRUE, all = TRUE, cex = .8)

# Logging
cat("Classification Tree [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_tree \n AUC: ", roc_tree$auc, "\n TIME: ", elapsed_tree, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")
cat(c("auc_tree_pruned \n AUC: ", roc_tree_pruned$auc, "\n TIME: ", elapsed_tree_pruned, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")
cat(c("auc_tree_tuned \n AUC: ", roc_tree_tuned$auc, "\n TIME: ", elapsed_tree_tuned, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")


# Save results and clear RAM
save(tree_param, tree, tree_pruned, tree_tuned, prediction_tree_pruned, prediction_tree, prediction_tree_tuned, roc_tree_pruned, roc_tree, roc_tree_tuned, elapsed_tree, elapsed_tree_pruned, elapsed_tree_tuned, file = "tree.rdata")
rm(tree_param, tree, tree_pruned, tree_tuned, prediction_tree_pruned, prediction_tree, prediction_tree_tuned, roc_tree_pruned, roc_tree, roc_tree_tuned, elapsed_tree, elapsed_tree_pruned, elapsed_tree_tuned)
gc()

##########################
#      randomForest      ####################################################################################################################
##########################

# Start counting ############################
tic()

# Utilize our tune data set to determine our tuning parameters mtry 
randomForest_param <- with.random.seed(
  train(x = select(tune, -default),
        y = tune$default,
        method = "rf",
        tuneGrid = expand.grid(mtry = seq(2, 20, 2)), 
        metric = "Accuracy",
        trControl = trainControl(method = "oob",
                                 classProbs = TRUE)), seed = 5690)

# Build Random Forest
randomForest <- randomForest(y = train$default,
                             x = select(train, -default),
                             # Utilizing OOB requires a lot of available RAM,
                             # hence the small amount of trees
                             ntree = 750,
                             mtree = randomForest_param$bestTune$mtry,
                             importance = TRUE,
                             data = train)

# Build Random Forest - including test error calculation
# randomForest <- randomForest(y = train$default,
#                              x = select(train, -default),
#                              ytest = test$default,
#                              xtest = select(test, -default),
#                              ntree = 2000, mtree = .mtry, data = train)

# Predict the outcome utilizing our test data set
prediction_randomForest <- predict(randomForest, type = "prob", newdata = select(test, -default))

# Compute the ROC curve
roc_randomForest <- roc(test$default, prediction_randomForest[,1])

############################## End counting #
elapsed_randomForest <- toc()
elapsed_randomForest <- elapsed_randomForest$toc-elapsed_randomForest$tic

# Print AUC
roc_randomForest$auc # Area under the curve: 0.7346

# Classes
cm_randomForest <- randomForest$confusion

# Mean Decrease in Gini Index
mdg_randomForest <- randomForest::importance(randomForest)

# Variable Importance
varImpPlot(randomForest,
           sort = T,
           n.var = 10)

# Logging
cat("randomForest [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_randomForest \n AUC: ", roc_randomForest$auc, "\n TIME: ", elapsed_randomForest, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(randomForest_param, randomForest, prediction_randomForest, roc_randomForest, mdg_randomForest, cm_randomForest, elapsed_randomForest, file = "randomForest.rdata")
# [Info] rf_ps.rdata omitts the randomForest model due to size
save(randomForest_param, prediction_randomForest, roc_randomForest, mdg_randomForest, cm_randomForest, elapsed_randomForest, file = "rf_ps.rdata")

rm(randomForest_param, randomForest, prediction_randomForest, roc_randomForest, mdg_randomForest, cm_randomForest, elapsed_randomForest)
gc()

##########################
#       Boosting         ####################################################################################################################
##########################

# Start counting ############################
tic()

# Utilize our tune data set to determine our tuning parameters 
gbm_param <- with.random.seed(
  train(y = tune$default,
        x = as.matrix(select(tune, -default)),
        method = "xgbTree",
        trControl = trainControl(method = "cv", 
                                 number = 10, 
                                 # Save losses across models
                                 returnResamp = "all",
                                 classProbs = TRUE),
        tuneGrid = expand.grid(# Number of trees to grow
          nrounds = 1000,
          # Learning rate
          eta = c(0.01, 0.001, 0.0001),
          # Maximum tree depth
          max_depth = c(5, 10, 20),
          # Minimum loss reduction (the larger gamma,
          # the less restricted)
          gamma = c(0.33, 0.66, 1),
          # Sub sample ratio of columns in each tree
          colsample_bytree = c(0.5, 1),
          # Avoids very small nodes
          min_child_weight = 1,
          # Ratio of data utilized to train the model
          # A lower ratio may prevent overfitting, we 
          # utilize the whole tuning data set
          subsample = 1)
  ), seed = 5690)

# temporary save
save(gbm_param, file = "gbm_param.rdata")

# Estimate our model utilizing our training data set
# See https://cran.r-project.org/web/packages/xgboost/vignettes/xgboostPresentation.html
gbm <- with.random.seed(
  xgboost(data = as.matrix(select(train, -default)),
          label = ifelse(train$default == "Default", 1, 0),
          params = list(# binary classification
            objective = "binary:logistic",
            eta = gbm_param$bestTune$eta,
            gamma = gbm_param$bestTune$gamma,
            max_depth = gbm_param$bestTune$max_depth,
            min_child_weight = gbm_param$bestTune$min_child_weight,
            subsample = gbm_param$bestTune$subsample,
            colsample_bytree = gbm_param$bestTune$colsample_bytree,
            # Metric
            eval_metric = "auc"),
          nrounds = gbm_param$bestTune$nrounds,
          # Print progress
          verbose = TRUE,
          # Truncation condition, if no further improvement after 250 trees
          early_stopping_rounds = 250), seed = 5690)

# Print test error
# err <- mean(as.numeric(pred > 0.5) != test$label)
# print(paste("test-error=", err))

# Predict the outcome utilizing our test data set
prediction_gbm <- predict(gbm, xgb.DMatrix(as.matrix(select(test, -default)), label = ifelse(test$default == "Default", 1, 0)))

# Compute the ROC curve
roc_gbm <- roc(test$default, prediction_gbm) 

############################## End counting #
elapsed_gbm <- toc()
elapsed_gbm <- elapsed_gbm$toc-elapsed_gbm$tic

# Print AUC
roc_gbm$auc # Area under the curve: 0.7482

# Print summary
summary(gbm)

# Compute an importance matrix
importance_mat <- xgb.importance(colnames(train), model = gbm)

# Logging
cat("eXtreme Gradient Boosting [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_gbm \n AUC: ", roc_gbm$auc, "\n TIME: ", elapsed_gbm, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(gbm_param, gbm, prediction_gbm, roc_gbm, elapsed_gbm, importance_mat, file = "xgbm.rdata")
rm(gbm_param, gbm, prediction_gbm, roc_gbm, elapsed_gbm, importance_mat)
gc()

############################################################################################################################################################
#                                  The following models were not used in the problem set, provide however good alternatives                                #
############################################################################################################################################################

##########################
#        Bagging         ####################################################################################################################
##########################

# Bagged ranger RF
rangerBag <- train(y = train$default,
                   x = select(train, -default),
                   method = "ranger",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            classProbs = TRUE),
                   tuneGrid = expand.grid(mtry = c(5, .mtry)),
                   metric = "Accuracy")

# Predict the outcome utilizing our test data set
prediction_rangerBag <- predict(rangerBag, select(test, -default), type = "prob")

# Compute the ROC curve
roc_rangerBag <- roc(test$default, prediction_rangerBag$Default)

# Calculate the AUC
auc_rangerBag <- pROC::auc(roc_rangerBag)

# Print AUC
auc_rangerBag # Area under the curve: ?.????

# Print summary
summary(rangerBag)

# Logging
cat("Bagged ranger [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_rangerBag \t", roc_rangerBag$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(rangerBag, prediction_rangerBag, roc_rangerBag, auc_rangerBag, file = "rangerBag.rdata")
rm(rangerBag, prediction_rangerBag, roc_rangerBag, auc_rangerBag)
gc()

####################################################################################################################

# Bagged ADA Boost
adaBag <- train(y = train$default,
                x = select(train, -default),
                method = "AdaBag",
                trControl = trainControl(method = "cv",
                                         number = 10,
                                         classProbs = TRUE),
                tuneGrid = expand.grid(mfinal = 500,
                                       maxdepth = c(2,5)),
                metric = "Accuracy")

# Predict the outcome utilizing our test data set
prediction_adaBag <- predict(adaBag, type = "prob", newdata = select(test, -default))

# Compute the ROC curve
roc_adaBag <- roc(test$default, prediction_adaBag$Default)

# Calculate the AUC
auc_adaBag <- pROC::auc(roc_adaBag)

# Print AUC
auc_adaBag # Area under the curve: ?.????

# Print summary
summary(adaBag)

# Logging
cat("Bagged AdaBoost [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_adaBag \t", roc_adaBag$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(adaBag, prediction_adaBag, roc_adaBag, auc_adaBag, file = "adaBag.rdata")
rm(adaBag, prediction_adaBag, roc_adaBag, auc_adaBag)
gc()

####################################################################################################################

# Bagged randomForest RF
rfBag <- train(y = train$default,
               x = select(train, -default),
               method = "rf",
               trControl = trainControl(method = "cv",
                                        number = 10,
                                        classProbs = TRUE),
               tuneGrid = expand.grid(mtry = c(5, .mtry)),
               metric = "Accuracy")

# Predict the outcome utilizing our test data set
prediction_rfBag <- predict(rfBag, select(test, -default), type = "prob")

# Compute the ROC curve
roc_rfBag <- roc(test$default, prediction_rfBag$Default)

# Calculate the AUC
auc_rfBag <- pROC::auc(roc_rfBag)

# Print AUC
auc_rfBag # Area under the curve: 0.7311

# Print summary/rfBag
summary(rfBag)
rfBag

# Logging
cat("Bagged randomForest [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_rfBag \t", roc_rfBag$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(rfBag, prediction_rfBag, roc_rfBag, auc_rfBag, file = "rfBag.rdata")
rm(rfBag, prediction_rfBag, roc_rfBag, auc_rfBag)
gc()

##########################
#        Bagging         ####################################################################################################################
##########################

# # Tree Bagging
# # https://rdrr.io/cran/caret/man/bag.html
# 
# treebag <- bag(y = left$default,
#                x = select(left, -default),
#                # Number of bootstrap samples
#                B = 10, 
#                vars = 5,
#                # Downsampling (Balancing default and paid ratio)
#                downSample = FALSE,
#                bagControl = bagControl(fit = ctreeBag$fit, predict = ctreeBag$pred, aggregate = ctreeBag$aggregate,
#                                        oob = TRUE, allowParallel = TRUE))
# 
# # Predict the outcome utilizing our test data set
# prediction_treebag <- predict(treebag, select(test, -default), type = "prob")
# 
# # Compute the ROC curve
# roc_treebag <- roc(test$default, prediction_treebag$Default) 
# 
# # Calculate the AUC
# auc_treebag <- pROC::auc(roc_treebag)
# 
# # Print AUC
# auc_treebag
# 
# # Print summary
# summary(treebag)
# 
# # Logging
# cat("treebag [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
# cat(c("auc_treebag \t", roc_treebag$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")
# 
# # Save results and clear RAM
# save(treebag, prediction_treebag, roc_treebag, auc_treebag, file = "treebag.rdata")
# rm(treebag, prediction_treebag, roc_treebag, auc_treebag)
# gc()

##########################
#  ranger Random Forest  ####################################################################################################################
##########################

# Random Forest via ranger package

# Utilize our tune data set to determine our tuning parameters mtry and splitrule
ranger_param <- with.random.seed(
  train(x = select(tune, -default),
        y = tune$default,
        method = "ranger",
        tuneGrid = expand.grid(mtry = seq(2, 20, 2)), 
        metric = "ROC",
        trControl = trainControl(method = "cv",
                                 summaryFunction = twoClassSummary,
                                 classProbs = TRUE,
                                 number = 10)), seed = 5690)

# See https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3250568/
ranger <- with.random.seed(
  ranger(default ~ ., 
         data = train, 
         num.trees = 3000, 
         replace = TRUE, 
         mtry = ranger_param$bestTune$mtry, 
         splitrule = "gini", 
         importance = "impurity", 
         probability = TRUE), seed = 5690)

# Predict the outcome utilizing our test data set
prediction_ranger <- predict(ranger, data = select(test, -default))

# Compute the ROC curve
roc_ranger <- roc(test$default, prediction_ranger$predictions[,1]) 
# Alternatively utilizing library(precrec)
# roc_ranger <- evalmod(labels = test$default, scores = as.numeric(prediction_ranger$predictions[,2]), auc = TRUE, plot = TRUE)

# Calculate the AUC
auc_ranger <- pROC::auc(roc_ranger)

# Print AUC
auc_ranger

# Print summary
summary(ranger)

# Mean Decrease in Gini Index
mdg_ranger <- ranger::importance(ranger)

# Logging
cat("ranger [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_ranger \t", roc_ranger$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(ranger_param, ranger, prediction_ranger, roc_ranger, auc_ranger, mdg_ranger, file = "ranger.rdata")
rm(ranger_param, ranger, prediction_ranger, roc_ranger, auc_ranger, mdg_ranger)
gc()

##########################
#       Boosting         ####################################################################################################################
##########################

# gradient boosting machine alternative to xgboost

# See http://www.jstatsoft.org/v28/i05/paper - p.10
# Utilize our tune data set to determine our tuning parameters alpha and lambda
gbm_param <- with.random.seed(
  train(y = tune$default,
        x = select(tune, -default),
        method = "gbm", 
        trControl = trainControl(method = "cv", 
                                 number = 10, 
                                 classProbs = TRUE), 
        tuneGrid = expand.grid(interaction.depth = c(2, 5, 10, 20),
                               n.trees = c(500, 1000, 2000), 
                               shrinkage = seq(.001, .05, .001),
                               n.minobsinnode = 10), 
        metric = "Accuracy"), seed = 5690)

# temporary save
save(gbm_param, file = "gbm_param.rdata")

# See https://www.r-bloggers.com/using-a-gbm-for-classification-in-r/
# Estimate our model utilizing our training data set
gbm <- with.random.seed(
  gbm.fit(x = as.matrix(select(train, -default)),
          y = ifelse(train$default == "Default", 1, 0),
          distribution = "bernoulli",
          interaction.depth = gbm_param$bestTune$interaction.depth,
          n.trees =  gbm_param$bestTune$n.trees,
          shrinkage = gbm_param$bestTune$shrinkage,
          n.minobsinnode = gbm_param$bestTune$n.minobsinnode,
          nTrain = round(nrow(train)*0.8)), seed = 5690)

# Predict the outcome utilizing our test data set
prediction_gbm <- predict(gbm, newdata = select(test, -default), type = "response")

# Compute the ROC curve
roc_gbm <- roc(test$default, prediction_gbm) 

# Calculate the AUC
auc_gbm <- pROC::auc(roc_gbm)

# Print AUC
auc_gbm # Area under the curve: 

# Print summary
summary(gbm)

# Performance Plot
gbm.perf(gbm)

# Logging
cat("Gradient Boosting Machine [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_gbm \t", roc_gbm$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

save(gbm_param, gbm, prediction_gbm, roc_gbm, auc_gbm, file = "gbm.rdata")
rm(gbm_param, gbm, prediction_gbm, roc_gbm, auc_gbm)
gc()

####################################################################################################################

# Logit Boost
log_boost = train(y = train$default,
                  x = select(train, -default),
                  method = "LogitBoost",
                  trControl = trainControl(method = "cv", 
                                           number = 10, 
                                           classProbs = TRUE),
                  tuneGrid = expand.grid(nIter = 750),
                  metric = "Accuracy")

# Predict the outcome utilizing our test data set
prediction_log_boost <- predict(log_boost, select(test, -default), type = "prob")

# Compute the ROC curve
roc_log_boost <- roc(test$default, prediction_log_boost$Default) 

# Calculate the AUC
auc_log_boost <- pROC::auc(roc_log_boost)

# Print AUC
auc_log_boost # Area under the curve: 0.668

# Print 
log_boost

# Logging
cat("Logit Boost [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_log_boost \t", roc_log_boost$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(log_boost, prediction_log_boost, roc_log_boost, auc_log_boost, file = "log_boost.rdata")
rm(log_boost, prediction_log_boost, roc_log_boost, auc_log_boost)
gc()

####################################################################################################################

# ADA Boost
ada = train(y = train$default,
            x = select(train, -default),
            method = "ada",
            trControl = trainControl(method = "cv", 
                                     number = 10, 
                                     classProbs = TRUE),
            # ADA performance 
            tuneGrid = expand.grid(iter = 750,
                                   maxdepth = 4,
                                   nu = c(.1, .01)),
            metric = "Accuracy"
)

# Predict the outcome utilizing our test data set
prediction_ada <- predict(ada, select(test, -default), type = "prob")

# Compute the ROC curve
roc_ada <- roc(test$default, prediction_ada$Default) 

# Calculate the AUC
auc_ada <- pROC::auc(roc_ada)

# Print AUC
auc_ada # Area under the curve: 0.7358

# Print 
ada

# Logging
cat("Ada Boosting [x] \n==============================================", file="prediction_log.txt", append=TRUE, sep = "\n")
cat(c("auc_ada \t", roc_ada$auc, "\n==============================================\n"), file="prediction_log.txt", append=TRUE, sep = "")

# Save results and clear RAM
save(ada, prediction_ada, roc_ada, auc_ada, file = "ada.rdata")
rm(ada, prediction_ada, roc_ada, auc_ada)
gc()

##########################
#          FINIS         ####################################################################################################################
##########################


