##################################################################################
###                   R code for DSB Assignment 2 by Group 1                   ###
#                                    *..      .                                  #
#                                    *.       .                                  #
#                                    /..     .,                                  #
#                                    %@@@@@@@@#                                  #
#                                    %#((/*//((                                  #
#                                    /,%*,%  .,                                  #
#                                    /.(@ %  .,                                  #
#                                    *(/&*&#..,                                  #
#                                    %#((///((*                                  #
#                                    @@@@@ @@@@                                  #
#                                  (@@@@@@@@@@@@                                 #
#                                 @@@@@@@@@@@@@@@%                               #
#                                @@@@@@@@@@@@% @@@@                              #
#                                @#((/  . .*((*@@@#                              #
#                                ..((((    (%%    .                              #
#                                ,.@%%@&  %%%@   .,                              #
#                                ,.%  %,   %%@    .                              #
#                                ,.%@#%@  .%%@    .                              #
#                                . #@ #@   %%@   ..                              #
#                                .&#  #%.  %#@   .,                              #
#                                .%&( ##&%.%#&// .,                              #
#                                ,///.*,,,.,...*  ,                              #
#                                ,.              .,                              #
#                                &.&& % && &&##%/..                              #
#                                &*,&(&/%,&..(.&/..                              #
#                                %&&&&&&&%&&&&&%/ .                              #
#                                ...             ..                              #
#                                ,...      ... ,,..                              #
#                                %%#%#####%%######(                              #
#                                #(((((((((((((/((/                              #
#                                 @@@@@@@@@@@@@@@#                               #
###                                A1 STEAK SAUCE                              ###
##################################################################################

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071","partykit","rpart","randomForest","xgboost", "dplyr", "plyr","matrixStats") #Check, and if needed install the necessary packages

credit.data<-read.csv(file.choose(), header=TRUE, sep=",") #load the data into the dataframe

### FUNCTIONS TO PRE-LOAD ###
# Fixing incorrectly classified data types:
newTypes<-function(data_frame) {
  data_frame$SEX <- as.factor(data_frame$SEX)
  data_frame$EDUCATION <- as.factor(data_frame$EDUCATION)
  data_frame$MARRIAGE <- as.factor(data_frame$MARRIAGE)
  data_frame$PAY_1 <- as.factor(data_frame$PAY_1) 
  data_frame$PAY_2 <- as.factor(data_frame$PAY_2)
  data_frame$PAY_3 <- as.factor(data_frame$PAY_3)
  data_frame$PAY_4 <- as.factor(data_frame$PAY_4)
  data_frame$PAY_5 <- as.factor(data_frame$PAY_5)
  data_frame$PAY_6 <- as.factor(data_frame$PAY_6)
  data_frame$default_0 <- as.factor(data_frame$default_0)
  
  return(data_frame)
}

newTypes2<-function(data_frame) {
  data_frame$SEX <- as.factor(data_frame$SEX)
  data_frame$EDUCATION <- as.factor(data_frame$EDUCATION)
  data_frame$MARRIAGE <- as.factor(data_frame$MARRIAGE)
  data_frame$PAY_1 <- as.factor(data_frame$PAY_1) 
  data_frame$PAY_2 <- as.factor(data_frame$PAY_2)
  data_frame$PAY_3 <- as.factor(data_frame$PAY_3)
  data_frame$PAY_4 <- as.factor(data_frame$PAY_4)
  data_frame$PAY_5 <- as.factor(data_frame$PAY_5)
  data_frame$PAY_6 <- as.factor(data_frame$PAY_6)
  #data_frame$default_0 <- as.factor(data_frame$default_0)
  
  return(data_frame)
}

# Feature engineering
newFeatures<-function(data_frame) {
  
  # Creates new columns for "Percent Paid"
  data_frame$BILL_AMT2temp <- ifelse(data_frame$BILL_AMT2 <= "0",data_frame$PAY_AMT1,data_frame$BILL_AMT2)
  data_frame$BILL_AMT3temp <- ifelse(data_frame$BILL_AMT3 <= "0",data_frame$PAY_AMT2,data_frame$BILL_AMT3)
  data_frame$BILL_AMT4temp <- ifelse(data_frame$BILL_AMT4 <= "0",data_frame$PAY_AMT3,data_frame$BILL_AMT4)
  data_frame$BILL_AMT5temp <- ifelse(data_frame$BILL_AMT5 <= "0",data_frame$PAY_AMT4,data_frame$BILL_AMT5)
  data_frame$BILL_AMT6temp <- ifelse(data_frame$BILL_AMT6 <= "0",data_frame$PAY_AMT5,data_frame$BILL_AMT6)
  
  data_frame$PPaid_1 <- data_frame$PAY_AMT1/data_frame$BILL_AMT2temp
  data_frame$PPaid_2 <- data_frame$PAY_AMT2/data_frame$BILL_AMT3temp
  data_frame$PPaid_3 <- data_frame$PAY_AMT3/data_frame$BILL_AMT4temp
  data_frame$PPaid_4 <- data_frame$PAY_AMT4/data_frame$BILL_AMT5temp
  data_frame$PPaid_5 <- data_frame$PAY_AMT5/data_frame$BILL_AMT6temp
  
  # Fixes NA
  integer_reac<-1
  for (i in 31 : 35){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    }
  }
  
  # Creates 6 temporary columns and calculates percent of partial payment per client
  for (i in 7 : 12){
    data_frame[,paste0("temp",i-6)]<-as.numeric(ifelse(data_frame[,i]=="0","1","0"))
  }
  data_frame$PPartial <- (data_frame$temp1+data_frame$temp2+data_frame$temp3+data_frame$temp4+data_frame$temp5+data_frame$temp6)/6
  
  # Calculates percent of full payment per client 
  for (i in 7 : 12){
    data_frame[,paste0("temp",i-6)]<-as.numeric(ifelse(data_frame[,i]=="-1","1","0"))
  }
  data_frame$PFull <- (data_frame$temp1+data_frame$temp2+data_frame$temp3+data_frame$temp4+data_frame$temp5+data_frame$temp6)/6
  
  # Calculates percent of no payment per client 
  for (i in 7 : 12){
    data_frame[,paste0("temp",i-6)]<-as.numeric(ifelse(data_frame[,i]=="-2","1","0"))
  }
  data_frame$PNone <- (data_frame$temp1+data_frame$temp2+data_frame$temp3+data_frame$temp4+data_frame$temp5+data_frame$temp6)/6
  
  # Calculates percent of months where payment is made
  data_frame$PPay <- data_frame$PPartial + data_frame$PFull + data_frame$PNone
  
  # Calculates percent of late payment / never paid
  data_frame$PLate <- 1 - data_frame$PPay
  
  # Calculates percent of fully paid or no payment
  data_frame$PFullNone <- data_frame$PFull + data_frame$PNone
  
  
  # Drops the temporary columns
  data_frame = subset(data_frame, select = -c(BILL_AMT2temp,BILL_AMT3temp,BILL_AMT4temp,BILL_AMT5temp,BILL_AMT6temp,temp1,temp2,temp3,temp4,temp5,temp6) )
  
  return(data_frame)
}

# Custom groups rare categories 
newCat<-function(data_frame) {
  
  data_frame$EDUCATION <- revalue(data_frame$EDUCATION,c("0"="Others"))
  data_frame$EDUCATION <- revalue(data_frame$EDUCATION,c("4"="Others"))
  data_frame$EDUCATION <- revalue(data_frame$EDUCATION,c("5"="Others"))
  data_frame$EDUCATION <- revalue(data_frame$EDUCATION,c("6"="Others"))
  
  data_frame$MARRIAGE <- revalue(data_frame$MARRIAGE,c("0"="3"))
  
  data_frame$PAY_1 <- revalue(data_frame$PAY_1,c("5"="VeryLate"))
  data_frame$PAY_1 <- revalue(data_frame$PAY_1,c("6"="VeryLate"))
  data_frame$PAY_1 <- revalue(data_frame$PAY_1,c("7"="VeryLate"))
  data_frame$PAY_1 <- revalue(data_frame$PAY_1,c("8"="VeryLate"))
  
  data_frame$PAY_2 <- revalue(data_frame$PAY_2,c("5"="VeryLate"))
  data_frame$PAY_2 <- revalue(data_frame$PAY_2,c("6"="VeryLate"))
  data_frame$PAY_2 <- revalue(data_frame$PAY_2,c("7"="VeryLate"))
  data_frame$PAY_2 <- revalue(data_frame$PAY_2,c("8"="VeryLate"))
  data_frame$PAY_2 <- revalue(data_frame$PAY_2,c("1"="1&2"))
  data_frame$PAY_2 <- revalue(data_frame$PAY_2,c("2"="1&2"))
  
  
  data_frame$PAY_3 <- revalue(data_frame$PAY_3,c("5"="VeryLate"))
  data_frame$PAY_3 <- revalue(data_frame$PAY_3,c("6"="VeryLate"))
  data_frame$PAY_3 <- revalue(data_frame$PAY_3,c("7"="VeryLate"))
  data_frame$PAY_3 <- revalue(data_frame$PAY_3,c("8"="VeryLate"))
  data_frame$PAY_3 <- revalue(data_frame$PAY_3,c("1"="1&2"))
  data_frame$PAY_3 <- revalue(data_frame$PAY_3,c("2"="1&2"))
  
  data_frame$PAY_4 <- revalue(data_frame$PAY_4,c("5"="VeryLate"))
  data_frame$PAY_4 <- revalue(data_frame$PAY_4,c("6"="VeryLate"))
  data_frame$PAY_4 <- revalue(data_frame$PAY_4,c("7"="VeryLate"))
  data_frame$PAY_4 <- revalue(data_frame$PAY_4,c("8"="VeryLate"))
  data_frame$PAY_4 <- revalue(data_frame$PAY_4,c("1"="1&2"))
  data_frame$PAY_4 <- revalue(data_frame$PAY_4,c("2"="1&2"))
  
  data_frame$PAY_5 <- revalue(data_frame$PAY_5,c("5"="VeryLate"))
  data_frame$PAY_5 <- revalue(data_frame$PAY_5,c("6"="VeryLate"))
  data_frame$PAY_5 <- revalue(data_frame$PAY_5,c("7"="VeryLate"))
  data_frame$PAY_5 <- revalue(data_frame$PAY_5,c("8"="VeryLate"))
  data_frame$PAY_5 <- revalue(data_frame$PAY_5,c("1"="1&2"))
  data_frame$PAY_5 <- revalue(data_frame$PAY_5,c("1"="1&2"))
  
  data_frame$PAY_6 <- revalue(data_frame$PAY_6,c("5"="VeryLate"))
  data_frame$PAY_6 <- revalue(data_frame$PAY_6,c("6"="VeryLate"))
  data_frame$PAY_6 <- revalue(data_frame$PAY_6,c("7"="VeryLate"))
  data_frame$PAY_6 <- revalue(data_frame$PAY_6,c("8"="VeryLate"))
  data_frame$PAY_6 <- revalue(data_frame$PAY_6,c("1"="1&2"))
  data_frame$PAY_6 <- revalue(data_frame$PAY_6,c("1"="1&2"))
  
  return(data_frame)
}

# Not used
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }



### EXPLORING AND CLEANING THE LOADED DATA ###
str(credit.data) #show the structure of (data types in) the dataframe

# Correct data types and add new features
credit.data<-newTypes(credit.data)
credit.data<-newFeatures(credit.data)

# check for rare categories
table(credit.data$SEX)
table(credit.data$EDUCATION) #to group 0, 4, 5, 6 (<300 obs)
table(credit.data$MARRIAGE) #to group 0, 5 (<300 obs)
table(credit.data$PAY_1) #to group 5 onwards as "Very Late"
table(credit.data$PAY_2)
table(credit.data$PAY_3)
table(credit.data$PAY_4)
table(credit.data$PAY_5)
table(credit.data$PAY_6)
table(credit.data$default_0)
# Findings: Very low observation count for number of people who delayed for one month in repayment status 2,3,4,5,6, but not in 1. Should not group them with others, but can replace them with 2

# Custom group rare categories
credit.data<-newCat(credit.data) #Ignore the errors - non-consequential
#credit.data<-combinerarecategories(credit.data,300) #combine categories with <300 values in STCdata into "Other"

### MODELLING ###

# Initialising data 
set.seed(1) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = credit.data$default_0,
                               p = 15999/24000, list = FALSE)
training <- credit.data[ inTrain,]
testing <- credit.data[ -inTrain,]

#
# Stepwise logit regression
#
  model_logistic<-glm(default_0~ ., data=training, family="binomial"(link="logit"))
  summary(model_logistic) 
  ## Stepwise regressions. There are three aproaches to runinng stepwise regressions: backward, forward and "both"
  ## In either approach we need to specify criterion for inclusion/exclusion. Most common ones: based on information criterion (e.g., AIC) or based on significance  
  model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
  # + and - in the console shows whether it is included or excluded
  summary(model_logistic_stepwiseAIC) 
  
  par(mfrow=c(1,4))
  plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
  par(mfrow=c(1,1))
# Findings: Fails normal plot

  logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response") #Predict probabilities
  logistic_classification<-rep("1",8000)
  logistic_classification[logistic_probabilities<(1500/(1500+5000))]="0" #Predict classification 
  # This maximizes accuracy but may not be the key objective of management
  logistic_classification<-as.factor(logistic_classification)
  
  ###Confusion matrix  
  confusionMatrix(logistic_classification,testing$default_0,positive = "1") #Display confusion matrix
  
  ####ROC Curve
  logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
  logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
  plot(logistic_ROC) #Plot ROC curve
  
  ####AUC (area under curve)
  auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
  logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
  logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
  
  #### Lift chart
  plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
  
   
# 
# C-tree
#
  ctree_tree<-ctree(default_0~.,data=training) #Run ctree on training data
  plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)
  
  ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") #Predict probabilities
  ctree_classification<-rep("1",8000)
  ctree_classification[ctree_probabilities[,2]<(1500/(1500+5000))]="0"
  ctree_classification<-as.factor(ctree_classification)

  ###Confusion matrix  
  confusionMatrix(ctree_classification,testing$default_0,positive = "1")
  
  ####ROC Curve
  ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
  ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0) #Calculate errors
  ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
  plot(ctree_ROC_testing) #Plot ROC curve
  
  ####AUC (area under curve)
  auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
  ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
  ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
  
  #### Lift chart
  plotLift(ctree_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
  
#
# RPART
#
  
  CART_cp = rpart.control(cp = 0.0005) #set cp to a small number to "grow" a large tree
  
  rpart_tree<-rpart(default_0~.,data=training, method="class", control=CART_cp) #"Grow" a tree on training data
  
  prunned_rpart_tree<-prune(rpart_tree, cp=0.0014) #Prun the tree. Play with cp to see how the resultant tree changes
  # prunned_rpart_tree<-prune(rpart_tree, cp=0.2) #Very little branches
  plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)
  
  printcp(rpart_tree) # Understand the relationship between the cross-validated error, size of the tree and cp
  plotcp(rpart_tree) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error
  # the dotted line shows the confidence interval from the lowest error point. As tree size increase, model is too complex and overfitted. Any branches between 2 to 42 is good.Median of the range = 22. We went with 0.0014 which gives 23 leaves
  
  rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
  confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1") #Display confusion matrix
  
  rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
  rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0) #Calculate errors
  rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
  plot(rpart_ROC_testing) #Plot ROC curve
  
  auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
  rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
  rpart_auc_testing #Display AUC value
  
  plotLift(rpart_prediction_class,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
  
#
# Random Forest
#
  
  model_forest <- randomForest(default_0~ ., data=training, 
                               importance=TRUE,
                               cutoff = c(0.5, 0.5),type="classification", ntree = 500, mtry = 20, nodesize = 20, maxnodes = 30)
  # No good answers on how to determine hyperparameters of ntree, mtry, etc. 
  
  print(model_forest)   
  plot(model_forest)
  importance(model_forest)
  varImpPlot(model_forest)
  
  ###Finding predicitons: probabilities and classification
  forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
  forest_classification<-rep("1",8000)
  forest_classification[forest_probabilities[,2]<(15/65)]="0" #Predict classification using 0.5 threshold. 
  forest_classification<-as.factor(forest_classification)
  
  confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix.
  
  #There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=testing, type="response") 
  #But it by default uses threshold of 50%: 
  
  ####ROC Curve
  forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
  forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
  plot(forest_ROC) #Plot ROC curve
  
  ####AUC (area under curve)
  AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
  forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
  forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
  
  #### Lift chart
  plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
  
  ### An alternative way is to plot a Lift curve not by buckets, but on all data points
  Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
  plot(Lift_forest)
  
#
# XGboost
#
  training.x <-model.matrix(default_0~ ., data = training)
  testing.x <-model.matrix(default_0~ ., data = testing)
  
  model_XGboost<-xgboost(data = data.matrix(training.x[,-1]), 
                         label = as.numeric(as.character(training$default_0)), 
                         eta = 0.1,
                         max_depth = 20, 
                         nround=100, 
                         objective = "binary:logistic")
  # need to play with parameter to see if it gives a good results (e.g. nround)
  
  XGboost_prediction<-predict(model_XGboost,newdata=testing.x[,-1], type="response") #Predict classification (for confusion matrix)
  confusionMatrix(as.factor(ifelse(XGboost_prediction>(5000/(5000+1500)),1,0)),testing$default_0,positive="1") #Display confusion matrix
  
  ####ROC Curve
  XGboost_pred_testing <- prediction(XGboost_prediction, testing$default_0) #Calculate errors
  XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
  plot(XGboost_ROC_testing) #Plot ROC curve
  
  ####AUC
  auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
  XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
  XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
  
  #### Lift chart
  plotLift(XGboost_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### PREDICTION ###

credit.pred<-read.csv(file.choose(), header=TRUE, sep=",") #load the data into the dataframe

credit.pred<-newTypes2(credit.pred)
credit.pred<-newFeatures(credit.pred)
credit.pred<-newCat(credit.pred) #Ignore errors

str(credit.pred)

credit.pred$ID <- 24001:25000
credit.pred.x <-model.matrix(X~ ., data = credit.pred)

logistic_probabilities_pred<-predict(model_logistic_stepwiseAIC,newdata=credit.pred,type="response")
ctree_probabilities_pred<-predict(ctree_tree,newdata=credit.pred,type="prob")
rpart_probabilities_pred<-predict(prunned_rpart_tree,newdata=credit.pred, type="prob")
forest_probabilities_pred<-predict(model_forest,newdata=credit.pred,type="prob")
#XGboost_prediction<-predict(model_XGboost,newdata=credit.pred.x[,-1], type="response")

credit.pred.comb <- cbind(logistic_probabilities_pred,ctree_probabilities_pred,rpart_probabilities_pred,forest_probabilities_pred)
credit.pred.comb <- subset(credit.pred.comb, select = c(1,3,5,7))
colnames(credit.pred.comb) <- c("Logit","Ctree","Rpart","Forest")
str(credit.pred.comb)

credit.pred.comb <- as.data.frame(credit.pred.comb)
credit.pred.comb$max <- rowMaxs(credit.pred.comb)
credit.pred.comb$max <- apply(credit.pred.comb, 1, max)
credit.pred.comb$Pred <- ifelse(credit.pred.comb$max<=(1500/(1500+5000)),0,1)

write.csv(credit.pred.comb,"A1A2 DSB Application Predictions.csv")  

