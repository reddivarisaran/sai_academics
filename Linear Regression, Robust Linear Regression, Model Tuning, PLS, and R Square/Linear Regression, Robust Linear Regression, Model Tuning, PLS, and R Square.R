
#installing the library and the pakages :
library(caret)
library(AppliedPredictiveModeling)
library(caTools)
install.packages("pls")
library("pls")

#6.1 Data:
data(tecator)
?tecator
dim(absorp)
dim(endpoints)

#6.1(b) calculating PCA:
PCA_absorp <- prcomp(absorp, center = TRUE, scale. = TRUE)
per_Var_PCA = PCA_absorp$sd^2/sum(PCA_absorp$sd^2)*100
var(per_Var_PCA)
plot(per_Var_PCA, xlab="Components", ylab="% of Total Variance", type="l", main="Plot of PCA Analysis")

#6.2 (c) spliting the data:

absorp<-cbind(absorp,endpoints[,2])
absorp<-as.data.frame(absorp)
colnames(absorp)<-c(1:100,"fat")

index_absorp = sample(1:nrow(absorp), size=0.7*nrow(absorp))
absorp_train = absorp[index_absorp,]
absorp_test = absorp[-index_absorp,]
dim(absorp_train)
dim(absorp_test)

  # remove fat column:
  absorp_train_new = absorp_train[,-101]
  absorp_test_new = absorp_test[,-101]

  #preprocessing both train and test set with center and scale:
  #Tranforming training data:
  absorp_train_trans<- preProcess(absorp_train_new, method=c("center","scale"))
  absorp_train_trans_new<- predict(absorp_train_trans,absorp_train_new)
  #Transforming test data:
  absorp_test_trans<- preProcess(absorp_test_new, method=c("center","scale"))
  absorp_test_trans_new<- predict(absorp_test_trans,absorp_test_new)
  dim(absorp_train_trans_new)
  dim(absorp_test_trans_new)
#6.1(d) Predicting best predictive ability:
  #(a) linear regression:
  lm_Pred = lm(absorp_train[,101] ~ . , data = absorp_train_trans_new)
  summary(lm_Pred)
  
  #Compute the fat% for the new test samples 
  lm_Pred_1 = predict(lm_Pred, absorp_test_trans_new)
  
  # Evaluate the test performance using a caret function
  lm_Values = data.frame(obs = absorp_test[,101], pred = lm_Pred_1)
  defaultSummary(lm_Values) 
  train_control <- trainControl(method = "cv", number = 10)
  set.seed(100)
  lm_fit <- train(x = absorp_train_trans_new, y = absorp_train[,101], method = "lm", trControl = train_control)
  lm_fit 
  
  #(b) Robust linear regression: 
  set.seed(100)
  rlm_PCA <- train(x = absorp_train_trans_new, y = absorp_train[,101], method = "rlm", preProcess = "pca", trControl = train_control)
  #on test set
  test_Result <- data.frame(obs = absorp_test[,101], pred = predict(rlm_PCA, absorp_test_trans_new))
  defaultSummary(test_Result)
  
  #(c)Partial Least Squares (PLS)
  # using partial least Square with 20 components
  pls_fit = plsr(absorp_train[,101] ~ . , data = absorp_train_trans_new, ncomp = 20)
  
  #on test data 
  pls_Pred = predict(pls_fit, absorp_test_trans_new, ncomp = 20)
  pls_Value = data.frame(obs = absorp_test[,101], pred = pls_Pred[,,1])
  defaultSummary(pls_Value) 
  
  # 10 folds cross validation
  set.seed(100)
  pls <- train(x = absorp_train_trans_new, y = absorp_train[,101],method = "pls",tuneGrid = expand.grid(ncomp = 20), trControl = train_control)
  pls
  test_pls <- data.frame(obs = absorp_test[,101], pred = predict(pls, absorp_test_trans_new))
  defaultSummary(test_pls)
  
  #(d)Penalised models
  #installing the elasticnet package and library:
  install.packages("elasticnet")
  library("elasticnet")
  set.seed(10)
  lm_Ridge <- enet(x = as.matrix(absorp_train_trans_new), y = absorp_train[,101], lambda = 0.001)
  Ridge_Pred = predict(lm_Ridge, newx = as.matrix(absorp_test_trans_new), s=1,mode="fraction", type = "fit")
  #on test data
  Ridge_Value = data.frame(obs = absorp_test[,101], pred = Ridge_Pred$fit)
  defaultSummary(Ridge_Value) 
  #Experiment with different penalty lambda:
  enetGrid <- expand.grid(lambda = c(0, 0.01, .1), fraction = seq(.05, 1, length = 20))
  set.seed(100)
  enetTune <- train(x = absorp_train_trans_new, y = absorp_train[,101],method = "enet",tuneGrid = enetGrid,trControl = train_control,preProc = c("center", "scale" ))
  enetTune
  plot(enetTune)
  #(e)Model tuning
  set.seed(100)
  pls_Tune <- train(x = absorp_train_trans_new, y = absorp_train[,101], method = "pls", tuneGrid = expand.grid(ncomp = 1:20), trControl = train_control)
  pls_Tune
  test_Result_PLS <- data.frame(obs = absorp_test[,101],pred = predict(pls_Tune, absorp_test_trans_new))
  defaultSummary(test_Result_PLS)
  set.seed(100)
  pcr_Tune <- train(x = absorp_train_trans_new, y = absorp_train[,101], method = "pcr", tuneGrid = expand.grid(ncomp = 1:35), trControl = train_control)
  pcr_Tune 
  test_Result_PCR <- data.frame(obs = absorp_test[,101], pred = predict(pcr_Tune, absorp_test_trans_new))
  defaultSummary(test_Result_PCR)

  
#6.2 

#6.2(a) Loading the data:
library(AppliedPredictiveModeling)
data(permeability)
summary(permeability)
dim(permeability)
dim(fingerprints)

#6.2(b) using nearzerovar
lowfreq<-nearZeroVar(fingerprints)
fil_fp<-fingerprints[,-lowfreq]
dim(fil_fp)
fil_fp_new<-cbind(fil_fp,permeability)
dim(fil_fp_new)

#6.2(c)splitting data and processing PLS Model:
set.seed(100)
index_FFP = sample(1:nrow(fil_fp_new), size=0.7*nrow(fil_fp_new))
FFP_train = fil_fp_new[index_FFP,]
dim(FFP_train) 
FFP_test = fil_fp_new[-index_FFP,]
dim(FFP_test)

# remove permeability column before preprocess
FFP_train_new = FFP_train[,-389]
FFP_test_new = FFP_test[,-389]

#preprocessing both train and test set with center and scale
FFP_train_trans<- preProcess(FFP_train_new, method=c("center","scale"))
FFP_train_trans_new<- predict(FFP_train_trans,FFP_train_new)

FFP_test_trans<- preProcess(FFP_test_new, method=c("center","scale"))
FFP_test_trans_new<- predict(FFP_test_trans,FFP_test_new)

  #partial least square for ncomp=5
  set.seed(100)
  ctrl<-trainControl(method="cv",number=10)
  FFP_pls_5<-train(FFP_train_trans_new,FFP_train[,389],method="pls",tuneGrid=expand.grid(ncomp=5),trctrl=ctrl)
  FFP_predict_5<-predict(FFP_pls_5,FFP_test_trans_new)
  FFP_result_5<-data.frame(observed=FFP_test[,389],predicted=FFP_predict_5)


  #partial least square for ncomp=10
  set.seed(100)
  ctrl<-trainControl(method="cv",number=10)
  FFP_pls_10<-train(FFP_train_trans_new,FFP_train[,389],method="pls",tuneGrid=expand.grid(ncomp=10),trctrl=ctrl)
  FFP_predict_10<-predict(FFP_pls_10,FFP_test_trans_new)
  FFP_result_10<-data.frame(observed=FFP_test[,389],predicted=FFP_predict_10)


  #partial least square for ncomp=15
  set.seed(100)
  ctrl<-trainControl(method="cv",number=10)
  FFP_pls_15<-train(FFP_train_trans_new,FFP_train[,389],method="pls",tuneGrid=expand.grid(ncomp=15),trctrl=ctrl)
  FFP_predict_15<-predict(FFP_pls_15,FFP_test_trans_new)
  FFP_result_15<-data.frame(observed=FFP_test[,389],predicted=FFP_predict_15)

  #partial least square for ncomp=20
  set.seed(100)
  ctrl<-trainControl(method="cv",number=10)
  FFP_pls_20<-train(FFP_train_trans_new,FFP_train[,389],method="pls",tuneGrid=expand.grid(ncomp=20),trctrl=ctrl)
  FFP_predict_20<-predict(FFP_pls_20,FFP_test_trans_new)
  FFP_result_20<-data.frame(observed=FFP_test[,389],predicted=FFP_predict_20)

  #partial least square for ncomp=25
  set.seed(100)
  ctrl<-trainControl(method="cv",number=10)
  FFP_pls_25<-train(FFP_train_trans_new,FFP_train[,389],method="pls",tuneGrid=expand.grid(ncomp=25),trctrl=ctrl)
  FFP_predict_25<-predict(FFP_pls_25,FFP_test_trans_new)
  FFP_result_25<-data.frame(observed=FFP_test[,389],predicted=FFP_predict_25)
  
#output 
  FFP_pls_5
  FFP_pls_10
  FFP_pls_15  
  FFP_pls_20
  FFP_pls_25
  
  
  