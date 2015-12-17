# Construct models 
library(randomForest)
library(gbm)
library(glmnet)
library(Matrix)
library(e1071)

## PCA: 1
## RF: 2
## GBM: 3
## LASSO: 4
## LASSO+PCA: 5
## SVM: 6
## SVM+PCA: 7
## GBM+PCA: 8
########################## Sliding Window ############################
predSlideWindow = function(n=1, numcomp = 20){
      dataset = stock_price_final        
      est_spy = vector(length=(nrow(dataset)-100))
      set.seed(1)
      
      if(n == 1){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):d,]
                  myPCA = prcomp(dat[,-c(1,ncol(dat))], center = T, scale. = T, retx = T)
                  pca2 = as.data.frame(cbind(myPCA$x[,1:numcomp],dat$response))
                  colnames(pca2)[numcomp+1] = "stock_return"
                  train_set = pca2[1:100,]
                  lm.fit = lm(stock_return~.,data = train_set)
                  est_spy[d-100] = predict(lm.fit,pca2[101,])
            }
      }else if(n == 2){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):(d-1),]
                  modelRF=randomForest(response~.-date,
                                       data=dat,
                                       mtry=15,
                                       importance=TRUE)
                  est_spy[d-100] = predict(modelRF,dataset[d,])
            }
      }else if(n == 3){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):(d-1),]
                  modelGBM=gbm(response~.-date,data=dat,
                              distribution= "gaussian",
                              n.trees=1000,
                              interaction.depth=5,
                              shrinkage = 0.05,
                              bag.fraction = 0.5)
                  est_spy[d-100] = predict(modelGBM, dataset[d,], n.trees = 1000)
            }
      }else if(n == 4){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):(d-1),]
                  fit.lasso=cv.glmnet(Matrix(as.matrix(dat[,-c(1,ncol(dat))]),sparse=T),
                                      Matrix(as.matrix(dat$response),sparse=T))
                  est_spy[d-100] = predict.cv.glmnet(fit.lasso,
                                                     Matrix(as.matrix(dataset[d,-c(1,ncol(dat))]),
                                                            sparse=T))
            }
      }else if( n == 5){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):d,]  # Use 101 row to train PCA
                  myPCA = prcomp(dat[,-c(1,ncol(dat))], center = T, scale. = T, retx = T)
                  pca2 = as.data.frame(cbind(myPCA$x[,1:numcomp],dat$response))
                  colnames(pca2)[numcomp+1] = "stock_return"
                  train_set = pca2[1:100,]
                  
                  fit.lasso=cv.glmnet(Matrix(as.matrix(train_set[,-ncol(train_set)]),sparse=T),
                                      Matrix(as.matrix(train_set$stock_return),sparse=T))
                  est_spy[d-100] = predict.cv.glmnet(fit.lasso,
                                                     Matrix(as.matrix(pca2[101,-ncol(train_set)])))
            }
      }else if(n == 6){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):(d-1),]
                  set.seed(1)
                  model.svm=svm(dat[,-c(1,ncol(dat))],
                                dat$response,
                                kernel = "radial",
                                cost = 0.1,
                                scale = T)
                  est_spy[d-100] = predict(model.svm,dataset[d,-c(1,ncol(dat))])
            }         
      }else if(n == 7){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):d,]  # Use 101 row to train PCA
                  myPCA = prcomp(dat[,-c(1,ncol(dat))], center = T, scale. = T, retx = T)
                  pca2 = as.data.frame(cbind(myPCA$x[,1:numcomp],dat$response))
                  colnames(pca2)[numcomp+1] = "stock_return"
                  train_set = pca2[1:100,]
                  
                  model.svm=svm(train_set[,-ncol(train_set)],
                                train_set$stock_return,
                                kernel = "radial",
                                cost = 1,
                                scale = T)
                  est_spy[d-100] = predict(model.svm,pca2[101,-ncol(train_set)])
            }
      }else if(n == 8){
            for(d in 101:nrow(dataset)){
                  dat = dataset[(d-100):d,]
                  myPCA = prcomp(dat[,-c(1,ncol(dat))], center = T, scale. = T, retx = T)
                  pca2 = as.data.frame(cbind(myPCA$x[,1:numcomp],dat$response))
                  colnames(pca2)[numcomp+1] = "stock_return"
                  train_set = pca2[1:100,]
                  
                  modelGBM=gbm(stock_return~.,data=train_set,
                               distribution= "gaussian",
                               n.trees=1000,
                               interaction.depth=5,
                               shrinkage = 0.05,
                               bag.fraction = 0.5)
                  est_spy[d-100] = predict(modelGBM, pca2[101,], n.trees = 1000)
            }
      }
      return(est_spy)
}
