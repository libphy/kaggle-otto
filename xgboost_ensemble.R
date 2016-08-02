#require(xgboost)
#require(methods)

#train = read.csv('data/train.csv',header=TRUE,stringsAsFactors = F)
#test = read.csv('data/test.csv',header=TRUE,stringsAsFactors = F)
#train = train[,-1]
#test = test[,-1]

#y = train[,ncol(train)]
#y = gsub('Class_','',y)
#y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

#x = rbind(train[,-ncol(train)],test)
#x = as.matrix(x)
#x = matrix(as.numeric(x),nrow(x),ncol(x))
#trind = 1:length(y)
#teind = (nrow(train)+1):nrow(x)

avgP <-matrix(0,byrow=TRUE,nrow=length(teind),ncol=9)
# Set necessary parameter
bestparam <- matrix(c(16, 10, 1, 243, 15, 8, 0.7, 188, 14, 7, 0.61, 213, 13, 6, 0.52, 194, 12, 5, 0.42, 207), byrow=TRUE, ncol=4)

for (i in 1:nrow(bestparam)){
     param <- list("objective" = "multi:softprob",
              "eta"=0.1,"max_depth"=bestparam[i,1],"min_child_weight"=bestparam[i,2],"gamma"=bestparam[i,3],"subsample"=0.7,
              "eval_metric" = "mlogloss",
              "num_class" = 9,
              "nthread" = 8)
     # Train the model
     nround = bestparam[i,4]
     bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)
     # Make prediction
     pred = predict(bst,x[teind,])
     pred = matrix(pred,9,length(pred)/9)
     pred = t(pred)
     avgP=avgP+pred
}
avgP<-avgP/nrow(bestparam)
# Output submission
avgP = format(avgP, digits=2,scientific=F) # shrink the size of submission
avgP = data.frame(1:nrow(avgP),avgP)
names(avgP) = c('id', paste0('Class_',1:9))
write.csv(avgP,file='submission.csv', quote=FALSE,row.names=FALSE)
