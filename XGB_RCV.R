require(xgboost)
require(methods)

train = read.csv('data/train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('data/test.csv',header=TRUE,stringsAsFactors = F)
train = train[,-1]
test = test[,-1]
y = train[,ncol(train)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-ncol(train)],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

# Gradient descent method
thetaold <-c(6,1,0)# here, we only change three parameters: max_depth, min_child_weight, and gamma
alpha <- c(200,50,0.5) #learning rate for each parameter
# Set initial values for parameters
param <- list("objective" = "multi:softprob",
              "eta"=0.3,"max_depth"=thetaold[1],"min_child_weight"=thetaold[2],"gamma"=thetaold[3],"subsample"=0.7,
              "eval_metric" = "mlogloss",
              "num_class" = 9,
              "nthread" = 8) #Bestset1: "eta"=0.1,"max_depth"=16,"min_child_weight"=8,"gamma"=1,"subsample"=0.7,

# Run initial Cross Valication
cv.nround = 50
bst.cv = xgb.cv(param=param, data = x[trind,], label = y, 
                nfold = 4, nrounds=cv.nround)
Errold=min(as.numeric(bst.cv[,test.mlogloss.mean]))
repo<-c(thetaold,Errold)
#initial kick
theta <-c(7,2,0.1)
for (i in seq(1,20,1)){
    param <- list("objective" = "multi:softprob",
                "eta"=0.3,"max_depth"=theta[1],"min_child_weight"=theta[2],"gamma"=theta[3],"subsample"=0.7,
                "eval_metric" = "mlogloss",
                "num_class" = 9,
                "nthread" = 8)
    bst.cv = xgb.cv(param=param, data = x[trind,], label = y, nfold = 4, nrounds=cv.nround)
    Err = min(as.numeric(bst.cv[,test.mlogloss.mean]))
    th=theta
    #update parameters
    for (j in 1:3){
        theta[j]=theta[j]-alpha[j]*(Err-Errold)/(theta[j]-thetaold[j])
    }
    thetaold=th
    Errold=Err
    repo<-rbind(repo,c(thetaold,Errold))
}
print(repo)
     


## Train the model
#nround = 50
#bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)

## Make prediction
#pred = predict(bst,x[teind,])
#pred = matrix(pred,9,length(pred)/9)
#pred = t(pred)

## Output submission
#pred = format(pred, digits=2,scientific=F) # shrink the size of submission
#pred = data.frame(1:nrow(pred),pred)
#names(pred) = c('id', paste0('Class_',1:9))
#write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)
