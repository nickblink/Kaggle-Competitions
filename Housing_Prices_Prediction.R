# This code is unfinished :(, It contains the initial exploration and feature engineering steps

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)
library(caret)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
train <- fread('../input/train.csv',stringsAsFactors = TRUE)
test <- fread('../input/test.csv',stringsAsFactors = TRUE)
test$SalePrice = -1
full <- rbind(train,test) # the test data is missing salesprice, but we can deal with that later.

dim(train)
dim(test)

mean(is.na(full))
# ~6% values missing.

hist(train$SalePrice)
# looks somewhat normal but right skewed. Mean = 181k

#cols.missing = apply(full,2, function(xx) mean(is.na(xx)))
#rint(paste0('removing column ', names(cols.missing)[which(cols.missing>.5)]))
#ull = full[,-which(cols.missing>0.5),with=F]
# remove the four columns with > 40% missing values.

###################### Feature engineering #################################
# Remaining questions:
# Should I just keep Year Remodeled and remove the original year?
# Removing Fence quality, pool quality, alley, and miscfeature might be a problem.
# I made BsmtCond and BsmtQual ordered, but this could be an issue putting no basement below a poor basement
#temp = ifelse(MSSubClass == 20 | MSsubCLass == 30, )
# Need to do stuff with MSSubClass. Maybe combine with HouseStyle and BldgType. These are repetitive.                    
# Change exteriors and Conditions to single binary vars.

full$BsmtExposure <- as.character(full$BsmtExposure)
full$BsmtExposure <- ifelse(full$BsmtExposure=='No','None',full$BsmtExposure)
full$BsmtExposure <- as.factor(full$BsmtExposure)
full$Pool <- ifelse(full$PoolArea>0,'Yes','No')
full <- full[,-c('Id','PoolQC','PoolArea')]

NA_to_No <- function(feature){
  if(!any(is.na(feature))){
    print('No NAs')
    return(feature)
  }else{
    feature <- as.character(feature)
    feature <- ifelse(is.na(feature),'No',feature)
    feature <- as.factor(feature)
  }
  return(feature)
}
backup = full
to_replace <- c('BsmtCond','BsmtQual','Alley','FireplaceQu','GarageType','Fence','MiscFeature',
                'BsmtExposure','BsmtFinType1','BsmtFinType2','GarageFinish','GarageQual','GarageCond')
for (col in to_replace){
  full[[col]] <- NA_to_No(full[[col]])
}
backup2 = full

full$Alley <- ordered(full$Alley, c('No','Pave','Grvl'))
full$LotShape <- ordered(full$LotShape, c("IR3","IR2","IR1",'Reg'))
full$Utilities <- ordered(full$Utilities, c('ELO','NoSeWa','NoSewr','AllPub'))
full$LandSlope <- ordered(full$LandSlope, c('Gtl','Mod','Sev'))
full$ExterQual <- ordered(full$ExterQual, c('Po','Fa','TA','Gd','Ex'))
full$ExterCond <- ordered(full$ExterCond, c('Po','Fa','TA','Gd','Ex'))
full$BsmtQual <- ordered(full$BsmtQual, c('No','Po','Fa','TA','Gd','Ex'))                     
full$BsmtCond <- ordered(full$BsmtCond, c('No','Po','Fa','TA','Gd','Ex'))   
full$BsmtExposure <- ordered(full$BsmtExposure, c('No','None','Mn','Av','Gd'))
full$BsmtFinType1 <- ordered(full$BsmtFinType1, c('No','Unf','LwQ','Rec','BLQ','ALQ','GLQ')) 
full$BsmtFinType2 <- ordered(full$BsmtFinType2, c('No','Unf','LwQ','Rec','BLQ','ALQ','GLQ')) 
# What to do About BsmtFinType2? This ain't right what I'm doing
full$HeatingQC <- ordered(full$HeatingQC, c('Po','Fa','TA','Gd','Ex'))
full$KitchenQual <- ordered(full$KitchenQual, c('Po','Fa','TA','Gd','Ex'))
full$Functional <- ordered(full$Functional, c('Sal','Sev','Maj2','Maj1','Mod','Min1','Min2','Typ'))
full$FireplaceQu <- ordered(full$FireplaceQu, c('No','Po','Fa','TA','Gd','Ex'))                     
full$GarageFinish <- ordered(full$GarageFinish, c('No','Unf','RFn','Fin'))           
full$GarageQual <- ordered(full$GarageQual, c('No','Po','Fa','TA','Gd','Ex'))
full$GarageCond <- ordered(full$GarageCond, c('No','Po','Fa','TA','Gd','Ex'))
full$PavedDrive <- ordered(full$PavedDrive, c('N','P','Y'))
full$Fence <- ordered(full$Fence, c('No','MnWw','GdWo','MnPrv','GdPrv'))

dv.maker <- dummyVars(~Exterior1st+Condition1,full)
full <- cbind(full,data.frame(predict(dv.maker,full)))

# Some sketchy moves coming up.
## Questionable move here that I'm about to do...setting any lot frontage na's to the minimum lot frontage
full$LotFrontage <- ifelse(is.na(full$LotFrontage),21,full$LotFrontage)
full$Remodeled <- ifelse(full$YearRemodAdd > full$YearBuilt, 'Yes', 'No')
full$GarageYrBlt <- ifelse(is.na(full$GarageYrBlt),median(full$GarageYrBlt,na.rm=T),full$GarageYrBlt) 
full$MasVnrType <- as.factor(ifelse(is.na(as.character(full$MasVnrType)),'None',
                                    as.character(full$MasVnrType)))
full$MasVnrArea <- ifelse(is.na(full$MasVnrArea),0,full$MasVnrArea)
mean(is.na(full))
#=.0001 = .01% missing values
mean(apply(full,1, function(xx) !any(is.na(xx))))
# = .0045 = .45 % houses with missing values. Let's just delete them.
full <- full[apply(full,1, function(xx) !any(is.na(xx))),]

ord.factor.cols <- colnames(full)[which(sapply(full, function(xx) {
  (any(class(xx)=='factor') & (any(class(xx) == 'ordered')))}))]
full.2 <- cbind(full[,setdiff(colnames(full),ord.factor.cols),with=F],
                as.data.frame(lapply(full[,ord.factor.cols,with=F], as.integer)))

factor.cols <- colnames(full.2)[which(sapply(full.2, function(xx) {
  (any(class(xx)=='factor') & !(any(class(xx) == 'ordered')))}))]
factor.formula <- as.formula(paste0('~',paste(factor.cols,collapse='+')))
full.2 <- data.frame(predict(dummyVars(factor.formula,full.2),full.2))

y.train = full$SalePrice[full$SalePrice!=-1]
train <- full[which(full$SalePrice!=-1),setdiff(colnames(full),'SalePrice'),with=F]
test <- full[which(full$SalePrice==-1),setdiff(colnames(full),'SalePrice'),with=F]
X = train
train.2 <- full.2[which(full$SalePrice!=-1),]
X.2 = train.2
test <- full.2[which(full$SalePrice==-1),setdiff(colnames(full.2),'SalePrice')]

combos = findLinearCombos(train.2)
train.3 <- train.2[,-combos$remove]
nzv.rm <- nearZeroVar(train.3, freqCut = 97/3)
train.3 <- train.3[,-nzv.rm]

combos = findLinearCombos(test)
test <- test[,-combos$remove]
nzv.rm <- nearZeroVar(test, freqCut = 97/3)
test <- test[,-nzv.rm]

X.3 = train.3;

#train.control = trainControl(method='repeatedcv',number=10,repeats=3)       
train.control = trainControl(method='cv',number=10,repeats=3)    
models <- c('glm','lm','pls','pcr','ridge','lasso','knn','rf') #'glmnet','enet' #,'bartMachine') #bartMachine is super slow.
n_models <- length(models)
R.list <- list()
for (data in list(X.3)){#,X.2,X.1)){
  print(paste0('Using data from ',deparse(substitute(train))))
  RMSE = rep(0, n_models)
  for (m in 1:n_models){
    print(paste0('model ', models[m]))
    fit =train(data, y.train, method=models[m], trControl=train.control)
    RMSE[m] <- fit$results[,2]
    names(RMSE)[m] <- models[m]
    R.list[[m]] <- fit$results
  } 
  print(paste0('Using data from ',deparse(substitute(train))))
  print(RMSE)
}
#        glm          lm         pls         pcr       ridge       lasso 
#52057.25819 51510.85552 61013.89385 65552.72402 52588.71840 67456.64793 
#        knn      glmnet        enet          rf bartMachine 
#53200.63633    79.13715     0.05000 54929.61170     2.00000            
## and using 10 folds repeat 3
#      glm       lm      pls      pcr    ridge    lasso      knn       rf 
#   50737.04 51759.37 60728.36 65088.86 51915.21 67001.78 52144.78 54153.28
# now do leave one out glm, ridge, knn
# glmnet and bartMachine don't have the same kind of output, but they seem to work pretty well.
library(glmnet)
grid=10^seq(5,-2,length=50)
cv.out = cv.glmnet(as.matrix(X.3),y.train,alpha=0,lambda=grid)
plot(cv.out)

final.model = train(X.3, y.train, method='ridge', trControl = trainControl(method='LOOCV') )
final.results = as.data.frame(predict(final.model,test))

final.results <- as.data.frame(cbind(test$PassengerId,final.results))
head(final.results)
setnames(final.results,c('PassengerId','Survived'))
write.csv(final.results, 'submission.csv',row.names=F)

#validate.range = 1:100
#fit = train(X, y, method='glm', trControl=train.control)
#pred.glm <- predict(fit, X.2[validate.range,])
#library(hydroGOF)
#rmse(y[validate.range],pred.glm)                   

################### Check dist of MSSubClass, BldgType, and HoustStyle with SalesPrice ######