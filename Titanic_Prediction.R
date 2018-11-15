# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)
library(mice)
library(FNN)
library(formula.tools)

backwardsSelection <- function(data, toPrint=F){
    uselessCols <- c('PassengerId','Name','Ticket','Cabin','lastNames','familySize','Sex')
    print('Now doing backwards selection')
    backCheck = TRUE; count = 0; 
    backNames = setdiff(colnames(data),c('Survived',uselessCols))
    alpha = .05
    
    while(count < ncol(full)){
        count = count + 1
        backFormula = as.formula(paste('Survived', paste(backNames, collapse = ' + '), sep = '~'))
        lm.fit = glm(formula = backFormula, data = full)
        p.values = summary(lm.fit)$coefficients[,4]
        if (max(p.values) < alpha){break}
        else{
            var = names(which(sapply(backNames, function(xx) grep(xx,names(which.max(p.values))))==1))
            backNames = setdiff(backNames,var)
            if(toPrint){print(paste0('Iter ',count,': Removing variable ',var))}
        }
    }
    print(paste0('Backwards selection done with formula: ',as.character(backFormula)))
    return(backFormula)
}

forwardsSelection <- function(data, toPrint=F){
    uselessCols <- c('PassengerId','Name','Ticket','Cabin','lastNames')
    fwdCheck = TRUE; count = 0; 
    fwdNames = setdiff(colnames(full),c(uselessCols,'Survived'))
    fwdModel <- tmpModel <- 'Survived ~ '
    alpha = .05
    
    while(count < ncol(full)){ #(count < ncol(full))
        model <- fwdModel
        count = count + 1
        min.p.value = 1
        
        for (name in fwdNames){
            if (count > 1){model = as.formula(paste(as.character(fwdModel), name, sep = ' + '))
            }else{model = as.formula(paste0(as.character(fwdModel), name))}
            lm.fit = glm(formula = model, data = full)
            if(is.factor(full[,name])){num=nlevels(full[,name])-1}
            else{num=1}
            cc = summary(lm.fit)$coefficients[,4]; nn = length(cc)
            p.value = min(cc[(nn+1-num):nn])
            if (!is.na(p.value)){
                if (p.value < min.p.value){
                    min.p.value = p.value
                    min.name = name
                    tmpModel = model
                }
            }
        }
        if (min.p.value < alpha){
            fwdNames = setdiff(fwdNames,min.name)
            fwdModel = tmpModel
        }else{break}
    }
    print(paste0('Forwards selection done with formula: ',as.character(fwdModel)))
    return(fwdModel)
}

blockValidation <- function(data, model, numBlocks){
    # Input is data set and model to use and numBLocks
    N = nrow(data); n = floor(N/numBlocks)
    data = data[sample(nrow),]
    for (i in 1:numBlocks){
        if (i == numBlocks){
            dataRows = ((i-1)*n + 1):N
        }else{
            dataRows = ((i-1)*n + 1):(i*n)
        }
        train = data[setdiff(1:N,dataRows),]
        test = data[dataRows,]
        # Do the stuff with the model. Let's just not use model for now and assume logistic reg.
    }

}

crossValidation <- function(data, CVformula, split = .7, Nruns=10){
    cat(paste0('\n\nDoing ',split*100,'/',(1-split)*100,' train/test split\n'))
    full2 = data[data$Survived != -1,]
    percentCorrect <- validMSE <- c()
    validMSE <- c()
    percentCorrect.4 <- percentCorrect.6 <- c()
    cutoffs <- seq(0,1,by=.1)
    percentCorrectDF = data.frame(matrix(ncol = length(cutoffs)))
    colnames(percentCorrectDF) = as.character(cutoffs)
    
    for (i in 1:Nruns){
        trainSample = sample(nrow(full2), round(split*nrow(full2)))
        testSample = setdiff(1:(nrow(full2)),trainSample)
        train = full2[trainSample,]
        validate = full2[testSample,]
        lm.fit = glm(formula = CVformula, family = binomial(link = 'logit'), data = train)
        validPredict = predict(lm.fit, validate, type='response')
        validPredictB = as.integer(validPredict>.5)
        #avg_tbl_.5 = avgtable(validPredictB,validate$Survived))
        #percentCorrect.4 = c(percentCorrect.4,100*sum(as.integer(validPredict>.4)==validate$Survived)/nrow(validate))
        #percentCorrect.6 = c(percentCorrect.4,100*sum(as.integer(validPredict>.6)==validate$Survived)/nrow(validate))
        percentCorrect = c(percentCorrect,100*sum(validPredictB==validate$Survived)/nrow(validate))
        newRow <- sapply(cutoffs, function(xx) 100*sum(as.integer(validPredict>xx)==validate$Survived)/nrow(validate))
        percentCorrectDF = rbind(percentCorrectDF,newRow)
        validMSE = c(validMSE,sum((validPredict-validate$Survived)^2)/nrow(validate))
        ### kmeans... to come soone... same with forward and backwards variable selection ... but chill for now.
        #tt = knn.reg(train[,!(names(train) %in% c('Survived'))], validate[,!(names(validate) %in% c('Survived'))],
         #   y = train$Survived, algorithm = 'kd_tree')
    }
   # print(colMeans(percentCorrectDF,na.rm=TRUE))
    print(paste0('Average over ', Nruns, ' runs using ', as.character(CVformula)))
    print(paste0('Logistic regression percentage categorized correct = ', sum(percentCorrect)/Nruns, '%'))
   # print(paste0('Logistic regression MSE (does this make sense?) = ', sum(validMSE)/Nruns))
}
#crossValidation(full, fullFormula)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
plotting = FALSE # determines if the program will run exploration plots.
system("ls ../input")
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test <- read.csv('../input/test.csv', stringsAsFactors = F)
gender_results <- read.csv('../input/gender_submission.csv', stringsAsFactors = F)
#train <- data.table(train)
#test <- data.table(test)
temp = cbind(test,-1) 
names(temp)[which(names(temp)=='-1')] = 'Survived'
full = rbind(train,temp)

# Create new variables
full$familySize = full$SibSp + full$Parch + 1
full$familyGroup = ifelse(full$familySize == 1, 'Single',
              ifelse(full$familySize <= 4, 'Small (2-4)',
              'Large (5+)'))
full$lastNames = sapply(full$Name, function(x) strsplit(x,',')[[1]][1])
full$Title = gsub('(\\..*)|(.*,)','',full$Name); full$Title = gsub(' ','',full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer', 'theCountess')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
full$Embarked[c(62,830)] <- 'C' # This is based on the exploration I did online and the class and fare of the peoples
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
full$AgeT = !is.na(full$Age) # It turns out those whose age we know survived more - this could be due to recording
# information after the fact.
fullDF <- as.data.frame(full)
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','lastNames','familyGroup')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# dealing with age
set.seed(129)
mice_mod <- mice(fullDF[,!names(fullDF) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], 
    method = 'rf')
mice_output <- complete(mice_mod)
fullWithAge <- full[!is.na(full$Age),]
full$Age <- mice_output$Age

# Interesting - create mother and child.
full$Child = ifelse(full$Age < 18, 'Child', 'Adult')
full$Mother = ifelse(full$Age >= 18 & full$Parch > 0 & full$Sex == 'female' & full$Title != 'Miss', 'Mother', 'NotMother')
#full$Father = full$Mother = ifelse(full$Age > 18 & full$Parch > 0 & full$Sex == 'male' & full$Title != 'Miss', 'Mother', 'NotMother')
full$Mother <- factor(full$Mother); full$Child <- factor(full$Child)

uselessCols <- c('PassengerId','Name','Ticket','Cabin','lastNames','familySize','Sex')
fullFormula = paste('Survived',paste(setdiff(colnames(full),c('Survived',uselessCols)), collapse= ' + '), sep = ' ~ ')
crossValidation(full, fullFormula)

backFormula <-  backwardsSelection(full, toPrint=F)
crossValidation(full, backFormula)

fwdFormula <- forwardsSelection(full)
crossValidation(full, fwdFormula)

crossValidation(full, Survived ~ Pclass + Title + familyGroup)
crossValidation(full, Survived ~ Pclass + Sex + familyGroup)
crossValidation(full, Survived ~ Pclass + Child + Mother + familyGroup)

crossValidation(full, fwdFormula, Nruns = 100)
crossValidation(full, fullFormula, Nruns = 100)

# It seems like none of the models do better than the full model. I guess fwdFormula beats it by a little bit,
# but it is negligible. I could try fwd/bkwd, but I don't feel like implementing that with factors. It's a pain.

### It looks like Pclass + Title + familyGroup is the best model, using a cutoff of 0.6.
train = full[full$Survived != -1,]
test = full[full$Survived == -1,-which(colnames(full)=='Survived')]
final.model = Survived ~ Pclass + Title + familyGroup
lm.final = lm(final.model,data=train)
summary(lm.final)
final.results <- predict(lm.final, test)
final.results <- as.integer(final.results>0.6)

head(test)
final.results <- as.data.frame(cbind(test$PassengerId,final.results))
head(final.results)
setnames(final.results,c('PassengerId','Survived'))
write.csv(final.results, 'submission.csv',row.names=F)


if (plotting == TRUE){
    print(mean(train$Survived)) # 38%
    print(mean(train$Pclass)) # 2.31
    print('most plots are made of the training data')
    ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
        geom_bar(stat='count', position='dodge') # clearly higher class people survive more.
    AgeGroup = train$Age
    AgeGroup = ifelse(train$Age < 10, '0-9',
            ifelse(train$Age < 20, '10-19',
            ifelse(train$Age < 30, '20-29',
            ifelse(train$Age < 40, '30-39',
            ifelse(train$Age < 50, '40-49',
            ifelse(train$Age < 60, '50-59',
            '60+'))))))
    
    ggplot(train, aes(x=AgeGroup, fill = factor(Survived))) + geom_bal(stat='count', position='dodge')
    train$AgePerc <- table(AgeGroup,Survived)[,2]/(table(AgeGroup,Survived)[,1]+table(AgeGroup,Survived)[,2])
    ggplot(data=train$AgePerc, aes(x=names(AgePerc), y=AgePerc*100, fill=names(AgePerc))) +
        geom_bar(stat="identity", colour='black') +
        guides(fill=F) + xlab('Age Group') + ylab('Percentage Surviving') +
        ggtitle('Survival by Age Group')
    
    ggplot(train, aes(x=familySize, fill = factor(Survived))) + geom_bar(stat='count', position='dodge')
    ggplot(train, aes(x=familyGroup, fill = factor(Survived))) + geom_bar(stat='count', position='dodge')
    # Do different ticket classes have different family sizes?
    ggplot(train, aes(x=Pclass, fill = factor(familyGroup))) + geom_bar(stat='count', position='dodge')
    # Correlation of men and women survival rates.
    ggplot(train, aes(x=Sex, fill = factor(Survived))) + geom_bar(stat='count', position='dodge')
    ggplot(full, aes(x=Pclass, y = Fare, fill = factor(Embarked))) + geom_boxplot()
}


# This gets real confusing with family definitions and all that. I can return to this later but I should do a more basic tutorial first
# familyMembers = rep('',length(Survived)) # Should have a list of <PassengerId>:<P(arent),(S)ibling/Spouse,C(hild)
# # cycle through each person or each last name > 1
# #for (i in 1:length(train$Survived)){
# nick = i = 0
# while(nick==0){
#     i = i + 1
#     if (train$SibSp[i] > 0){
#         tempTable = fullData[(lastNames == lastNames[i]) & (fullData$SibSp==fullData$SibSp[i])]
        
#     }
#     if (train$Parch[i] > 0){
#         print('NM')
#     }
# }
# 
# Now let's fill in missing data
naNum = sapply(full, function(x) sum(is.na(x)))
blankNum = sapply(full, function(x) sum(x == '', na.rm = T))
missingNum = naNum + blankNum


# Any results you write to the current directory are saved as output.


# Notes:
# - class has a clear effect on survival
# - family size has a quadratic effect on survival (best survival for families of 2-4)
# - higher classes have a higher proportion of small families, so the above should be tested controlling for class.
# - gender has a HUGE effect on survival.
# - There are lots of different titles. Are those correlated with age and/or class and could these be used to fill
#     in missing data?

# Questions:
# - is there a way to figure out if two people are siblings or spouses? Their ages...? Cabin...?
# - Could we infer ethnicity by surname and use that as a predictive variable?
# - How can we run a model when some variables are missing a lot of data while others aren't. Do we impute? Do we run 
#     a slightly different model for observations with missing data?
# - Need to look at correlation of categorical/categorical and categorical/continuous and continuous/continuous covariates
#     using chi-squared, anova, or pearson's test (and need to learn what these are)


# people with the same last names are not necessarily going to be from the same family. 
# Also there are families in the test set. How could I check who is in the same family? How could I use that info?
# I could regress the survival of other family members against one person. I could also look at some relational
# dynamics in families, like if the mother survived what that means for the likelihood of the kid surviving. And
# with the father, etc... But the data set isn't huge, so I can't get too fancy here.