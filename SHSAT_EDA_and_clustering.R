
setwd('../input')
schools = read.csv(dir()[1], stringsAsFactors = FALSE)
shsat = read.csv(dir()[2], stringsAsFactors = FALSE)


dim(schools) # 1272 161
dim(shsat) # 140 7

shsat$School.name = tolower(shsat$School.name)
schools$School.Name = tolower(schools$School.Name)
length(unique(shsat$School.name))
# 30 - this is because there is data for schools over multiple years
length(unique(schools$School.Name))
# 1270 - looks like they're mostly all unique
length(intersect(shsat$School.name,schools$School.Name))
# yikes - only 21 schools in both data sets.
sum(shsat$School.name %in% schools$School.Name)
# 110. Meaning there are 110 rows in the shsat with school data, meaning generally 5 years worth of data per school.

# Could I look at how schools scores change over time? Like between consecutive years Or just the average each year.
table(shsat$Year.of.SHST)
# 2013 2014 2015 2016 
#  33   35   35   37
# So pretty even
registered = c()
taken = c()
for (year in 2013:2016){
    D = shsat[shsat$Year.of.SHST==year,]
    registered = c(registered,sum(D[,6])/sum(D[,5]))
    taken = c(taken,sum(D[,7])/sum(D[,5]))
}
registered # 0.2122869 0.2584027 0.1886852 0.1711310
taken # 0.1009971 0.1069997 0.1077750 0.1044643
# Interesting - the proportion of sts. taking the test stays constant, 
# but the proportion of students registering went down a bit.

# need to change the type of a few columns and remove some redundant ones
cols.to.factor = c('Community.School.','Student.Achievement.Rating','Grade.Low','Grade.High')
for (col in cols.to.factor){
    schools[,col] = as.factor(schools[,col])
}
cols.to.numeric = c('Economic.Need.Index','Average.ELA.Proficiency','Average.Math.Proficiency')
for (col in cols.to.numeric){
    schools[,col] = as.numeric(schools[,col])
}
# 135 NAs introduced here.
cols.percent = c('Percent.ELL','Percent.Asian','Percent.Black','Percent.Hispanic','Percent.Black...Hispanic',
'Percent.White','Student.Attendance.Rate','Percent.of.Students.Chronically.Absent','Rigorous.Instruction..',
'Collaborative.Teachers..','Supportive.Environment..','Effective.School.Leadership..','Strong.Family.Community.Ties..',
'Trust..')
for (col in cols.percent){
    schools[,col] = sub('%','',schools[,col])
    schools[,col] = as.integer(schools[,col])
}
# 200 NAs added.


schools[,'School.Income.Estimate'] = sub('[$]','',schools[,'School.Income.Estimate'])
schools[,'School.Income.Estimate'] = sub(' ','',schools[,'School.Income.Estimate'])
schools[,'School.Income.Estimate'] = sub(',','',schools[,'School.Income.Estimate'])
schools[,'School.Income.Estimate'] = gsub('\\..*','',schools[,'School.Income.Estimate'])
schools[,'School.Income.Estimate'] = as.integer(schools[,'School.Income.Estimate'])
# 396 NAs introduced
# will fill these in with economic need index

# school income estimate has 396 NAs while school economic need index has 25,
# so it's probably just better to use school economic need index and rm school
# income estimate. I can fill in the schools that are missing need index
# with income estimate predictions if they
# have that (19/25 do) and otherwise...delete the remaining 5? Or just avg?
ind = !is.na(schools$School.Income.Estimate) & !is.na(schools$Economic.Need.Index)
#plot(schools$Economic.Need.Index[ind],schools$School.Income.Estimate[ind])
lm.fit = lm(Economic.Need.Index~School.Income.Estimate,data=schools[ind,])
# R^2 = .79, which is pretty darn good. So I'll use school income to 
# estimate some of the missing economic need indexes
ind.na = which(is.na(schools$Economic.Need.Index))
schools$Economic.Need.Index[ind.na] = predict(lm.fit,schools[ind.na,])
sum(is.na(schools$Economic.Need.Index)) #6- screw it delete them
schools = schools[!is.na(schools$Economic.Need.Index),]

# Remove unneccessary rating columns and school income estimate column
cols.to.remove = c('Rigorous.Instruction.Rating','Collaborative.Teachers.Rating','Supportive.Environment.Rating',
                   'Effective.School.Leadership.Rating','Strong.Family.Community.Ties.Rating','Trust.Rating','School.Income.Estimate')
schools = schools[,setdiff(names(schools),cols.to.remove)]

sum(is.na(shsat)) # 0 - nice
sum(is.na(schools)) # 250 now :(
na.ind = which(apply(schools,1,function(xx) any(is.na(xx)))) # 49 rows with nas.
col.nas = sapply(schools, function(xx) sum(is.na(xx)))
col.nas[col.nas>0]
# Looks like there are 19 schools missing all the ratings, which is fine. Delete them
# or just fill in with averages or do something fancier like mice? Honestly,
# these are probably not that important so I can probably get away with just 
# adding in the averages (lower down)

# Another intermediate issue to look at 
sum(schools$Grade.8.Math...All.Students.Tested==0)
# this means 743 schools did not do any 8th grade math testing, so if I were
# to use this info along with schools$Grade.8.Math.4s..., which is the number
# of students getting 4s on the exam, I would run into the issue of what to
# do with the 743 schools that have no test info. Yikes. I could fill that 
# in with mice, but that is an extremely high number of people without info,
# so I am going to exclude these columns for now. A more advanced analysis
# would find a way to include them, though.

# Another possible analysis on the side would be to use these extra columns
# to calculate the proportion of students in each race scoring 4s in the exams.
# then that could be compared with the estimated proportion that end up taking
# the shsat. If there is a mismatch, e.g. one group is performing well on the
# state exams but not taking the shsat test, then that is a useful target group.
# ^ Sounds like an interesting analysis!



cols.to.average = c('Student.Attendance.Rate','Percent.of.Students.Chronically.Absent',
                    'Rigorous.Instruction..','Collaborative.Teachers..','Supportive.Environment..',
                    'Effective.School.Leadership..','Strong.Family.Community.Ties..',
                    'Trust..','Average.ELA.Proficiency','Average.Math.Proficiency')
for(col in cols.to.average){
  ind = which(is.na(schools[,col]))
  schools[ind,col]=mean(schools[-ind,col])
}

sum(is.na(schools)) #0 - yay, finally!


schools = schools[,1:34]
# created school type variable - just simplifying into middle or elementary school)
table(schools$Grade.Low)
table(schools$Grade.High)
# Seems like it's mostly all grouped into normal schools
schools$school.type = ifelse(schools$Grade.High %in% c('0k','02','03','04','05','06'), 'elementary', 'middle')
# issues here - 40% of middle schools are elementary/middle schools
names(schools)


classes = sapply(schools, class)
cor.s = cor(schools[,classes %in% c('integer','numeric')])
library(corrplot)
#corrplot(cor.s) # This probs won't show up on kaggle but it shows up on my
# desktop R.

# interesting correlations:
# latitude - percent.hispanic = 0.55
# economic needs index with % hispanic, black, and chronically absent
# school income with %white and ELA and math proficiency
# math proficiency with % white and asian
# Obvi, all the different school/teacher ratings are correlated with each other.
# If not using PCA in the end, it might be useful to just average the ratings scores.
# percent black is negatively correlated with ELL.

schools$school.type = as.integer(ordered(schools$school.type,c('elementary','middle')))
schools$Community.School. = as.integer(schools$Community.School.)
schools$Student.Achievement.Rating = as.integer(ordered(schools$Student.Achievement.Rating,
c('Not Meeting Target','Approaching Target','N/A','Meeting Target','Exceeding Target')))
# Yes this last one is sketchy, esp. with the N/A in the middle, but it's all I can do for now

pca.rm = c('Adjusted.Grade','New.','Other.Location.Code.in.LCGMS','School.Name','SED.Code','Grade.High','Grade.Low',
'Location.Code','Latitude','Longitude','Address..Full.','City','Zip','Grades','Percent.Black...Hispanic')
schools.2=schools[,setdiff(colnames(schools),pca.rm)]
set.seed(10)
pr.out = prcomp(schools[,setdiff(colnames(schools),pca.rm)],scale=TRUE)
cumsum(pr.out$sdev^2)/21
# Could we just cluster by 21 variables? I could just use the top 10 to get ~90% of the variance, which is pretty good.
# Or I could try both

# first, clustering on all 21 variables.
# kmeans is the most basic clustering algorithm, clustering by trying to minimize
# the distance between points, so it needs numerical data. There are issues with
# kmeans, especially with higher numbers of variables, so take this with a grain of salt.
for(k in (2:10)){ # ranging from 2 to 10 clusters, what do we get?
    schools.clusters = kmeans(scale(schools.2),k,nstart=30)
    print(k)
    print(schools.clusters$withins)
    print(sum(schools.clusters$withins))
    print(table(schools.clusters$cluster))
}
# Now, let's do this with the PCA 10 variables
schools.3 = pr.out$x[,1:10]
for(k in (2:6)){ # ranging from 2 to 6 clusters, what do we get?
    schools.clusters = kmeans(schools.3,k,nstart=30)
    print(k)
    print(schools.clusters$withins)
    print(sum(schools.clusters$withins))
    print(table(schools.clusters$cluster))
}
# in-cluster variance is much smaller, but that's obviously expected because I'm only using 10 vars,
# though these 10 vars do take into account 90% variation. The sum of abs(schools.3) = 2/3 sum(abs(schools.2))
# I'll stick with the PCA clusters for now. How many clusters though? 3? 4? 5? We could analyze each.
# Say we'll stick with 4 for now.
schools.clusters = kmeans(schools.3,4,nstart=30)
# Should analyze different variables across clusters, but first I want to try some regression with these
clusters = cbind(schools$School.Name,schools.clusters$cluster)
colnames(clusters) = c('School.name','Cluster')
shsat.2 = merge(shsat, clusters, by='School.name')
table(shsat.2$Cluster) # Issues if any clusters have too few
# Any results you write to the current directory are saved as output.
shsat.2$percent.registering=shsat.2$Number.of.students.who.registered.for.the.SHSAT/shsat.2$Enrollment.on.10.31
shsat.2$percent.taking=shsat.2$Number.of.students.who.took.the.SHSAT/shsat.2$Enrollment.on.10.31

# now doing some regression. I have low expectations for these outcomes. Maybe it would have been better to just do
# straight up regression with all the variables. I'll do that next.
lm.fit = lm(percent.taking~Cluster,data=shsat.2)
summary(lm.fit)

lm.fit = lm(percent.registering~Cluster,data=shsat.2)
summary(lm.fit)
# Interesting. No correlation between cluster and percent registering, but a definite correlation between
# cluster and percent taking. There might be something to explore there (i.e. do some clusters have a
# correlation between the percentage of students that register that end up taking the exam). Unfortunately,
# the correlation coefficient is pretty weak, so don't over-emphasize this.
shsat.3 = shsat.2[shsat.2$Number.of.students.who.registered.for.the.SHSAT>0,] # because you can't divide by zero
shsat.3$exam.follow.through = shsat.3$Number.of.students.who.took.the.SHSAT/shsat.3$Number.of.students.who.registered.for.the.SHSAT
lm.fit = lm(exam.follow.through~Cluster,data=shsat.3)
summary(lm.fit) # Meh...maybe a correlation between clusters 2,3,4 and follow through, but unclear here. Too little data.
# Back to the first lm.fit
lm.fit = lm(percent.taking~Cluster,data=shsat.2)
summary(lm.fit)
# (,) is a 95%
# Being in cluster 2 leads to an increase of ~(12%,56%) of test-taking over cluster 1
# Being in cluster 3 leads to an increase of ~(4%,20%) of test-taking over cluster 1
# Being in cluster 4 leads to an increase of (-1%,7%) of test-taking over cluster 1
# So what's going on with cluster 1 and cluster 2?
# As a side note, this regression has an R-squared of 0.13, so basically it's jack-shit and the clusters themselves explain little of
# the percentage of test takers.

# let's try regressing on more variables!
shsat.4 = merge(shsat.2,schools,by.x='School.name',by.y='School.Name')
shsat.4 = shsat.4[,setdiff(colnames(shsat.4),pca.rm)]
dim(shsat.4) # 110 31
# hm but we still can't regress on everythihng - that's too many variables, especially since
# some of them are categorical variables and will be treated as extra variables when regressing.
# One option is to do lasso to remove many variables, but is it too many variables to start with
# to even do lasso? Wait - why not start with PCA regression and see what we get?

# remember that schools.3 is the first 10 PC's
schools.3 = as.data.frame(schools.3)
schools.3$School.name = schools$School.Name
shsat.5 = merge(shsat.2,schools.3,by='School.name')

# start with three PC's. I'm still worried about putting too many variables in because of sample size.
# I could lasso here, though lasso-ing on PC's seems counter-intuitive and probably won't yield much.
# But who knows - maybe it will. I'll try it later.
lm.fit = lm(percent.taking~PC1 + PC2 + PC3,data=shsat.5) 
summary(lm.fit)
# yikes - no significant variables and R^2 = .04. So basically the first three PC's tell us nothing...

lm.fit = lm(percent.taking~PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,data=shsat.5) 
summary(lm.fit)
# same here - woo this is rough. In retrospect, regressing on PC's is kind of dumb because PC's blend
# all variables together, so the actually predictive variables will be spread across all the other
# uninformative variables. There's a reason I haven't seen regressing on PC's before...let's now try 
# this on real vars

# back to shsat.4! As a side note, I don't know the statistical problems with regressing on the same 
# inputs (i.e. same schools) and multiple outputs. I would imagine there are some issues here...

lm.fit = lm(percent.taking~., data=shsat.4[,setdiff(colnames(shsat.4),c('School.name','DBN','Grade.level',
'Number.of.students.who.registered.for.the.SHSAT','Number.of.students.who.took.the.SHSAT','percent.registering'))])
summary(lm.fit)
# as expected, way too many variables. I even got some singularites in the results. Not sure how that happened. 
# Did those 4 variables have no variance?




### Lasso time! Lasso is a regression that penalizes the beta coefficients
# as well as the number of variables, so the hope is that it removes 
# the uninformative variables. Our data set might still be too small to do
# this here, but I'm going to give it a shot.
library(glmnet)
y = shsat.4$percent.taking
x = shsat.4[,setdiff(colnames(shsat.4),c('percent.registering','DBN',
                                         'percent.taking','School.name',
                                         'Number.of.students.who.registered.for.the.SHSAT',
                                         'Number.of.students.who.took.the.SHSAT'))]
x = sapply(x, as.numeric)
cv.fit = cv.glmnet(x,y,alpha=1)
# cv means cross-validated, which is generally best to do
# alpha = 1 means lasso. alpha = 0 means ridge regression, which I'm not doing here.
lambda.min = cv.fit$lambda.min
lasso.fit = glmnet(x,y,alpha=1,lambda=10*lambda.min)
# I'll come back to this. I'm tired
MSE = mean((y-predict(lasso.fit,x))^2)
R2 = 1 - MSE/var(y)
# 0.689 <- that's much better than I expected. So about 70% of the variation in the
# percentage of students taking the test is given by the variables
lasso.fit$beta
# interesting points
#   - student attendance rate and % students chronically absent are both important.
#   - more of the ratings things came up than expected.
#   - school type mattered ... duh - elementary schools are not taking the exam. (actually not true - just checked)
#   - Average MATH and ELA proficiency are not important.
#   - The only important race variable is % white. Surprised % asian didn't make it.
# ** It's important to remember that lasso knocks out correlated variables, so Average.Math.Proficiency might
# actually be important but %white and attendance rate might be good proxies for this.

# Another idea - should I just run a regression on the variables that are able to be influenced in the real world?
# I.e. don't include racial percentages because we're not going to implement an intervention to make schools more white.
B = matrix(lasso.fit$beta)
B = cbind(colnames(x),B)
B = B[B[,2]!=0,]
col.means = c()
col.sd = c()
for(col in B[,1]){
    col.means = c(col.means,mean(shsat.4[,col]))
    col.sd = c(col.sd,sd(shsat.4[,col]))
}
B = cbind(B,col.means)
B = cbind(B,col.sd)
# this is just my own thing...never seen someone do this before. But I do want to
# see the importance of each variable by multiplying sd times beta. Since the higher
# sd vars will have a bigger impact on the outcome. There are more verified methods
# of looking at variable importance that would be more worthwhile, but I'm too lazy right
# now.
var.importance = abs(col.sd*as.numeric(B[,2]))
var.importance = var.importance/max(var.importance)
B = cbind(B,var.importance)
B = B[order(var.importance,decreasing=TRUE),]
colnames(B) = c('variable','beta estimate','variable mean','variable sd','var importance...kind of')

# Grade level is so important because there are only 8th and 9th graders in the data set, and 9th graders
# rarely take the test (1.5% do) while more 8th graders do (19%), so grade level is an obviously good predictor

# Sadly, according to my sketchy variable importance metric, the percentage of white students in the school
# is the second most important variable for improving % sts taking the test. And of course, this variable is inversely
# correlated with the percentage of black, hispanic and asian students, though asian students do have the 
# highest recorded percentage of students taking the test, which is odd with these findings.

# Student attendance rate is also a significant negative predictor (%chronically absent not as much)
# I am surprised by collaborative teachers makes the cut, not because collaborative teachers wouldn't
# lead to more test taking, but because I am surprised that metric actually captures some truth about
# the collaborativeness about the teachers.

# Percent ELL is a strong negative predictor, which isn't surprising.

# How did economic level not make the cut? Is this because percent white, percent ell and student attendance rate are 
# proxies for this?

# Maybe I should have done this earlier - let's do some single regression on each variable, scaled, to assess the importance of each.
D = shsat.4[,colnames(shsat.4)[-c(1,2)]]
for(j in colnames(D)){
    D[,j] = as.numeric(D[,j])
}
D = as.data.frame(scale(D))
D = D[,setdiff(colnames(D),'District')] # no variance for district. I guess only district 5 reported their SHSAT scores! Interesting to keep
# in mind for future analyses...that's why other kernel's have looked at things compared to district 5.

results = as.data.frame(matrix(0,nrow = (dim(D)[2]-1),ncol = 5))
colnames(results) = c('var','B.lower','B','B.upper','p.value')
i = 0
for(col in setdiff(colnames(D),'percent.taking')){
    i = i +1
    model.form = as.formula(paste0('percent.taking ~ ',col))
    print(model.form)
    lm.fit = lm(model.form,data=D)
    B = summary(lm.fit)$coefficients[2,1]
    sd = summary(lm.fit)$coefficients[2,2]
    p.value = summary(lm.fit)$coefficients[2,4]
    results[i,]=c(col,B-1.96*sd,B,B+1.96*sd,p.value)
    #lm.2 = lm(percent.taking ~ get(col),data=D)
}
sum(results$p.value<.05)
results[results$p.value<.05,]
# remember that D is scaled, so we can compare the beta levels across variables to be approximate measures of importance.
# It looks like percent asian is just as good a predictor as percent white, and percent black is just as good a negative
# predictor as percent white. Interesting percent hispanic didn't make the cut. And interestingly, there was close to 
# a positive association with percent hispanic. I guess district 5's hispanic population does well, though we can't be
# too statistically confident about that.
# Strong community ties and average ela proficiency had similar positive importance to the percent.white and percent.asian.
# Also, enrollment/class size and attendance rates are negative predictors. Why are all these beta values basically in the 
# same positive or negative range? Is that due to scaling?
# Anyway, it's break time for now.




















