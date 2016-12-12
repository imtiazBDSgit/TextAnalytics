
#Removing stale objects from the environment
rm(list = ls())

#Loading Required Libraries for data analysis and Modelling

library(ggplot2)
library(e1071)
library(gridExtra)
library(Boruta)
library(Matrix)
library(caTools)
library(magrittr)
library(data.table)
library(xgboost)
library(ROCR)

#Reading Data from the git hub repository.

data=read.csv("https://raw.githubusercontent.com/imtiazBDSgit/TextAnalytics/master/ModelingData.txt",sep="\t",header=TRUE)

#Structure of the data
dim(data)
str(data)
head(data)

#Summarizing the data , objective here is to remove NA values.
summary(data)

#Since NA's are only two in number in the whole data, it has little impact 
#so we can safely remove them.

data= na.omit(data)
#Categorical Features as mentioned in the code book.
categoricalFeatures=c('CHK_ACCT','HISTORY','SAV_ACCT','EMPLOYMENT','PRESENT_RESIDENT','JOB','NEW_CAR',	'USED_CAR',	'FURNITURE',	'RADIO.TV',	'EDUCATION',	'RETRAINING',	'MALE_DIV',	'MALE_SINGLE',	'MALE_MAR_or_WID',	'CO.APPLICANT',	'GUARANTOR',	'REAL_ESTATE',	'PROP_UNKN_NONE',	'OTHER_INSTALL',	'RENT',	'OWN_RES',	'TELEPHONE',	'FOREIGN',	'RESPONSE')

length(categoricalFeatures)
#Making those values as factor values

data[,categoricalFeatures]=lapply(data[,categoricalFeatures],as.factor)

#removing observation column as it is mere index value.

data=data[,-1]
data_numeric=data[ , !(names(data) %in% categoricalFeatures)]

#Some Reusable functions to do exploratory visual analysis.

#Plots histograms
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

#Plots density plots for numeric variables
plotDen <- function(data_in, i){
  data=data.frame(x=data_in[[i]])
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

#Barplots of categorical features.

doPlots(data_categorical, fun = plotHist, ii = 1:25, ncol = 5)

#Insights
#People with chk_acct status are very less compared other categories
#People were fairly compliant in paying back the bills , we can infer that
# from the credit History category 2
#New cars are fairly less
#Used cars are fairly more.
#People have less furniture at their homes
#People having radio/tv are also pretty less
#Education levels is also low
#purpose of credit is also not retraining in most cases
#For most customers Account balances are low.
#People with 1-4 years experienced employees dominate
#male_divorced or male_married_widowed are less
#people dont have co-applicants and guarantor mostly
#Present residence fairly distributed
#own houses are more
#fairly a quarter of them own real estate
#people dont have telephones in great deal
#nature of the job is mostly skilled employees
#Most people dont rent.
#workers are local , foreign workers are less
#Credit rating is good dominated.

#Numerical variable density plots
doPlots(data_numeric, fun = plotDen, ii = 1:6, ncol = 2)

#Insights

#Amount is right skewed distributed
#Age is right skewed distributed
#Rest all features dont have a particular pattern , mostly decreasing exception is install_rate

#Feature Engineering Using Boruta package to reduce unimportant features and to retain most important
#features
set.seed(100)
boruta.train <- Boruta(RESPONSE~., data = data, doTrace = 2)
print(boruta.train)

final.boruta <- TentativeRoughFix(boruta.train)
importantFeatures=names(final.boruta$finalDecision[final.boruta$finalDecision=='Confirmed'])

#plotting feature importance

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
 axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
 
 
#Model Building using Xgboost as the algorithm is seen to perform the best because of
#Ensembling techniques.
 
 #Filtering out important features which we got from boruta package.
 
 TotalData=data[ , (names(data) %in% importantFeatures)]
 str(TotalData)
 TotalData$RESPONSE=data$RESPONSE

 #Data preparation for Xgboost Model, Since the model accepts only numeric data.
 
 set.seed(101) 
 
 output_vector = train[,"RESPONSE"]==1
 sparse_matrix <- sparse.model.matrix(RESPONSE~.-1, data = TotalData)
 colnames(sparse_matrix)
 dim(sparse_matrix)
 
 sample = sample.split(TotalData, SplitRatio = .75)
 trainLabel =  subset(TotalData, sample == TRUE)$RESPONSE
 testLabel  =  subset(TotalData, sample == FALSE)$RESPONSE
 trainMatrix =sparse_matrix[sample == TRUE,]
 testMatrix  =sparse_matrix[sample == FALSE,]
 trainLabel = trainLabel==1
 testLabel=testLabel==1
 
 #categoricalFeatures=c('CHK_ACCT','HISTORY','SAV_ACCT')
 #binaryFeatures=c('NEW_CAR','USED_CAR','GUARANTOR','REAL_ESTATE','PROP_UNKN_NONE','OTHER_INSTALL','OWN_RES','RESPONSE')
 
 
 
 bst <- xgboost(data = trainMatrix, label=trainLabel,max_depth = 8,
                eta = 1, nthread = 2, nrounds = 20,objective = "binary:logistic")
 
 
 #Model Evaluation on test set.
 
 y_pred <- predict(bst, testMatrix)
 
 table(testLabel,y_pred>0.5)
 
 pred <- prediction(y_pred, testLabel)
 perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
 plot(perf, col=rainbow(10))
 
 
 #Auc Calculations
 auc = as.numeric(performance(pred, "auc")@y.values)
 auc
 
 