#P1 -IRIS
library(ggplot2)
library(GGally)
library(MASS)

irisTrain<-read.csv("C:/Users/imtiaz.a.khan/Desktop/ml_assign/IRIS/train.csv")
irisTest<-read.csv("C:/Users/imtiaz.a.khan/Desktop/ml_assign/IRIS/test.csv")

#Eliminating row number value
irisTrain<-irisTrain[,2:length(names(irisTrain))]
irisTest<-irisTest[,2:length(names(irisTrain))]

head(irisTrain)
str(irisTrain)

#Scatter Plots
ggpairs(irisTrain,columns=1:4,ggplot2::aes(shape=Species, color=Species))

#creating a meta class
irisTrain$metaClass<-ifelse((irisTrain$Species=='versicolor' | irisTrain$Species=='virginica'),'class4','class3')

#FirstFischer Projection
irisTrain.lda <- lda(metaClass ~ ., data=irisTrain[,-which(names(irisTrain) %in% 'Species')])
irisTrain.lda.values <- predict(irisTrain.lda)
plotFischerDiscrimant1<-data.frame(index=seq(1,length(irisTrain.lda.values$x[,1]),1),ldaProjection=irisTrain.lda.values$x[,1],class=irisTrain.lda.values$class)
ggplot(plotFischerDiscrimant1,aes(x=index,y=ldaProjection,colour=class))+geom_point()


#Second FirstFischer Projection
irisTrainSubset<-subset(irisTrain,(irisTrain$Species=='versicolor' | irisTrain$Species=='virginica'))
irisTrainSubset$Species<-factor(irisTrainSubset$Species)
irisTrainSubset.lda <- lda(Species ~ ., data=irisTrainSubset[,-which(names(irisTrainSubset) %in% 'metaClass')])
irisTrainSubset.lda.values <- predict(irisTrainSubset.lda)
plotFischerDiscrimant2<-data.frame(index=seq(1,length(irisTrainSubset.lda.values$x[,1]),1),ldaProjection2=irisTrainSubset.lda.values$x[,1],class=irisTrainSubset.lda.values$class)
ggplot(plotFischerDiscrimant2,aes(x=index,y=ldaProjection2,colour=class))+geom_point()

#Projecting entire test data on the first fischer projection
testFirstFischerPredictions = predict(object = irisTrain.lda, # predictions
               newdata = irisTest)
plotFischerDiscrimant1Test<-data.frame(index=seq(1,length(testFirstFischerPredictions$x[,1]),1),ldaProjection1=testFirstFischerPredictions$x[,1],class=testFirstFischerPredictions$class)
ggplot(plotFischerDiscrimant1Test,aes(x=index,y=ldaProjection1,colour=class))+geom_point()

#Projecting entire data on the second fischer projection
testSecondFischerPredictions = predict(object = irisTrainSubset.lda, # predictions
                                      newdata = irisTest)
testSecondFischerPredictions<-data.frame(index=seq(1,length(testSecondFischerPredictions$x[,1]),1),ldaProjection2=testSecondFischerPredictions$x[,1],class=testSecondFischerPredictions$class)

ggplot(testSecondFischerPredictions,aes(x=index,y=ldaProjection2,colour=class))+geom_point()

#P2-Mushroom information gain

rm(list=ls())
cat('\014')

mushTrain<-read.csv('C:/Users/imtiaz.a.khan/Desktop/ml_assign/MUSHROOM/train.csv')
mushTest<-read.csv('C:/Users/imtiaz.a.khan/Desktop/ml_assign/MUSHROOM/test.csv')

#Eliminating row number value
mushTrain<-mushTrain[,2:length(names(mushTrain))]
mushTest<-mushTest[,2:length(names(mushTest))]

#information gain due to each feature
library(RWeka)
IG.RWeka <- InfoGainAttributeEval(V1 ~ ., data=mushTrain,)
sort(IG.RWeka)

gini_process <-function(classes,splitvar = NULL){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  Node_Gini<-NULL
  for(m in 1:length(levels(splitvar))){
    Node_Gini <- c(Node_Gini,1-sum(crossprob[,m]**2))
    #No_Node_Gini <- 1-sum(crossprob[,2]**2)
  }
  return(sum(base_prob * Node_Gini))
}
info_process <-function(classes,splitvar = NULL){
  Node_Info<-NULL
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(-sum(base_prob*log(base_prob,2)))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  for(m in 1:length(levels(splitvar))){
    Node_Info<-c(Node_Info,-sum(crossprob[crossprob[,m]>0,m]*log(crossprob[crossprob[,m]>0,m],2)))
  }
  return(sum(base_prob * Node_Info))
}
columnNames<-names(mushTrain)
ResultDataset<-data.frame(Feature=columnNames[-c(1,17)],entropy=rep(NA,length(columnNames[-c(1,17)])),GiniIndex=rep(NA,length(columnNames[-c(1,17)])),Accuracy=rep(NA,length(columnNames[-c(1,17)])))
columnNames=columnNames[-c(1,17)]
for(j in 1:length(columnNames)){

ResultDataset$GiniIndex[j]=gini_process(mushTrain$V1,eval(parse(text=paste0('mushTrain','$',columnNames[j]))))
ResultDataset$entropy[j]=info_process(mushTrain$V1,eval(parse(text=paste0('mushTrain','$',columnNames[j]))))
}
for(j in 1:length(columnNames)){
  
  for(i in 1:length(levels(eval(parse(text=paste0('mushTrain','$',columnNames[j])))))){
    p=table(eval(parse(text=paste0('mushTrain','$',columnNames[j]))),mushTrain$V1)[,1]>table(eval(parse(text=paste0('mushTrain','$',columnNames[j]))),mushTrain$V1)[,2]
    for(l in 1:length(p)){
    if(p[l]){
      mushTrain$predicted<-'NA'
      mushTrain[eval(parse(text=paste0('mushTrain','$',columnNames[j])))==names(p)[l],]$predicted='e'
    }
      else{
        mushTrain[eval(parse(text=paste0('mushTrain','$',columnNames[j])))==names(p)[l],]$predicted='p'
        
      }
      
    }
    
    ResultDataset$Accuracy[j]<-Accuracy(mushTrain$predicted,mushTrain$V1)
    mushTrain$predicted<-NULL
  }
}

ggplot(data=ResultDataset,aes(x=Accuracy,y=entropy))+geom_point()+geom_text(aes(label=Feature),hjust=0, vjust=0)
write.csv(ResultDataset,"C:/Users/imtiaz/Desktop/ml_assign/MUSHROOM/resultdataset.csv")


#P3 : MUSHROOM NB/DT
rm(list=setdiff(ls(), c('mushTrain','mushTest')))
library(e1071)
TrainAccuracy<-NULL
TestAccuracy<-NULL
for(lambda in 1:50){
NBmodel <- naiveBayes(V1 ~ ., data = mushTrain,laplace = lambda)
predTrain <- predict(NBmodel, mushTrain[,-1])
predTest <- predict(NBmodel, mushTest)
TrainAccuracy<-c(TrainAccuracy,sum(predTrain==mushTrain$V1)*100/nrow(mushTrain))
print(TrainAccuracy)
TestAccuracy<-c(TestAccuracy,sum(predTest==mushTest$V1)*100/nrow(mushTest))
print(TestAccuracy)

}
 
NaiveBayesData<-data.frame(Lambda=1:50,TrainAccuracy=TrainAccuracy,TestAccuracy=TestAccuracy)  
ggplot(NaiveBayesData, aes(Lambda)) + 
  geom_line(aes(y = TrainAccuracy, colour = "TrainAccuracy")) + 
  geom_line(aes(y = TestAccuracy, colour = "TestAccuracy"))

#Decision Tree
#rm(list=setdiff(ls(), c('mushTrain','mushTest')))
library(rpart)
DTrainAccuracy<-NULL
DTestAccuracy<-NULL
for(sizeThreshold in c(4,64,4)){
  DTmodel <- rpart(V1 ~ ., data = mushTrain,minbucket=sizeThreshold)
  predDTrain <- predict(DTmodel, mushTrain[,-1],type='class')
  predDTest <- predict(DTmodel, mushTest,type='class')
  DTrainAccuracy<-c(DTrainAccuracy,sum(predDTrain==mushTrain$V1)*100/nrow(mushTrain))
  DTestAccuracy<-c(DTestAccuracy,sum(predDTest==mushTest$V1)*100/nrow(mushTest))
}

DecisionData<-data.frame(sizeThreshold=seq(4,64,4),TrainAccuracy=DTrainAccuracy,TestAccuracy=DTestAccuracy)  
ggplot(DecisionData, aes(sizeThreshold)) + 
  geom_line(aes(y = TrainAccuracy, colour = "TrainAccuracy")) + 
  geom_line(aes(y = TestAccuracy, colour = "TestAccuracy"))


CompareAccuracies<-data.frame(NaivesTrainAccuracy=range(TrainAccuracy),NaivesTestAccuracy=range(TestAccuracy),DecisionTreeTrainAccuracy=range(DTrainAccuracy),DecisionTreeTestAccuracy=range(DTrainAccuracy))
write.csv(CompareAccuracies,'C:/Users/imtiaz/Desktop/ml_assign/CompareAccuracies.csv')

#P4 - MNIST BAYESIAN

resultsPath <- 'C:/Users/imtiaz.a.khan/Desktop/71620031-20170730T090634Z-001/71620031/MNIST/'

train<-NULL
for(k in 1:10){
  temp<-read.csv(paste0(resultsPath,'train',k-1,'.csv'),stringsAsFactors = FALSE)
  temp$class<-k-1
  train<-rbind(train,temp)
  }
train<-na.omit(train)
train<-train[,-1]

test<-NULL
for(t in 1:10){
  temptest<-read.csv(paste0(resultsPath,'test',t-1,'.csv'),stringsAsFactors = FALSE)
  temptest$class<-t-1
  test<-rbind(test,temptest)
}
test<-na.omit(test)
test<-test[,-1]

rm(list = ls()[!ls() %in% c("train","test")])

combinedData<-rbind(train,test)
library(caret)
nzv_cols <- nearZeroVar(combinedData)
if(length(nzv_cols) > 0) combinedData <- combinedData[, -nzv_cols]

prin_comp <- prcomp(sapply(combinedData[,-785],as.numeric))
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

data.pca <- data.frame(class = combinedData$class, prin_comp$x)
#we are interested in first 9 PCAs
D1Train <- data.pca[1:nrow(train),1:10]
D1Test<-data.pca[nrow(train):(nrow(train)+nrow(test)),1:10]
#rm(list = ls()[!ls() %in% c("D1Train","D1Test","combinedData")])

#Fischer Projection
library(MASS)
r <- lda(formula = class ~ ., data = combinedData) 
plda = predict(object = r, # predictions
               newdata = combinedData)
data.lda <- data.frame(class = combinedData$class, plda$x)
D2Train <- data.lda[1:nrow(train),1:10]
D2Test<-data.lda[nrow(train):(nrow(train)+nrow(test)),1:10]

#Modelling 
rm(list = ls()[!ls() %in% c("D1Train","D1Test","D2Train","D2Test")])

library(caret)
library(e1071)

p4Results<-data.frame(modelName=c('PCABayesianFullCov','PCABayesianDiagonalCov','LDABayesianFullCov','LDABayesianDiagonalCov')
,ModelTestAccuracy=rep(NA,4))
#Bayesian full covariance matrix model

BayesianFCMD1Model<-qda(class ~ ., data = D1Train)
qda.class <- predict(BayesianFCMD1Model, D1Test[,-1])$class
testAccuracy<-caret::confusionMatrix(qda.class, D1Test$class)
testAccuracy$overall[1]
p4Results$ModelTestAccuracy[1]<-testAccuracy$overall[1]

#Bayesian diagonal covariance matrix model

library(sparsediscrim)
BayesianDCMD1Model<-dqda(class ~ ., data = D1Train)
dqda.class <- predict(BayesianDCMD1Model, D1Test[,-1])$class
testAccuracy<-caret::confusionMatrix(dqda.class, D1Test$class)
testAccuracy$overall[1]
p4Results$ModelTestAccuracy[2]<-testAccuracy$overall[1]

#Bayesian Full covariance matrix model

BayesianFCMD2Model<-qda(class ~ ., data = D2Train)
qda.class <- predict(BayesianFCMD2Model, D2Test[,-1])$class
testAccuracy<-caret::confusionMatrix(qda.class, D2Test$class)
testAccuracy$overall[1]
p4Results$ModelTestAccuracy[3]<-testAccuracy$overall[1]

#Bayesian diagonal covariance matrix model
BayesianDCMD2Model<-dqda(class ~ ., data = D2Train)
dqda.class <- predict(BayesianDCMD2Model, D2Test[,-1])$class
testAccuracy<-caret::confusionMatrix(dqda.class, D2Test$class)
testAccuracy$overall[1]
p4Results$ModelTestAccuracy[4]<-testAccuracy$overall[1]

write.csv(p4Results,'QuestionP4TestAccuracies.csv')

#P5 : MNIST - kNN / Parzen window
rm(list = ls()[!ls() %in% c("D1Train","D1Test","D2Train","D2Test")])

library(class)
Question5Results<-data.frame(Kvalue=seq(1,17,2),trainaccuracy_D1=rep(NA,9),trainaccuracy_D2=rep(NA,9),testaccuracy_D1=rep(NA,9),testaccuracy_D2=rep(NA,9))
for(k in seq(1,17,2)){
  train_predD1 <- knn(train = D1Train[,-1], test = D1Train[,-1],cl = D1Train[,1], k=k)
  trainAccuracyD1<-100 * sum( D1Train[,1] == train_predD1)/nrow(D1Train)
  test_predD1 <- knn(train = D1Train[,-1], test = D1Test[,-1],cl = D1Train[,1], k=k)
  testAccuracyD1<-100 * sum( D1Test[,1] == test_predD1)/nrow(D1Test)
  train_predD2 <- knn(train = D2Train[,-1], test = D2Train[,-1],cl = D2Train[,1], k=k)
  trainAccuracyD2<-100 * sum( D2Train[,1] == train_predD2)/nrow(D2Train)
  test_predD2 <- knn(train = D2Train[,-1], test = D2Test[,-1],cl = D2Train[,1], k=k)
  testAccuracyD2<-100 * sum( D2Test[,1] == test_predD2)/nrow(D2Test)
  Question5Results[Question5Results$Kvalue==k,]$trainaccuracy_D1<-trainAccuracyD1
  Question5Results[Question5Results$Kvalue==k,]$testaccuracy_D1<-testAccuracyD1
  Question5Results[Question5Results$Kvalue==k,]$trainaccuracy_D2<-trainAccuracyD2
  Question5Results[Question5Results$Kvalue==k,]$testaccuracy_D2<-testAccuracyD2
  
}
write.csv(Question5Results,'KNNQ5.csv')
ggplot(Question5Results, aes(Kvalue)) + 
  geom_line(aes(y = trainaccuracy_D1, colour = "trainaccuracy_D1")) + 
  geom_line(aes(y = testaccuracy_D1, colour = "testaccuracy_D1"))

ggplot(Question5Results, aes(Kvalue)) + 
  geom_line(aes(y = trainaccuracy_D2, colour = "trainaccuracy_D2")) + 
  geom_line(aes(y = testaccuracy_D2, colour = "testaccuracy_D2"))

#Parzen Window
library(KernelKnn)
D1Train$kknnLevel<-D1Train$class+1
D2Train$kknnLevel<-D2Train$class+1
trainAccuracypar<-NULL
testAccuracypar<-NULL
trainAccuracyparD2<-NULL
testAccuracyparD2<-NULL
for(sigma in seq(0.1,3,0.1)){
fit.kknn <- KernelKnn(D1Train[,-c(1,11)],D1Test[,-c(1)],y=D1Train[,11],weights_function='gaussian',method='euclidean',Levels=unique(D1Train[,11]),h=sigma,regression=FALSE)
fit.knn1<-t(apply(fit.kknn,1,function(x) c(which.max(x)-1)))
fit.kknntrain <- KernelKnn(D1Train[,-c(1,11)],y=D1Train[,11],weights_function='gaussian',method='euclidean',Levels=unique(D1Train[,11]),h=sigma,regression=FALSE)
fit.knntrain1<-t(apply(fit.kknntrain,1,function(x) c(which.max(x)-1)))
testAccuracypar[sigma*10]<-100*sum(D1Test$class==fit.knn1)/nrow(D1Test)
trainAccuracypar[sigma*10]<-100*sum(D1Train$class==fit.knntrain1)/nrow(D1Train)

fit.kknnD2 <- KernelKnn(D2Train[,-c(1,11)],D2Test[,-c(1)],y=D2Train[,11],weights_function='gaussian',method='euclidean',Levels=unique(D2Train[,11]),h=sigma,regression=FALSE)
fit.knnD21<-t(apply(fit.kknnD2,1,function(x) c(which.max(x)-1)))
fit.kknntrainD2 <- KernelKnn(D2Train[,-c(1,11)],y=D2Train[,11],weights_function='gaussian',method='euclidean',Levels=unique(D2Train[,11]),h=sigma,regression=FALSE)
fit.knnDtrainD21<-t(apply(fit.kknntrainD2,1,function(x) c(which.max(x)-1)))
testAccuracyparD2[sigma*10]<-100*sum(D2Test$class==fit.knnD21)/nrow(D2Test)
trainAccuracyparD2[sigma*10]<-100*sum(D2Train$class==fit.knnDtrainD21)/nrow(D2Train)
print(paste0('iteration : ',sigma*10))
}
parzenWindowResults<-data.frame(sigma=seq(0.1,3,0.1),trainAccuracyD1=trainAccuracypar,testAccuracyD1=testAccuracypar,trainAccuracyD2=trainAccuracyparD2,testAccuracyD2=testAccuracyparD2)

write.csv(parzenWindowResults,"parzenWindowResults.csv")

ggplot(parzenWindowResults, aes(sigma)) + 
  geom_line(aes(y = trainAccuracyD1, colour = "trainAccuracyD1")) + 
  geom_line(aes(y = testAccuracyD1, colour = "testAccuracyD1"))

ggplot(parzenWindowResults, aes(sigma)) + 
  geom_line(aes(y = trainAccuracyD2, colour = "trainAccuracyD2")) + 
  geom_line(aes(y = testAccuracyD2, colour = "testAccuracyD2"))


#P6 : News group Text Classifier
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(text2vec)
library(data.table)

text.files <- list.files(path="C:/Users/imtiaz.a.khan/Desktop/71620031-20170730T090634Z-001/71620031/20_newsgroups/", recursive=T, full.names=T) 
readDatFile <- function(f) { dat.fl <- readLines(f) } 
text.data <- sapply(text.files, readDatFile) 
cls <- names(text.data)
class<-basename(dirname(cls))
docname<-paste(class,basename(cls),sep='-')
textContent<-NULL
for(i in 1:length(text.data)){
  textContent[i]<-paste(text.data[i])
}


NewsGroup<-data.frame(documentName=docname,text=textContent,class=class)

rm(list = ls()[!ls() %in% c("NewsGroup")])
set.seed(2017L)
NewsGroup$text<-as.character(NewsGroup$text)

train.index <- createDataPartition(NewsGroup$class, p = .7, list = FALSE)
train <- NewsGroup[ train.index,]
test  <- NewsGroup[-train.index,]
trainAccuracy<-NULL
testAccuracy<-NULL
#loop for both 5000 terms and 1000 terms.
for(i in 1:2){
  prep_fun = tolower
  tok_fun = word_tokenizer
  
  it_train = itoken(train$text, 
                    preprocessor = prep_fun, 
                    tokenizer = tok_fun, 
                    ids = train$documentName, 
                    progressbar = FALSE)
  vocab = create_vocabulary(it_train,ngram = c(1L, 2L))
  
  vectorizer = vocab_vectorizer(vocab)
  t1 = Sys.time()
  dtm_train = create_dtm(it_train, vectorizer)
  print(difftime(Sys.time(), t1, units = 'sec'))
  
  
  dim(dtm_train)
  
  pruned_vocab = prune_vocabulary(vocab, 
                                  term_count_min = 10, 
                                  doc_proportion_max = 0.5,
                                  doc_proportion_min = 0.001,max_number_of_terms=5000*i)
  vectorizer = vocab_vectorizer(pruned_vocab)
  # create dtm_train with new pruned vocabulary vectorizer
  t1 = Sys.time()
  dtm_train  = create_dtm(it_train, vectorizer)
  print(difftime(Sys.time(), t1, units = 'sec'))
  
  
  
  it_test = test$text %>% 
    prep_fun %>% 
    tok_fun %>% 
    itoken(ids = test$documentName, 
           # turn off progressbar because it won't look nice in rmd
           progressbar = FALSE)
  
  dtm_test = create_dtm(it_test, vectorizer)
  
  
  system.time( classifier <- naiveBayes(as.matrix(dtm_train), y=train$class, laplace = 30) )
  
  
  #Train accuracy
  system.time( predTrain <- predict(classifier, newdata=as.matrix(dtm_train)))
  trainAccuracy[i]<-100*sum(predTrain==train$class)/nrow(train)
  
  #testAccuracy
  system.time( pred <- predict(classifier, newdata=as.matrix(dtm_test)))
  
  testAccuracy[i]=100*sum(pred==test$class)/nrow(test)
}

