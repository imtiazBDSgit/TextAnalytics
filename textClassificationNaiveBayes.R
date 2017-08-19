rm(list = ls()[!ls() %in% c("NewsGroup")])

library(text2vec)
library(data.table)
set.seed(2017L)
NewsGroup$text<-as.character(NewsGroup$text)
library(caret)
train.index <- createDataPartition(NewsGroup$class, p = .7, list = FALSE)
train <- NewsGroup[ train.index,]
test  <- NewsGroup[-train.index,]
trainAccuracy<-NULL
testAccuracy<-NULL
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

