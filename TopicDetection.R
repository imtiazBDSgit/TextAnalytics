rm(list=ls())
library("rvest")
library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)
library(igraph)
library(textir)

######################################

counts = c(0,10,20,30,40,50)
reviews = NULL
for (j in counts){
  url1 = paste0("http://www.imdb.com/title/tt1187043/reviews?filter=love;filter=love;start=",j)
  url2 = paste0("http://www.imdb.com/title/tt1187043/reviews?filter=hate;filter=hate;start=",j)
  
  page1 = read_html(url1)
  page2 = read_html(url2)
  reviews1 = html_text(html_nodes(page1,'#tn15content p'))
  reviews2 = html_text(html_nodes(page2,'#tn15content p'))
  
  reviews.positive = setdiff(reviews1, c("*** This review may contain spoilers ***","Add another review"))
  reviews.negative = setdiff(reviews2, c("*** This review may contain spoilers ***","Add another review"))
  
  reviews = c(reviews,reviews.positive,reviews.negative)
  
}

reviews = gsub("\n",' ',reviews)
class(reviews)
writeLines(reviews,'Three Idiots.txt')

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

data = data.frame(id = 1:length(reviews),  # creating doc IDs if name is not given
                  text = reviews, 
                  stringsAsFactors = F)

stpw1 = readLines('https://raw.githubusercontent.com/imtiazBDSgit/TextAnalytics/master/stopwords.txt')      # read-in stopwords.txt
stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
comn  = unique(c(stpw1, stpw2,c("movie","movies","watch","story")))         # Union of two list
stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation

x  = text.clean(data$text)                # applying func defined above to pre-process text corpus
x  =  removeWords(x,stopwords)            # removing stopwords created above
x  =  stripWhitespace(x)

#This function initialises the word_tokenizer which splits by spaces.
tok_fun = word_tokenizer  # using word & not space tokenizers

#This function iterates over input objects
#This function creates iterators over input objects to vocabularies, 
#corpora, or DTM and TCM matrices. This iterator is usually used in following functions : 
#create_vocabulary, create_corpus, create_dtm, vectorizers, create_tcm
it_0 = itoken( x,
               #preprocessor = text.clean,
               tokenizer = tok_fun,
               ids = data$id,
               progressbar = T)
# func collects unique terms & corresponding statistics
#Creates a vocabulary of unique terms
vocab = create_vocabulary(it_0,   
                          ngram = c(1L, 1L) #,
                          #stopwords = stopwords
)
# length(vocab); str(vocab)     # view what vocab obj is like
#This function filters the input vocabulary and throws out very frequent and 
#very infrequent terms.
pruned_vocab = prune_vocabulary(vocab,  # filters input vocab & throws out v frequent & v infrequent terms
                                term_count_min = 10)
# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

# length(pruned_vocab);  str(pruned_vocab)

vectorizer = vocab_vectorizer(pruned_vocab) #  creates a text vectorizer func used in constructing a dtm/tcm/corpus

dtm_m  = create_dtm(it_0, vectorizer) # high-level function for creating a document-term matrix

#colnames(dtm_0)
#story of the code
# For creating a dtm i would need itoken and vectorizer. For creating vectorizer 
#we need a vocab object which gets from create_vocabulary or prune_vocabulary.
#For creating a itoken
# Sort bi-gram with decreasing order of freq

#rollup (aggregrate) sparse arrays along arbitrary dimensions.


# Code bi-grams as unigram in clean text corpus

# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
dtm = dtm[a0,]                  # drop empty docs

print(difftime(Sys.time(), t1, units = 'sec'))

# view a sample of the DTM, sorted from most to least frequent tokens 
dtm = dtm[,order(apply(dtm, 2, sum), decreasing = T)]     # sorting dtm's columns in decreasing order of column sums
inspect(dtm[1:5, 1:5])     # inspect() func used to view parts of a DTM object           




                          # ngram = c(2L, 2L),
                          #stopwords = stopwords

# length(vocab); str(vocab)     # view what vocab obj is like


#--------------------------------------------------------#
## Step 2a:     # Build word cloud                       #
#--------------------------------------------------------#

#   1- Using Term frequency(tf)             

tst = round(ncol(dtm)/100)  # divide DTM's cols into 100 manageble parts
a = rep(tst,99)
b = cumsum(a);rm(a)
b = c(0,b,ncol(dtm))

ss.col = c(NULL)
for (i in 1:(length(b)-1)) {
  tempdtm = dtm[,(b[i]+1):(b[i+1])]
  s = colSums(as.matrix(tempdtm))
  ss.col = c(ss.col,s)
  print(i)
}

tsum = ss.col
tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
head(tsum)
tail(tsum)

windows()  # New plot window
wordcloud(names(tsum), tsum,     # words, their freqs 
          scale = c(4, 0.5),     # range of word sizes
          1,                     # min.freq of words to consider
          max.words = 200,       # max #words
          colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
title(sub = "Term Frequency - Wordcloud")     # title for the wordcloud display

# plot barchart for top tokens
test = as.data.frame(round(tsum[1:15],0))

windows()  # New plot window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "Blue") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#tfidf

# step 2b - Using Term frequency inverse document frequency (tfidf)             
# -------------------------------------------------------------- #


dtm.tfidf = tfidf(dtm, normalize=F)

tst = round(ncol(dtm.tfidf)/100)
a = rep(tst, 99)
b = cumsum(a);rm(a)
b = c(0,b,ncol(dtm.tfidf))

ss.col = c(NULL)
for (i in 1:(length(b)-1)) {
  tempdtm = dtm.tfidf[,(b[i]+1):(b[i+1])]
  s = colSums(as.matrix(tempdtm))
  ss.col = c(ss.col,s)
  print(i)
}

tsum = ss.col

tsum = tsum[order(tsum, decreasing = T)]       #terms in decreasing order of freq
head(tsum)
tail(tsum)

windows()  # New plot window
wordcloud(names(tsum), tsum, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency Inverse Document Frequency - Wordcloud")

as.matrix(tsum[1:20])     #  to see the top few tokens & their IDF scores
(dtm.tfidf)[1:10, 1:10]   # view first 10x10 cells in the DTM under TF IDF.

# plot barchart for top tokens
test = as.data.frame(round(tsum[1:15],0))
windows()  # New plot window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Term co-occurence matrix and co-occurence graphs

vectorizer = vocab_vectorizer(pruned_vocab, 
                              grow_dtm = FALSE, 
                              skip_grams_window = 5L)

tcm = create_tcm(it_0, vectorizer) # func to build a TCM

tcm.mat = as.matrix(tcm)         # use tcm.mat[1:5, 1:5] to view
adj.mat = tcm.mat + t(tcm.mat)   # since adjacency matrices are symmetric

z = order(colSums(adj.mat), decreasing = T)
adj.mat = adj.mat[z,z]

# Plot Simple Term Co-occurance graph
adj = adj.mat[1:30,1:30]

library("igraph")
cog = graph.adjacency(adj, mode = 'undirected')
cog =  simplify(cog)  

cog = delete.vertices(cog, V(cog)[ degree(cog) == 0 ])

windows()
plot(cog)


#Distilled Cog

distill.cog = function(mat1, # input TCM ADJ MAT
                       title, # title for the graph
                       s,    # no. of central nodes
                       k1){  # max no. of connections  
  library(igraph)
  a = colSums(mat1) # collect colsums into a vector obj a
  b = order(-a)     # nice syntax for ordering vector in decr order  
  
  mat2 = mat1[b, b]     # order both rows and columns along vector b
  
  diag(mat2) =  0
  
  ## +++ go row by row and find top k adjacencies +++ ##
  
  wc = NULL
  
  for (i1 in 1:s){ 
    thresh1 = mat2[i1,][order(-mat2[i1, ])[k1]]
    mat2[i1, mat2[i1,] < thresh1] = 0   # neat. didn't need 2 use () in the subset here.
    mat2[i1, mat2[i1,] > 0 ] = 1
    word = names(mat2[i1, mat2[i1,] > 0])
    mat2[(i1+1):nrow(mat2), match(word,colnames(mat2))] = 0
    wc = c(wc,word)
  } # i1 loop ends
  
  
  mat3 = mat2[match(wc, colnames(mat2)), match(wc, colnames(mat2))]
  ord = colnames(mat2)[which(!is.na(match(colnames(mat2), colnames(mat3))))]  # removed any NAs from the list
  mat4 = mat3[match(ord, colnames(mat3)), match(ord, colnames(mat3))]
  graph <- graph.adjacency(mat4, mode = "undirected", weighted=T)    # Create Network object
  graph = simplify(graph) 
  V(graph)$color[1:s] = "green"
  V(graph)$color[(s+1):length(V(graph))] = "pink"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) # delete singletons?
  
  plot(graph, 
       layout = layout.kamada.kawai, 
       main = title)
  
} # func ends

windows()
distill.cog(tcm.mat, 'Distilled COG',  10,  5)

## adj.mat and distilled cog for tfidf DTMs ##

adj.mat = t(dtm.tfidf) %*% dtm.tfidf
diag(adj.mat) = 0
a0 = order(apply(adj.mat, 2, sum), decreasing = T)
adj.mat = as.matrix(adj.mat[a0[1:50], a0[1:50]])

windows()
distill.cog(adj.mat, 'Distilled COG',  10,  10)

# Polarity and sentiments


library(qdap)

x1 = x[a0]    # remove empty docs from corpus

t1 = Sys.time()   # set timer

pol = polarity(x1)         # Calculate the polarity from qdap dictionary
wc = pol$all[,2]                  # Word Count in each doc
val = pol$all[,3]                 # average polarity score
p  = pol$all[,4]                  # Positive words info
n  = pol$all[,5]                  # Negative Words info  

Sys.time() - t1  # how much time did the above take?

head(pol$all)
head(pol$group)

positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list

print(positive_words)       # Print all the positive words found in the corpus
print(negative_words)       # Print all neg words

#--------------------------------------------------------#
#   Create Postive Words wordcloud                       #
#--------------------------------------------------------#

pos.tdm = dtm[,which(colnames(dtm) %in% positive_words)]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows() # opens new image window
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")

# plot barchart for top tokens
test = as.data.frame(v[1:15])
windows() # opens new image window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#--------------------------------------------------------#
#  Create Negative Words wordcloud                       #
#--------------------------------------------------------#

neg.tdm = dtm[,which(colnames(dtm) %in% negative_words) ]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")

# plot barchart for top tokens
test = as.data.frame(v[1:15])
windows()
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#--------------------------------------------------------#
#  Positive words vs Negative Words plot                 #
#--------------------------------------------------------#

len = function(x){
  if ( x == "-" && length(x) == 1)  {return (0)} 
  else {return(length(unlist(x)))}
}

pcount = unlist(lapply(p, len))
ncount = unlist(lapply(n, len))
doc_id = seq(1:length(wc))

windows()
plot(doc_id,pcount,type="l",col="green",xlab = "Document ID", ylab= "Word Count")
lines(doc_id,ncount,type= "l", col="red")
title(main = "Positive words vs Negative Words" )
legend("topright", inset=.05, c("Positive Words","Negative Words"), fill=c("green","red"), horiz=TRUE)


# Documet Sentiment Running plot
windows()
plot(pol$all$polarity, type = "l", ylab = "Polarity Score",xlab = "Document Number")
abline(h=0)
title(main = "Polarity Plot" )

### COG for sentiment-laden words ? ###

senti.dtm = cbind(pos.tdm, neg.tdm); dim(senti.dtm)
senti.adj.mat = as.matrix(t(senti.dtm)) %*% as.matrix(senti.dtm)
diag(senti.adj.mat) = 0

windows()
distill.cog(senti.adj.mat,   # ad mat obj 
            'Distilled COG of senti words',       # plot title
            5,       # max #central nodes
            5)        # max #connexns
