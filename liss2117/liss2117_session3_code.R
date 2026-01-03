# code version 20/05/2025

      #################################################################
      #### LISS2117 ~ Quantitative methods for text classification ####
      ####            and topic detection                          ####
      ####                                                         ####
      #### Session 3 ~ Word-embeddings approaches                  ####
      ####             and large language models                   ####
      ####                                                         ####
      #### Dr Michele Scotto di Vettimo                            ####
      #### 21.05.2025                                              ####
      #################################################################

#### This code covers a minimal examples on how to obtain a word-embeddings
#### representation of our texts and using it for classification with 
#### conventional machine learning algorithms.
#### We will using the word2vec() package to convert tokens to vectors and then
#### aggregate word-embeddings at the document-level before classification.
#### Some explanation is given in the comments alongside the codes. However, you could
#### make use of the slides in parallel with this code to better understand the logic and aim
#### of some steps. If unsure, just get in touch.

      #############################################################
      #### Preliminaries: package installation and data import ####
      #############################################################
      
#### Install required libraries if you haven't done yet. 
      
# This needs to be done only for the first time you use the libraries. 
# Then, you can comment out or delete these lines of code
      
if(!require("word2vec")) {install.packages("word2vec"); library("word2vec")}
if(!require("openxlsx")) {install.packages("openxlsx"); library("openxlsx")}
if(!require("stringr")) {install.packages("stringr"); library("stringr")}

rm(list=ls()) # this is to delete all stuff in the R environment before starting

#### Load in the required libraries
library(tidyverse) # for some convenient functions for data manipulation 
library(quanteda) # core text analysis package
library(stringr)
library(openxlsx) # for plotting
library(Matrix) # to adjust dfm input 
library(word2vec) # for perplexity function

# load R environment for saved models
load(url("https://mscottodivettimo.github.io/liss2117/liss2117_session3_env.RData"))

# Let's use sentences from the Comparative Manifesto Project
# there are sentences from party manifestos that have been classified by coders into different
# categories, depending on the content
manifesto_texts0 <- read.xlsx("https://mscottodivettimo.github.io/liss2117/manifesto_project.xlsx")

dim(manifesto_texts0) 
str(manifesto_texts0)

# create index to split into train and test set 
# this is needed both for "traditional" classification and for word-embeddings classification
set.seed(123); train_ind <- sample(seq_len(nrow(manifesto_texts0)), size = 0.8 * nrow(manifesto_texts0))

      #######################################################
      #### "Traditional" machine learning classification ####
      #######################################################

# create our tokens with some minimal pre-processing
tokens0 <- manifesto_texts0 %>%
  corpus(text_field = 'text_original', docid_field = 'idx') %>%
  tokens() %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_trim(min_termfreq = 5, max_docfreq = 0.5, docfreq_type = 'prop')

# arrange our tokens into a dfm
dfm0 <- dfm(tokens0)

# select texts for training set
train_x1 <- as.matrix(dfm0[c(train_ind),])

# and record the assigned label
train_y <- dfm0@docvars$label[train_ind]

# do the same for the test set
test_x1 <- as.matrix(dfm0[-train_ind,])
test_y <- dfm0@docvars$label[-train_ind]

# explore labels and frequencies
table(train_y)
table(test_y)

# fit a random forest model on the dfm (training data only)
set.seed(123); randomforest_model1 <- randomForest(x = train_x1, y = factor(train_y), ntree = 500)
print(randomforest_model1)

# get prediction from the test set
preds1 <- predict(randomforest_model1, newdata = test_x1, type = 'class')

# assess predictions by comparing them with true test labels
print(cm1 <- table(test_y, preds1, dnn = c('Actual', 'Predicted')))

# compute classification measures as usual
accuracy1 <- (sum(diag(cm1))/sum(cm1)) 

precision1 <- c(); for(k in colnames(cm1)){precision1 <- c(precision1, cm1[k,k]/sum(cm1[,k]))}

recall1 <- c(); for(k in colnames(cm1)){recall1 <- c(recall1, cm1[k,k]/sum(cm1[k,]))}

mean(precision1)
mean(recall1)

f1_score1 <- mean(2*((precision1 * recall1)/(precision1 + recall1)))

balanced_accuracy1 <- mean(recall1)

      #######################################################
      #### Embeddings + machine learning classification  ####
      #######################################################

# https://cran.r-project.org/web/packages/word2vec/readme/README.html
# https://cran.r-project.org/web/packages/word2vec/word2vec.pdf

# from the tokens, we need to create word vectors with word2vec()
# we can decide of how many dimensions

word_vectors <- as.matrix(word2vec(as.list(tokens0), type = "skip-gram", dim = 1000))

word_vectors[10:15,1:10]

# our labels are at the text-level. With a dfm we used word frequencies in a document
# to predict labels. Now, we have n-dimensional numerical vectors for each word in our 
# corpus. We need to condense this information at the document level so as to pair it with
# our labels.

# easiest way is to average the vectors 

# Function to convert each document to average word vector
get_doc_vector <- function(doc, model_matrix) {
  words <- str_split(doc, " ")[[1]]
  valid_words <- words[words %in% rownames(model_matrix)]
  if (length(valid_words) == 0) return(rep(0, ncol(model_matrix)))
  colMeans(model_matrix[valid_words, , drop = FALSE])
}

# get texts as "string" (e.g., like raw)
texts_from_tokens <- sapply(tokens0, function(x) paste(x, collapse = " "))

texts_from_tokens[1] # texts are not arranged in dfm or tokens or corpus.

# create document-vectors by averaging word vectors
doc_vectors <- t(sapply(texts_from_tokens, get_doc_vector, model_matrix = word_vectors))

doc_vectors[1:5,1:10] 
# to each document now corresponds a numeric value for each of the dimensions extracted
# compare this to a dfm for equivalent purpose = rows : document; cols : features (here cols: dimensions)

# split in train and test set as above
train_x2 <- doc_vectors[c(train_ind),]

test_x2 <- doc_vectors[-train_ind,]

# Train Random Forest algorithm on the document vectors (not dfm)
set.seed(123); randomforest_model2 <- randomForest(x = train_x2, y = factor(train_y), ntree = 500)
print(randomforest_model2)

# Predict on the test document vectors
preds2 <- predict(randomforest_model2, newdata = test_x2, type = 'class')

# compare with test labels and calculate performance measures
print(cm2 <- table(test_y, preds2, dnn = c('Actual', 'Predicted')))

accuracy2 <- (sum(diag(cm2))/sum(cm2)) 

precision2 <- c(); for(k in colnames(cm2)){precision2 <- c(precision2, cm2[k,k]/sum(cm2[,k]))}

recall2 <- c(); for(k in colnames(cm2)){recall2 <- c(recall2, cm2[k,k]/sum(cm2[k,]))}

mean(precision2)
mean(recall2)

f1_score2 <- mean(2*((precision2 * recall2)/(precision2 + recall2)))

balanced_accuracy2 <- mean(recall2)

save.image(file = 'liss2117_session3_env.RData')
