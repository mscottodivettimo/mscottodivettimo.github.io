# code version 13/05/2025

      #################################################################
      #### LISS2117 ~ Quantitative methods for text classification ####
      ####            and topic detection                          ####
      ####                                                         ####
      #### Session 2 ~ Topic models and machine-learning           ####
      ####             algorithms for classification               ####
      ####                                                         ####
      #### Dr Michele Scotto di Vettimo                            ####
      #### 14.05.2025                                              ####
      #################################################################

#### This code covers some minimal examples on how to implement topic modelling 
#### and machine learning classification tasks. Building on the previous session,
#### we will use dfm as input for unsupervised models (LDA and STM).
#### Then, we will then run semisupervised models where topic keywords guide
#### the topic detection process (keyATM models). Finally, we move to the use of
#### machine learning algorithms for classification. Though we are covering only
#### the random forest algorithm, most notions are relevant for other methods as well.
#### Throughout the code, we will introduce and use different measures for 
#### validating our results.
#### Some explanation is given in the comments alongside the codes. However, you could
#### make use of the slides in parallel with this code to better understand the logic and aim
#### of some steps. If unsure, just get in touch.
#### Along the code, there are some basic questions for you to consider as we go on coding.
#### They are there to push you to reflect on the purpose/aim of some of the commands we run.

      #############################################################
      #### Preliminaries: package installation and data import ####
      #############################################################
      
#### Install required libraries if you haven't done yet. 
      
# This needs to be done only for the first time you use the libraries. 
# Then, you can comment out or delete these lines of code
      
if(!require("textmineR")) {install.packages("textmineR"); library("textmineR")}
if(!require("Matrix")) {install.packages("Matrix"); library("Matrix")}
if(!require("text2vec")) {install.packages("text2vec"); library("text2vec")}
if(!require("tidytext")) {install.packages("tidytext"); library("tidytext")}
if(!require("ggrepel")) {install.packages("ggrepel"); library("ggrepel")}
rm(list=ls()) # this is to delete all stuff in the R environment before starting

#### Load in the required libraries
library(tidyverse) # for some convenient functions for data manipulation 
library(quanteda) # core text analysis package
library(lexicon) # for lemmatisation 
library(ggplot2) # for plotting
library(dplyr) # data cleaning
library(textmineR) # for some postestimation fit of topic models
library(Matrix) # to adjust dfm input to LDA in textmineR
library(text2vec) # for perplexity function
library(tidytext) # cleaning and displaying of results
library(ggrepel) # plotting

# convenience function to convert dictionary matching into topic classification
get_topic <- function(x, cols = colnames(x), new_var = 'predicted') {
  u <- x[,c(cols)]
  x$pred <- NA
  for(i in 1:nrow(u)){
    if(all(u[i,]==0)){
      x$pred[i] <- NA
    } else {
      if(length(which(u[i,] == max(u[i,]))) > 1){
        x$pred[i] <- sample(cols[which(u[i,] == max(u[i,]))],1)
      } else {
        x$pred[i] <- cols[which(u[i,] == max(u[i,]))]
      }
    }
  }
  names(x)[names(x) == 'pred'] <- new_var
  return(x)
}

# Note, there are other packages that can compute topic models
# though the coding and functions might change, the overall flow is the same
# all these packages just implement variations of Latent Dirichlet Allocation (LDA)
# see also:
# "topicmodels" https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf
# "seededlda" https://cran.r-project.org/web/packages/seededlda/seededlda.pdf (does both seeded and classic LDA)

# load R environment for saved models
load(url("https://mscottodivettimo.github.io/liss2117/liss2117_session2_env.RData"))

# Let's use again the same BBC News corpus we used for dictionary methods
bbc_texts0 <- readRDS(url("https://mscottodivettimo.github.io/liss2117/bbc_news_2023.rds"))

dim(bbc_texts0) 
str(bbc_texts0) 

bbc_texts0 <- bbc_texts0 %>%
  mutate(area=case_when(section == 'Europe' ~ 'Europe',
                        section %in% c('Africa','Asia','Australia','India','China','Middle East','US & Canada','US and Canada','World') ~ 'World',
                        TRUE ~ 'UK'))

# To run a topic model (with this package at least), we have to arrange our texts
# into a document-feature matrix. To do so, we first make a corpus, then split into tokens
# and finally arrange them into a dfm. In doing so, we can do some pre-processing.
# You can have a look at the Session 1 code for an introduction to dfm creation and preprocessing

      #######################################################
      #### Make our dfm and run unsupervised LDA models  ####
      #######################################################

# Make corpus. For the moment we focus on the "content" variable, where the full text is stored
bbc_corpus0 <- corpus(bbc_texts0,text_field = 'content')

# Split corpus into tokens, and do some minimal pre-processing
bbc_tokens0 <- tokens(bbc_corpus0) %>% # start from original tokens object, and carry it on 
  tokens_tolower() %>% # perform lowercasing 
  tokens(remove_punct = TRUE, remove_symbols = TRUE) %>% # remove punctuation (note: we are getting rid of numbers below, otherwise switch remove_numbers = TRUE here)
  tokens_remove(pattern = stopwords(language = 'en', source = 'marimo')) %>% # remove stop words (from one of the default list)
  tokens_remove(pattern = "[[:digit:]|[:punct:]]", valuetype = 'regex')

# Arrange tokens in dfm. Remove very frequent and very rare words
bbc_dfm0 <- dfm(bbc_tokens0) %>% 
  dfm_trim(max_docfreq = 0.5, docfreq_type = 'prop', min_termfreq = 5) 

# For the textmineR package, we have also to convert the dfm into a matrix
# Conceptually, we still think of it as a dfm, though. This is just a technical step
dtm0 <- Matrix(bbc_dfm0)

#### Our first LDA model

# contrary to dictionary methods, there's more computational power required to run topic models
# let's check to see how long it takes to run a very small model 
print(time_start <- Sys.time())
lda_model0a <- FitLdaModel(dtm0, k = 3, iterations = 100); print(execution_time <- Sys.time() - time_start) 
# Note: the execution time depends on your machine's computing power
# on my laptop, it takes between 15-20 seconds 

# Explore the topics we got
as.data.frame(GetTopTerms(lda_model0a$phi, 20)) %>%
  `colnames<-`(paste('Topic',as.character(c(1:3)))) 
  
# LDA is a Bayesian model, it means that some form of random sampling is going on
# behind the scenes. Hence, running the same models on the same data WILL NOT
# lead to exactly the same results (though they should be largely equivalent):

# let's run an identical model to check
lda_model0b <- FitLdaModel(dtm0, k = 3, iterations = 100)
GetTopTerms(lda_model0b$phi, 20)
# are the top terms the same?

# check if two elements (in this case the word-topic assignments) are identical
identical(GetTopTerms(lda_model0a$phi, 100), GetTopTerms(lda_model0b$phi, 100))

## Importance of seeding:

# We need to make sure that the random process starts form a specified point
# so that it can be reproducible
# Note: this random element, and therefore the need to "seed" the execution,
# is central to all methods from now onwards. Keep this in mind when coding!

# for instance
sample(1:100, 3) # give me three random numbers from 1 to 100
sample(1:100, 3) # again
# same instructions leads to different results.
# imagine sending this as a replication material to a journal... not ideal, right?

# set specific starting point for random process and then sample
set.seed(123); sample(1:100, 3) 

set.seed(123); sample(1:100, 3) # sample again, get same results

# Note, using ; allows you to add more commands on the same line.
# I find it convenient in case of seeding because it makes sure that you are 
# running both lines in sequence. However, it is not required, it would work
# even if the commands are on separate lines

# same logic applies to LDA models

set.seed(123); lda_model0c <- FitLdaModel(dtm0, k = 3, iterations = 100)

set.seed(123); lda_model0d <- FitLdaModel(dtm0, k = 3, iterations = 100)

identical(GetTopTerms(lda_model0c$phi, 100),GetTopTerms(lda_model0d$phi, 100))

GetTopTerms(lda_model0c$phi, 10)
GetTopTerms(lda_model0d$phi, 10)

#### Let's run a "bigger" model with 10 topics
# This is actually the model that is on the slides

## Model estimation
set.seed(123); lda_model0 <- FitLdaModel(dtm0, k = 10, iterations = 500, calc_likelihood = TRUE) 
# I would recommend at least 500 iterations

# Quick inspection of terms to make sense of topics
as.data.frame(GetTopTerms(lda_model0$phi, 20)) %>%
  `colnames<-`(paste('Topic',as.character(c(1:10))))

# Snapshot on document-topics distribution
as.data.frame(lda_model0$theta) %>%
  `colnames<-`(paste('Topic',as.character(c(1:10)))) %>% 
  round(3) %>% head(20)

## Model evaluation

# eye-balling top words per topic and reading specific documents is already useful
# for making sense of the results of an LDA model
# However, there are also metrics that we could use to get sense of the quality of
# our results and topics. Note, however, that these are more statistical properties
# rather than conceptual/substantive ones.

# Convergence
lda_model0$log_likelihood %>%
  `colnames<-`(c('iter','loglik')) %>%
  ggplot(aes(x = iter, y = loglik)) + 
  geom_point(shape = 4, size = 1, stroke = 2) + 
  theme_bw() +
  labs(x = 'Number of iterations', y = 'LogLikelihood', 
       title = 'LogLikelihood of LDA model over 500 iterations')

# Note, loglikelihood is affected by k, so it is not good to compare LDA models
# with different k values. However, it is useful to understand if, for a given k
# (and a given dfm), more iterations are needed to ensure model convergence

## plot topic-word distributions

# let's look at words again, but with some charts
phis <- 
  t(lda_model0$phi) %>% as.data.frame() %>%
  `colnames<-`(paste('Topic',as.character(c(1:10)))) %>% 
  mutate(term = row.names(.)) %>%
  pivot_longer(cols = setdiff(names(.),'term')) %>% 
  dplyr::rename(phi = value, topic = name) %>%
  group_by(topic) %>%
  slice_max(phi, n = 10) %>% # select top 10 per topic
  ungroup() %>%
  arrange(topic, -phi)

phis %>%
  mutate(term = reorder_within(term, phi, topic)) %>%
  ggplot(aes(phi, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE, alpha = .7) +
  facet_wrap(~ topic, scales = "free") +
  labs(x = 'Phi (P(token|topic))', y = 'Term', title = 'LDA model on BBC News corpus (k = 10)') + 
  scale_y_reordered() + theme_bw()

## FREX metrics (frequency-exclusivity)

# utility function for FREX (Frequency-exclusivity) 
frex <- function(x, w = 0.7) {
  phi_t <- t(phi_log <- log(x$phi))
  mat <- phi_t / rowSums(phi_t)
  ex <- as.data.frame(apply(mat, 2, rank) / nrow(mat)) %>%
    `colnames<-`(paste('Topic',as.character(c(1:ncol(.))))) %>% 
    mutate(term = row.names(.)) %>%
    pivot_longer(cols = setdiff(names(.),'term')) %>% 
    dplyr::rename(ex = value, topic = name)
  fr <- as.data.frame(apply(phi_t, 2, rank) / nrow(mat)) %>%
    `colnames<-`(paste('Topic',as.character(c(1:ncol(.))))) %>%
    mutate(term = row.names(.)) %>%
    pivot_longer(cols = setdiff(names(.),'term')) %>% 
    dplyr::rename(fr = value, topic = name)
  df <- merge(ex, fr, by = c('term','topic')) %>%
    mutate(frex = 1 / (w /ex + (1 - w) / fr))
  return(df)
}

# use newly created frex() function to extract scores from LDA model
frex_scores <- frex(lda_model0)

frex_scores %>%
  group_by(topic) %>%
  slice_max(frex, n = 20) %>%
  ggplot(aes(x = ex, y = fr, label = term)) + 
  geom_text_repel(aes(ex, fr, label = term, size = frex),
                  segment.color = 'transparent',max.overlaps = Inf) + 
  facet_wrap(~ topic, nrow = 3) +
  labs(x = "Exclusivity",
       y = "Frequency") +
  guides(size = FALSE, scale = 'none') +
  theme(legend.position="none") +
  theme(legend.title=element_blank()) +
  theme(text = element_text(size=10)) +
  theme(axis.text=element_text(size=9)) +
  theme(strip.text = element_text(size=10)) +
  scale_size(range=c(1,4))

# Perplexity metric
cat('Perplexity metric:',
    perplexity0 <- text2vec::perplexity(X = dtm0,
                                        topic_word_distribution = lda_model0$phi,
                                        doc_topic_distribution = lda_model0$theta))

# Topic coherence
lda_model0$coherence
cat('Coherence metric:',mean(lda_model0$coherence))

# How can we use these measures for model comparison?
# loglikelihood -> convergence: determine best number of iterations, once dfm and k are set
# perplexity -> predictive capacity: could be used for deciding k, once dfm is set
# coherence -> how good topics are? less sensitive to dfm size, could be used to find good k value

#### Tuning of LDA model: what is the right number of k?

small_dtm <- Matrix(dfm_subset(bbc_dfm0[1:100,], min_ntoken = 1))

ks <- c(2:30); i <- 0
stat.prp <- stat.cohe <- c() # to store results
lda_models <- list() # empty list to store estimated models

for(k in ks) { # looping over various model specifications to compare their fit
  i <- i + 1 # this index to store results in correct list position
  
  cat('Estimating LDA model with',k,'topics.\n')
  
  set.seed(123); lda_models[[i]] <- FitLdaModel(small_dtm, k = k, iterations = 500, calc_likelihood = TRUE)
  
  stat.prp[i] <- perplexity(X = small_dtm,
                            topic_word_distribution = lda_models[[i]]$phi, 
                            doc_topic_distribution = lda_models[[i]]$theta)
  
  stat.cohe[i] <- mean(lda_models[[i]]$coherence)
}

# Inspect perplexity (as dfm hasn't changed): lower is better
cbind(ks,stat.prp) %>%
  `colnames<-`(c('k','perplexity')) %>%
  ggplot(aes(x = k, y = perplexity)) + 
  geom_point(size = 1) +
  theme_bw() +
  labs(x = 'Number of topics', y = 'Model perplexity', 
       title = 'Perplexity metric of various LDA models')

# Inspect coherence: higher is better
cbind(ks,stat.cohe) %>%
  `colnames<-`(c('k','coherence')) %>%
  ggplot(aes(x = k, y = coherence)) + 
  geom_point(size = 1) +
  theme_bw() +
  labs(x = 'Number of topics', y = 'Model coherence', 
       title = 'Average topic coherence for various LDA models')

# Strike a balance (or just focus on coherence)
which.mins <- function(x, n=5) {head(order(x), n)}
which.maxs <- function(x, n=5) {head(order(-x), n)}

which.mins(stat.prp) # those are the best 5 for perplexity
which.maxs(stat.cohe) # those are the best 5 for coherence

intersect(which.mins(stat.prp), which.maxs(stat.cohe)) # these are in both groups

# once we are happy with our topic model, we can use
# topic-document distributions for classification

bbc_texts1 <- bbc_texts0 %>%
  cbind(lda_model0$theta %>%
          `colnames<-`(paste('Topic',as.character(c(1:ncol(lda_model0$theta))))))

bbc_texts1 <- get_topic(bbc_texts1, grep('Topic',names(bbc_texts1),value = TRUE))

# look at the frequencies of the k topics 
table(bbc_texts1$predicted)

# look at the shares of the k topics
round(prop.table(table(bbc_texts1$predicted))*100, 2)

bbc_texts1 %>% 
  group_by(published_date) %>%
  summarise(across(grep('Topic',names(.),value = TRUE), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = setdiff(names(.),'published_date')) %>%
  ggplot(aes(x = published_date, y = value, fill = name)) + 
  geom_bar(stat = "identity") + theme_bw() + 
  labs(x = 'Date', y = 'Average topic share', fill = '', 
       title = 'BBC News topic coverage (March 2023)') +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

bbc_texts1 %>% 
  group_by(published_date) %>%
  summarise(across(grep('Topic',names(.),value = TRUE), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(cols = setdiff(names(.),'published_date')) %>%
  ungroup() %>%
  subset(name == 'Topic 3') %>%
  ggplot(aes(x = published_date, y = value, group = '')) + 
  geom_line(linewidth = 1) + theme_bw() + 
  labs(x = 'Date', y = 'Average topic share', 
       title = 'Coverage of Topic 1 on BBC News (March 2023)') +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

## Improving pre-processing for cleaner results

bbc_tokens1 <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove(pattern = stopwords(language = 'en', source = 'marimo')) %>% 
  tokens_remove(min_nchar = 2) %>% 
  tokens_remove(pattern = "[[:digit:]|[:punct:]]", valuetype = 'regex') %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) 

sum(lengths(bbc_tokens0))
sum(lengths(bbc_tokens1))

bbc_dfm1 <- dfm(bbc_tokens1)

bbc_dfm1 <- dfm_trim(bbc_dfm1, max_docfreq = 0.5, docfreq_type = 'prop', min_termfreq = 5) 

dim(bbc_dfm0)
dim(bbc_dfm1)

dim(bbc_dfm1)[2]/dim(bbc_dfm0)[2] # new dfm is 75% of original one

dtm1 <- Matrix(bbc_dfm1)

set.seed(123); lda_model1 <- FitLdaModel(dtm1, k = 10, iterations = 500, calc_likelihood = TRUE) 

cat('Perplexity metric:',
    perplexity1 <- perplexity(X = dtm1,
                              topic_word_distribution = lda_model1$phi,
                              doc_topic_distribution = lda_model1$theta))

cat('Coherence metric:',mean(lda_model1$coherence))

# check
perplexity1 < perplexity0 # we want lower perplexity
mean(lda_model1$coherence) > mean(lda_model0$coherence) # we want higher coherence

      #######################################
      #### Structural topic models (STM) ####
      #######################################

if(!require("stm")) {install.packages("stm"); library("stm")}
      
# for STM we will be using stm() package
# https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
# https://www.structuraltopicmodel.com/ (see also list of published applications)

### brief example: modelling topic prevalence as function of both section and date

# STM comes with its own preprocessing function. It can be customised, 
# but here we will use its default behaviour

stm_texts <- textProcessor(bbc_texts0$content, metadata = bbc_texts0)

# We need to arrange our documents, words, and metatada in one object
stm_out <- prepDocuments(stm_texts$documents, stm_texts$vocab, stm_texts$meta)

# then we use the stm() function to actually run the STM, by passing our object
stm_model <- stm(documents = stm_out$documents, 
                 vocab = stm_out$vocab,
                 K = 10,
                 prevalence =~ area + s(published_date), # here you specify how the metadata influence topic prevalence
                 data = stm_out$meta)

plot.STM(stm_model, type = "labels")

labelTopics(stm_model, c(1:10))

stm_effects <- estimateEffect(1:10 ~ area + s(published_date), 
                              stm_model, meta = stm_texts$meta, 
                              uncertainty = "Global")

summary(stm_effects, topics = 1)

# please refer to the documentation and paper cited above for post-estimation
# commands and evaluation functions.
# In general, STM can be evaluated using the same metrics as LDA models

      #################################################
      #### Keyword assisted topic models (KeyATM)  ####
      #################################################

if(!require("keyATM")) {install.packages("keyATM"); library("keyATM")}

# for keyword assisted topic models, we will be using keyATM() package
# https://keyatm.github.io/keyATM/
# https://cran.r-project.org/web/packages/keyATM/keyATM.pdf

# we use again our BBC data with human annotated labels (see Session 1)
bbc_texts2005 <- readRDS(url("https://mscottodivettimo.github.io/liss2117/bbc_news_topics_gc2005.rds"))

set.seed(123); bbc_texts2005 <- bbc_texts2005[sample(1:nrow(bbc_texts2005)), ] # this shuffles our data

table(bbc_texts2005$topic)

# as keyatm are a semi-supervised model, we need to provide seedwords (or keywords) 
# for the topics we are interested in

our_dictionary <- list()
our_dictionary[['business']] <- c('econom*','unemployment','financ*','gdp','inflation','business')
our_dictionary[['entertainment']] <- c('cinema','theatre','music','film','movie','concert','comedy')
our_dictionary[['politics']] <- c('prime minister','government','parliament','mp?','member* of parliament','house of commons')
our_dictionary[['sport']] <- c('football','premier league','fa cup','championship','uefa','cricket','rugby')
our_dictionary[['tech']] <- c('computer*','phone*','teleco*','technol*')
our_dictionary <- dictionary(our_dictionary)
our_dictionary

# make corpus
bbc2005_corpus <- corpus(bbc_texts2005, text_field = 'text')

summary(bbc2005_corpus, 5)

# Tokenise
bbc_tokens2005 <- tokens(bbc2005_corpus, remove_punct = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_compound(pattern = phrase(our_dictionary), concatenator = ' ') %>%
  tokens_remove(pattern = stopwords(language = 'en', source = 'marimo')) %>% 
  tokens_remove(min_nchar = 2) %>% 
  tokens_remove(pattern = "[[:digit:]|[:punct:]]", valuetype = 'regex') %>%
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) 

bbc_dfm2005 <- dfm(bbc_tokens2005)

bbc_dfm2005 <- dfm_trim(bbc_dfm2005, max_docfreq = 0.5, docfreq_type = 'prop', min_termfreq = 5)

# keyATM models do not work well with wildcards or compound tokens, hence, we need to adjust our keywords 
# explicitly. This function looks at our corpus and isolates the tokens matching the patterns in our
# dictionary. It is a bit of a logically redundant step, but we need to do it for keyATM models.
# Don't worry, when these peculiarities arise, package documentation and tutorials tell us what to do

# make keyatm keywords
keyatm_keywords <- read_keywords(docs = keyATM_read(bbc_dfm2005), dictionary = our_dictionary, split = FALSE)

# prepare dfm
keyatm_dfm <- keyATM_read(texts = bbc_dfm2005)

keyatm_model1 <- keyATM(docs = keyatm_dfm, # documents
                        no_keyword_topics = 0, # number of topics without keywords provided
                        keywords = keyatm_keywords, # topics with seed words
                        model = 'base', # model type
                        options = list(seed=123, # set seed for reproducibility
                                       iterations=500))

bbc_texts2005 <- bbc_texts2005 %>%
  cbind(as.data.frame(keyatm_model1$theta)[,1:keyatm_model1$keyword_k] %>%
          `colnames<-`(names(our_dictionary)))

bbc_texts2005 <- get_topic(bbc_texts2005, names(our_dictionary), 'topicmodel_prediction')

      ####################################################
      #### Validation with human categories (KeyATM)  ####
      ####################################################

cmat_tm <- table(bbc_texts2005$topic, bbc_texts2005$topicmodel_prediction,
                 dnn = c('Actual', 'Predicted'))

print(cmat_tm)

sum(diag(cmat_tm)) # element on diagonal (correct classification)
sum(cmat_tm) # all elements

print(accuracy <- sum(diag(cmat_tm))/sum(cmat_tm)) # accuracy

# compare with dictionary 

bbc_tlookup2005 <- tokens_lookup(bbc_tokens2005, dictionary = our_dictionary, case_insensitive = TRUE)

dictionary_results <- convert(dfm(bbc_tlookup2005), to = 'data.frame')

bbc_texts2005$dictionary_prediction <- get_topic(dictionary_results, names(our_dictionary))$predicted

cmat_d <-table(bbc_texts2005$topic, bbc_texts2005$dictionary_prediction,
               dnn = c('Actual', 'Predicted'))

sum(diag(cmat_d))/sum(cmat_d)

# precision 
# let's look at the topics "politics"

print(cmat_tm)
cmat_tm[,'politics'] # assigned to "politics" by model (column)
sum(cmat_tm[,'politics']) # total  
cmat_tm['politics','politics'] # correct classification
# (adapt this depending on whether model classification are in rows or columns)

print(cmat_tm['politics','politics']/sum(cmat_tm[,'politics']))

precision <- c()
for(k in colnames(cmat_tm)){
  precision <- c(precision, cmat_tm[k,k]/sum(cmat_tm[,k]))
}

# recall
recall <- c()
for(k in colnames(cmat_tm)){
  recall <- c(recall, cmat_tm[k,k]/sum(cmat_tm[k,])) 
  # we change the denominator as we now want to capture all instances in k
}

cat('Model performance\n\nAccuracy:',accuracy,
    '\nAvg. Precision:',mean(precision),
    '\nAvg. Recall:',mean(recall))

# fairly balanced performance
# what does the comparison of precision and recall mean in practice?
# look at "politics" topic:
p <- which(colnames(cmat_tm)=='politics')
cat('Precision (politics):',round(precision[p],3),'\nRecall (politics):',round(recall[p],3))

      ###################################################
      #### KeyATM(): additional model specifications ####
      ###################################################

# keyatm() allows you to estimate (discover) additional topics with no keywords
# you just need to decide on how many new topics by passing a value to the no_keyword_topics option

keyatm_model2 <- keyATM(docs = keyatm_dfm, # documents
                        no_keyword_topics = 3, # number of topics without keywords provided
                        keywords = keyatm_keywords, # topics with seed words
                        model = 'base', # model type
                        options = list(seed=123, # set seed for reproducibility
                                       iterations=500))

# you could construct a loop similar to the one we did for LDA, to assess the 
# best value for k by comparing model performance at different k values.
# however, in this case we can use human labels to assess performance

ks <- c(0,2,4,6,8,10); i <- 0
stat.acc <- stat.rec <- stat.prec <- c() # to store results
keyatm_models <- list() # empty list to store estimated models

for(k in ks) { # looping over various model specifications to compare their fit
  i <- i + 1 # this index to store results in correct list position
  
  cat('Estimating KeyATM model with',k,' no keyword topics.\n')
  
  keyatm_models[[i]] <- keyATM(docs = keyatm_dfm, 
                               no_keyword_topics = k, # number of topics without keywords provided
                               keywords = keyatm_keywords, # topics with seed words
                               model = 'base', # model type
                               options = list(seed=123, # set seed for reproducibility
                                              iterations=500))
  
  preds <- get_topic(as.data.frame(keyatm_models[[i]]$theta)[,1:keyatm_models[[i]]$keyword_k] %>%
                       `colnames<-`(names(keyatm_keywords)))$predicted
  
  cm <- table(bbc_texts2005$topic, preds, dnn = c('Actual', 'Predicted'))
  
  stat.acc[i] <- sum(diag(cm))/sum(cm)
  
  .p <- c(); for(k in colnames(cm)){.p <- c(.p, cm[k,k]/sum(cm[,k]))}
  .r <- c(); for(k in colnames(cm)){.r <- c(.r, cm[k,k]/sum(cm[k,]))}
  stat.prec[i] <- mean(.p)
  stat.rec[i] <- mean(.r)
}

as.data.frame(cbind(ks,stat.acc,stat.prec,stat.rec)) %>% 
  `colnames<-`(c('k','Accuracy','Precision','Recall')) %>%
  round(3)

# keyatm() allows you also to "emulate" a structural topic model by modelling 
# document-topic distribution as a function of some covariates

# you need to change the model type under "model", and pass the metadata with the
# covariates using model_setting option
# see: https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_cov.html

keyatm_model2 <- keyATM(docs = keyATM_read(texts = bbc_dfm1), 
                        no_keyword_topics = 0, 
                        keywords = keyatm_keywords, 
                        model = 'covariates', 
                        model_settings=list(covariates_data = bbc_dfm1@docvars,
                                            covariates_formula = ~ area + s(published_date)),
                        options = list(seed=123,
                                       iterations=500),
                        keep = c("Z", "S"))

strata_topic <- by_strata_DocTopic(
  keyatm_model2, by_var = "areaUK",
  labels = c("Europe or World", "UK")
)

plot(
  strata_topic, 
  var_name = "area",
  show_topic = c(1:5), 
  by = "covariate"
)

strata_tw <- by_strata_TopicWord(
  keyatm_model2, keyATM_read(texts = bbc_dfm1),
  by = as.vector(bbc_dfm1@docvars$area)
)

top_words(strata_tw, n = 10)

      ############################################################
      #### Supervised classification: Random Forest algorithm ####
      ############################################################

# Let's say we want to predict whether an article is about politics, entertainment or any other.
# we can get this information from the "topic" variable handcoded by humans

# create a new variable with our label
bbc_dfm2005@docvars$label <- factor(ifelse(bbc_dfm2005@docvars$topic == 'politics', 'Politics',
                                    ifelse(bbc_dfm2005@docvars$topic == 'entertainment', 'Entertainment', 'Other')))
table(bbc_dfm2005@docvars$label)

# we can now use this dataset to train a classifier that will learn how our tokens 
# map into the label 

if(!require("randomForest")) {install.packages("randomForest"); library("randomForest")}
if(!require("pROC")) {install.packages("pROC"); library("pROC")}

# we need, however, to do a train-test split first 
# (for tuning, people would actually do a train-test-validation split)

# there is no rule for the size of train-test split, but many established practices (e.g., 80 vs 20, 75 vs 25)
# really depends on how many data you have, how balanced they are, etc. etc.
set.seed(123); train_ind <- sample(seq_len(dim(bbc_dfm2005)[1]), size = 0.8 * dim(bbc_dfm2005)[1])

test_ind <- setdiff(seq_len(dim(bbc_dfm2005)[1]), train_ind)

train_x <- as.matrix(bbc_dfm2005[c(train_ind),])
train_y <- bbc_dfm2005@docvars$label[train_ind]

test_x <- as.matrix(bbc_dfm2005[c(test_ind),])
test_y <- bbc_dfm2005@docvars$label[test_ind]

# we can now estimate our first random forest model
# ntree determines how many separate decision trees we want in our forest

# again, do not forget to seed your forest...
rf1 <- randomForest(x = train_x[1:100,], y = train_y[1:100], ntree = 100)
rf2 <- randomForest(x = train_x[1:100,], y = train_y[1:100], ntree = 100)

# even with the same specification and data, the models delived slightly different results
identical(rf1,rf2)

set.seed(123); randomforest_model <- randomForest(x = train_x, y = train_y, ntree = 200)
# see help(randomForest) for other parameters that we could tune (e.g., mtry)
# 500 trees at minimum are advised, but it takes longer to compute

print(randomforest_model) # confusion matrix 
# (but this is only on the data in the training set. It is constructed with an approach 
# called out of bag (OOB) error. It is a way of getting some sort of performance measure even 
# without test data)
plot(randomforest_model) # graphically display the errors

# Use the model to make predictions on the test set
preds <- predict(randomforest_model, newdata = test_x, type='class')

cmat_rf <- table(test_y, preds, dnn = c('Actual', 'Predicted'))

print(accuracy <- sum(diag(cmat_rf))/sum(cmat_rf)) # that's awesome! 

print(cmat_rf) # issues?

# get precision metric
precision <- c(); for(k in colnames(cmat_rf)){precision <- c(precision, cmat_rf[k,k]/sum(cmat_rf[,k]))}

# get recall metric
recall <- c(); for(k in colnames(cmat_rf)){recall <- c(recall, cmat_rf[k,k]/sum(cmat_rf[k,]))}
      
mean(precision)
mean(recall)
      
      ########################################
      #### Additional validation measures ####
      ########################################

## F1 score

f1 <- 2*((precision * recall)/(precision + recall))
mean(f1)

## balanced accuracy

bal_acc <- mean(recall)
bal_acc_w <- weighted.mean(recall, unname(table(test_y)))

bal_acc_w == accuracy # as you can see, when we weight by class size we end up with traditional accuracy measure

#### going beyond accuracy is useful for imbalanced data
# what does imbalanced mean? one or few categories are underrepresented in our data

# simulate an imbalanced dataset
round(prop.table(table(.actual <- c(rep('Politics',80),rep('Entertainment',100),rep('Other',1000)))),3)
.pred <- c(rep('Politics',30),rep('Entertainment',110),rep('Other',1040))

# get confusion matrix
print(cm <- table(.actual,.pred, dnn = c('Actual', 'Predicted')))

# calculate metrics
print(.accuracy <- sum(diag(cm))/sum(cm))

.precision <- c(); for(k in colnames(cm)){.precision <- c(.precision, cm[k,k]/sum(cm[,k]))}
.recall <- c(); for(k in colnames(cm)){.recall <- c(.recall, cm[k,k]/sum(cm[k,]))}

.f1 <- 2*((.precision * .recall)/(.precision + .recall))
mean(.f1)

print(.bal_acc <- mean(.recall))

#### classify texts and add labels to our data

# we can now use our trained model to predict labels on a new set of texts
# let's use the bbc 2023 data for this scope

label_this <- convert(bbc_dfm1, to = 'data.frame')
for(f in setdiff(colnames(train_x),colnames(label_this))){label_this[f] <- 0}

preds <- predict(randomforest_model, newdata = as.matrix(label_this), type='class')

bbc_texts1$predicted_label <- preds

# have a look at the results 

      ######################################
      #### Introduction to model tuning ####
      ######################################

# tuning means finding optimal parameter values for training the model

# 1. decide which parameters can/needs to be tuned (this depends on the model
# and on the importance of the parameter for classification performance)

help("randomForest")

# ntree	
# Number of trees to grow. This should not be set to too small a number, 
# to ensure that every input row gets predicted at least a few times.

# mtry	
# Number of variables randomly sampled as candidates at each split. 
# Note that the default values are different for classification 
# (sqrt(p) where p is number of variables in x) and regression (p/3)

# 2. set parameter values that you want to try for each parameter

round(sqrt(dim(bbc_dfm2005)[2]), 0) # this is the default value of mtry in our case

try.ntree <- c(50, 100, 150)
try.mtry <- c(50, 88, 100)

# 3. split your data in train, validation and test set
# (alternatively, you can use cross-validation during tuning, important is that all final evaluations are on the test set)

set.seed(123); train_ind <- sample(seq_len(dim(bbc_dfm2005)[1]), size = 0.6 * dim(bbc_dfm2005)[1])
set.seed(123); valid_ind <- sample(setdiff(seq_len(dim(bbc_dfm2005)[1]),train_ind), size = 0.2 * dim(bbc_dfm2005)[1])

test_ind <- setdiff(seq_len(dim(bbc_dfm2005)[1]), union(valid_ind, train_ind))

# check that we did not leak some observations...
is_empty(intersect(test_ind, train_ind))
is_empty(intersect(valid_ind, train_ind))
is_empty(intersect(test_ind, valid_ind))

train_x <- as.matrix(bbc_dfm2005[c(train_ind),])
train_y <- bbc_dfm2005@docvars$label[train_ind]

valid_x <- as.matrix(bbc_dfm2005[c(valid_ind),])
valid_y <- bbc_dfm2005@docvars$label[valid_ind]

test_x <- as.matrix(bbc_dfm2005[c(test_ind),])
test_y <- bbc_dfm2005@docvars$label[test_ind]

# size of our datasets
length(train_y)
length(valid_y)
length(test_y)

# 4. try different models, one for each combination of parameter values, and assess their performance

model_fit_table <- c()

# how many models are we tuning?
length(try.ntree)*length(try.mtry)

for(.ntree in try.ntree){
  for(.mtry in try.mtry){
    
    # 4.1 fit model on train data
    set.seed(123); m <- randomForest(x = train_x, y = train_y, mtry = .mtry, ntree = .ntree)
    
    # 4.2 assess performance on validation set
    cm <- table(valid_y, predict(m, newdata = valid_x, type='class'), dnn = c('Actual', 'Predicted'))
    
    .accuracy <- sum(diag(cm))/sum(cm)
    
    .precision <- c(); for(k in colnames(cm)){.precision <- c(.precision, cm[k,k]/sum(cm[,k]))}
    
    .recall <- c(); for(k in colnames(cm)){.recall <- c(.recall, cm[k,k]/sum(cm[k,]))}
    
    .f1 <- 2*((.precision * .recall)/(.precision + .recall))
    
    model_fit_table <- rbind(model_fit_table, c(.ntree, .mtry, .accuracy, mean(.precision), mean(.recall),mean(.f1)))
  }
}
colnames(model_fit) <- c('ntree','mtry','accuracy','precision','recall','f1')
model_fit <- as.data.frame(model_fit)

# 4.3 Select parameter values used for the best performing model
model_fit$ntree[which.max(model_fit$f1)]
model_fit$mtry[which.max(model_fit$f1)]

# 5. test model on test set to simulate real world scenario
# (validation set influences the model as it is used for tuning, hence it is not anymore a credible benchmark)

set.seed(123); tuned_model <- randomForest(x = train_x, y = train_y, mtry = 88, ntree = 150)

cm <- table(test_y, predict(tuned_model, newdata = test_x, type='class'), dnn = c('Actual', 'Predicted'))

.accuracy <- sum(diag(cm))/sum(cm)
.precision <- c(); for(k in colnames(cm)){.precision <- c(.precision, cm[k,k]/sum(cm[,k]))}
.recall <- c(); for(k in colnames(cm)){.recall <- c(.recall, cm[k,k]/sum(cm[k,]))}
.f1 <- 2*((.precision * .recall)/(.precision + .recall))

cat('Accuracy:',.accuracy,'\nF1:',mean(.f1))

# 6. if test performance is satisfactory and no additional tuning is needed,
# you can train the model once again on the full labelled data, with the 
# parameter values decided after tuning, and then use it to label unseen data

################################################
#### Other common classification algorithms ####
################################################

# naive bayes: https://cran.r-project.org/web/packages/naivebayes/naivebayes.pdf
# gradient boosting: https://cran.r-project.org/web/packages/xgboost/xgboost.pdf
# k-nearest neighbour: https://cran.r-project.org/web/packages/class/class.pdf

#save.image(file = 'liss2117_session2_env.RData')
