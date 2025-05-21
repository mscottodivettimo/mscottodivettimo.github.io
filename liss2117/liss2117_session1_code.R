# code version 03/05/2025

      #################################################################
      #### LISS2117 ~ Quantitative methods for text classification ####
      ####            and topic detection                          ####
      ####                                                         ####
      #### Session 1 ~ Key concepts for automated classification   ####
      ####             and introduction to bag-of-words approaches ####
      ####                                                         ####
      #### Dr Michele Scotto di Vettimo                            ####
      #### 07.05.2025                                              ####
      #################################################################

#### This code covers tasks such as creation of a corpus, tokens and document-feature matrix,
#### data exploration with keyword matching and dictionary methods, and related aspects 
#### like dictionary creation, pre-processing steps, and use of dictionary methods for classification.
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
      
if(!require("quanteda")) {install.packages("quanteda"); library("quanteda")} # main text analysis package
if(!require("ggplot2")) {install.packages("ggplot2"); library("ggplot2")} # package used for plotting
if(!require("lexicon")) {install.packages("lexicon"); library("lexicon")} # package used for lemmatization in preprocessing
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")} # package used for some convenient functions for data manipulation 
if(!require("dplyr")) {install.packages("dplyr"); library("dplyr")} # data cleaning
rm(list=ls()) # this is to delete all stuff in the R environment before starting

#### Load in the required libraries
library(tidyverse) # for some convenient functions for data manipulation 
library(quanteda) # core text analysis package
library(lexicon) # for lemmatisation 
library(ggplot2) # for plotting
library(dplyr) # data cleaning

# quanteda is the core R package for quantitative text analysis. It is used for most
# tasks related to data preparation and cleaning, but it also implements many methods
# to actually analyse data. As R is an open-source software, users may have developed other
# packages to implement text as data methodologies. Hence, in this course we will not just
# use quanteda for the analyses, though this package contains many of the most common methods.

# main webpage: http://quanteda.io/
# publication: https://joss.theoj.org/papers/10.21105/joss.00774
# reference manual: https://cran.r-project.org/web/packages/quanteda/quanteda.pdf
# tutorials: https://tutorials.quanteda.io/
# quick start guide: https://quanteda.io/articles/quickstart.html

#### Import data

# We will be using a dataset containing BBC news releases from March 2023
# https://huggingface.co/datasets/RealTimeData/bbc_news_march_2023
# In this sense, we have already solved somehow the step 1 (identifying texts) in the workflow

# I saved a cleaned version of the dataset in rds format, and made it available on my website
# You can access the data directly via the url
bbc_texts <- readRDS(url("https://mscottodivettimo.github.io/liss2117/bbc_news_2023.rds"))

# If you want to import the file from your local laptop (good if you want to access
# to it when you have no internet connection), you can download it from the link 
# (https://mscottodivettimo.github.io/liss2117/bbc_news_2023.rds), save it on your laptop,
# and then import it by uncommenting the following line:

#bbc_texts <- readRDS("bbc_news_2023.rds") 

# Just make sure the file directory is correct for your machine

## Minimal exploration

dim(bbc_texts) # dimension of data: 1982 rows (texts) and 7 variables
str(bbc_texts) # see names and preview variable content
view(bbc_texts) # view dataframe in separate window

# How many texts do we have?
# What information/variables we have for each?
# What variable(s) store the actual texts?

# Feel free to import your own dataset, but make sure that it is in the right shape 
# i.e., each row is a unit of analysis like our example dataset

      ############################################################
      #### Getting data in shape: transforming raw texts with ####
      ####          bag-of-words representations              ####
      ############################################################

#### Bag-of-words representations: key concepts ####

## Transforming texts in a bag-of-words representations

# This part of the code covers the steps needed to set up your data for subsequent text analysis
# We want to make a proper corpus from the imported texts, perform tokenisation and pre-processing
# and eventually have a document-feature matrix that can be used for classification

# Our aim so to get to a document-feature matrix

## First, convert your texts into a proper corpus object with corpus()
# This is necessary because most of the actions we need to perform 
# using the quanteda package requires a corpus as original input

bbc_corpus <- corpus(bbc_texts, text_field = 'content') 
# make sure to tell what variable contains the text ("content" in our bbc_texts data)

# You can see in the environment that we have now a second distinct object

# Note that although we use corpus to refer to our collection of texts, 
# in R the term is used to indicate a specific class of object.
# For instance, we could generally say that our bbc_texts file is our corpus, 
# as it contains the collection of our texts
# This would be true in a general sense, but in R that object is a "data frame", 
# and can't be used directly for text analysis
# Hence, we have to transform it into a "corpus" object via the corpus() function.

## Explore your corpus

head(bbc_corpus)
summary(bbc_corpus,5)

# What additional information the summary() function gives us about our texts?
# Which text, among the 5 displayed, is the longest?

## Tokenise
# We now move to splitting each text in our corpus into individual tokens
# to do so, we use the tokens() function

bbc_tokens0 <- tokens(bbc_corpus)

bbc_tokens0 # view

# We started with texts arranged into a column of a dataset (bbc_texts$content). 
# How are the texts arranged now after tokenisation?

## Move from tokens to dfm
# Then we "compact" the tokens into a document-feature matrix using the dfm() function

bbc_dfm0 <- dfm(bbc_tokens0, tolower = FALSE)
# Note: By default, dfm() transforms all tokens to lowercase unless 
# otherwise specified. That is why we set tolower = FALSE: to keep uppercase. 
# We might get rid of them in preprocessing anyway, but keeping them in the 
# dfm gives us flexibility

bbc_dfm0 # view

# What are the differences between the tokenised texts and the texts now arranged in a dfm?

      ########################
      #### Pre-processing ####
      ########################

# Here we cover many functions and steps that are basically aimed at simplifying our data by
# removing tokens that we deem not useful for our subsequent analysis. These steps have to be
# thought carefully, and you do not necessarily need to implement all of them for your task.

# Lowercasing
tokens_tolower(bbc_tokens0)[1]

# Compare differences
bbc_tokens0[1]
dim(bbc_dfm0)[2]
dim(dfm(tokens_tolower(bbc_tokens0)))[2]

# Why lowercasing can be useful?
# Can you think of problems/issues you can experience/introduce when lowercasing your texts?

# Removing punctuation and numbers
tokens(bbc_tokens0, remove_punct = TRUE, remove_numbers = TRUE)

# Did we get rid of "80th" in first text?
# Removed ' in "parents' " (text3), but not in "Ukraine's" (text1)
# Why it can be the case? (that's no theoretical question, is just about understanding how R works)

## Removing stop words
# (using provided lists of stopwords)

# inspect available lists of stop words
stopwords() # default behaviour 
help("stopwords")
stopwords(language = 'en', source = 'snowball') # default, but called explicitly with instructions
stopwords(language = 'en', source = 'marimo')

tokens_remove(bbc_tokens0, pattern = stopwords(language = 'en', source = 'marimo'))

# tokens_remove() is different from tokens(). The latter is generally used to create
# tokens in the first place. The former (and other tokens_*() functions) are used on
# tokens for various pre-processing steps

# provide custom list by adding words to one of the predefined lists

"can" %in% stopwords(language = 'en', source = 'marimo')

additional_stopwords <- c("can") # define new vector of stop words

# paste them together

my_stopwords <- c(stopwords(language = 'en', source = 'marimo'), additional_stopwords) 

"can" %in% my_stopwords

# remove "can" from text5
tokens_remove(bbc_tokens0, pattern = my_stopwords)

# Removing tokens using custom patterns

bbc_tokens0[1] # let's look at the first document as example

# "fixed" type patterns
tokens_remove(bbc_tokens0, pattern = "members", valuetype = 'fixed', case_insensitive = TRUE)[1]

# Note, case_insensitive = TRUE is default:
tokens_remove(bbc_tokens0, pattern = "members", valuetype = 'fixed', case_insensitive = FALSE)[1]

# "glob" type patterns: https://en.wikipedia.org/wiki/Glob_(programming)
tokens_remove(bbc_tokens0, pattern = "memb*", valuetype = 'glob')[1]

# "regex" (regular expression) type patterns:
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
tokens_remove(bbc_tokens0, pattern = "[:digit:]|[:punct:]", valuetype = 'regex')[1]
# Among other things, this removes "80th" from text1 

# How different this is from the tokens(remove_numbers = TRUE) we used above?

# Removing tokens based on length
tokens_remove(bbc_tokens0, min_nchar = 3)[1]
bbc_tokens0[6] # sixth document has a long token
tokens_remove(bbc_tokens0, max_nchar = 10)[6]

# Grouping in equivalent classes

# Stemming
tokens_wordstem(bbc_tokens0)
# See what happens and try to figure out if for your task is useful/relevant

# Lemmatisation
# Here we need a "lemma table"
# something that links our tokens (e.g., see, seeing, saw) with their corresponding lemma (see)

# use lexicon library
hash_lemmas$token[100:110] # this is a list of tokens
hash_lemmas$lemma[100:110] # this is a list of same length with corresponding lemmas

hash_lemmas$lemma[which(hash_lemmas$token=='saw')] # check if it works with "saw"

bbc_tokens0[4] # let's look at the fourth as example

tokens_replace(bbc_tokens0, pattern = hash_lemmas$token, replacement = hash_lemmas$lemma)[4]

# Use %<% operator (pipe) to write in a more compact form (tidyverse package)

# Join all codes we just saw into single "pipe" and make new tokens object
bbc_tokens1 <- bbc_tokens0 %>% # start from original tokens object, and carry it on 
  tokens_tolower() %>% # perform lowercasing 
  tokens(remove_punct = TRUE) %>% # remove punctuation (note: we are getting rid of numbers below, otherwise switch remove_numbers = TRUE here)
  tokens_remove(pattern = stopwords(language = 'en', source = 'marimo')) %>% # remove stop words (from one of the default list)
  tokens_remove(min_nchar = 2) %>% # and remove short tokens
  tokens_remove(pattern = "[:digit:]|[:punct:]", valuetype = 'regex') %>% # and tokens containing numbers and punctuation
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) # finally, lemmatisation
  
# Compare number of tokens
sum(lengths(bbc_tokens0))
sum(lengths(bbc_tokens1))

sum(lengths(bbc_tokens1))/sum(lengths(bbc_tokens0)) # current tokens are just 44.4% of the original number

# Make new dfm
bbc_dfm1 <- dfm(bbc_tokens1)

dim(bbc_dfm0)
dim(bbc_dfm1) # same docs, less features

# Smaller, but still very "sparse"
head(bbc_dfm1)
# "sparsity" is the share of cells with 0 count

# Reduce complexity/sparsity by removing very rare words and very common ones

# Why we might want to do that? 
# (It will be clearer when we cover methods beyond keywords/dictionaries)

# Some final pre-processing on the dfm directly

bbc_dfmx <- dfm_trim(bbc_dfm1, min_termfreq = 10) # remove terms appearing less than 10 times
head(bbc_dfmx)

bbc_dfmx <- dfm_trim(bbc_dfm1, min_docfreq = 10) # remove terms appearing inn less than 10 documents
head(bbc_dfmx)

bbc_dfmx <- dfm_trim(bbc_dfm1, min_docfreq = 0.01, docfreq_type = 'prop') # remove terms appearing in less than 1% of documents
head(bbc_dfmx)

# Let's do a combination
bbc_dfm1 <- dfm_trim(bbc_dfm1, max_docfreq = 0.5, docfreq_type = 'prop', min_termfreq = 5) 
head(bbc_dfm1)

# Issue with ordering of the pre-processing steps and whether you perform them 
# on the tokens or on the dfm
# This needs to be tailored to your needs. 
# Let' keep this in mind and we will explore more options below when we are 
# doing some dictionary analysis

      #################################################
      ####   Basic approaches to classification:   ####
      #### Keyword matching and dictionary methods ####
      #################################################

# For keyword matching and dictionary methods, pre-processing can be kept to minimum
# 1. they are not resource intensive approaches so they run fast anyway
# 2. they look for words that we specify before hand, so there's little added value 
# in deleting words that we would ignore in the matching anyway.
# Yet, some harmonisation of the terms (e.g. lowercasing or lemmatisation) can help

bbc_tokens1 <- bbc_tokens0 %>%
  tokens_tolower() %>%
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) 

# Note also that we can apply these methods on the token object directly
# we do not even need to get to the dfm (though we could use them on the dfm as well)

## Preliminary exploration of keywords

kwic(bbc_tokens1, 'ukraine', window = 5)
kwic(bbc_tokens1, 'unemployment', window = 5)
kwic(bbc_tokens1, 'unempl*', window = 5) # wildcard for glob pattern

# How many additional matches we get by using the wildcard?
# Can you think about possible issues with using wildcards?

#### Keyword counting ####

## Let's look at the counts of the terms starting with "unempl"

# tokens_select keeps only tokens matching criteria
tokens_select(bbc_tokens1, 'unempl*') 

# What happened to all our nice tokens!?

# make dataframe with counts 
counts <- tokens_select(bbc_tokens1, 'unempl*') %>% dfm() %>% convert(to = "data.frame")

view(counts)

# Why we have two columns (beyond the one with the ids)?
# In the code above what are we using the dfm() function for?

# what terms have been matched by the pattern?
grep('unempl*', names(counts), value = TRUE) 

table(counts$unemployed)
table(counts$unemployment)

# repeat with references to incumbent pm (in 2023): "sunak"
counts <- tokens_select(bbc_tokens1, 'sunak') %>% dfm() %>% convert(to = "data.frame")
table(counts$sunak)

# basic bar chart
ggplot(counts, aes(x = sunak)) + 
  geom_bar(fill='grey', color = 'black') + theme_bw() +
  labs(x = 'Mentions of "Sunak"', y = 'Number of news') +
  geom_text(stat='count', aes(label=..count..), vjust=-1) + ylim(0,1800)

# mentions of "prime minister"
counts <- tokens_select(bbc_tokens1, 'prime minister') %>% dfm() %>% convert(to = "data.frame")
kwic(bbc_tokens1, 'prime minister', window = 5) 

# Are there no references?

bbc_tokens1[[559]][1100:1110]

# Why are we not able to match it?

# If we plan to capture multi-tokens expressions, we have to deal with ngrams
# To do so, we go back to re-creating our tokens so as to deal with ngrams appropriately

# this was our code for cleaning the original tokens object (bbc_tokens0)
#bbc_tokens1 <- bbc_tokens0 %>%
#  tokens_tolower() %>%
#  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma)

# say we want to keep all types of ngrams
# (this is inefficient but good if you can't anticipate
# all possible relevant ngrams and want to do some exploration
# before constructing the dictionary/keywords list)

bbc_tokens2 <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  # add following line to create also bigrams <<----<<
  tokens_ngrams(n = 1:2, concatenator = ' ')
  # make sure you have 1:n, having only n will create only ngrams, dropping single tokens ("unigrams")

bbc_tokens2[[1]][1000:1005]

# have a look again for "prime minister"
kwic(bbc_tokens2, 'prime minister', window = 3) 
# we now have many matches and can have look at context the term appears

# for the same logic, we won't still be able to capture the following:
kwic(bbc_tokens2, 'members of parliament', window = 3) 
# unless we adapt our tokens to:

bbc_tokens3 <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens_ngrams(n = 1:3, concatenator = ' ')

kwic(bbc_tokens3, 'members of parliament', window = 3) 

# as a general rule: make always sure that the resulting tokens match those 
# we want to pick up via keywords/dictionary
# this also applies to more complicated pre-processing like stemming or
# lemmatisation (see below): if tokens have been stemmed (also ngrams), 
# we want to have apply stemming on the keywords as well,
# or use wildcards to be sure we capture stemmed/lemmatised 
# tokens (unigrams and bigrams and so on alike).

# use of wildcards/glob patterns can save you time and give enough flexibility
# whilst retaining control over keywords

kwic(bbc_tokens3, 'member* of parliament', window = 3) 

# a perhaps more efficient and more elegant solution, is to create targeted ngrams
# this has two main advantages: 
# i. makes sure that the multi-tokens keywords we are looking for are properly 
# converted into ngrams in our tokens
# ii. it is more efficient as it does not create ngrams that we won't use, 
# but that increase the size of our dfm

# so if you have a list of keywords to use for keyword matching, 
# you can compound for those (see tokens_compound()):

our_keywords <- c('member* of parliament')

bbc_tokensx <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% # we also remove puntucation just to add a bit of pre-processing
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_compound(pattern = phrase(our_keywords), concatenator = ' ', window = 0)

bbc_tokens0[[210]][459:461]
bbc_tokensx[[210]][409]

# Note (again) on the pre-processing: doing more complex steps is not forbidden,
# though with dictionary matching it has little added value
# Actually, in some cases it can be problematic:
# imagine we remove stopwords like "of" before matching "member* of parliament"

## Plotting overtime trends of our keywords

# select keyword
our_keywords <- 'premier league'

# create tailored tokens 
bbc_tokensx <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_compound(pattern = phrase(our_keywords), concatenator = ' ', window = 0)

# make dataframe
counts <- tokens_select(bbc_tokensx, our_keywords) %>% dfm() %>% convert(to = "data.frame")

# add info about publication date to counts datasets and get daily totals
totals <- counts %>%
  mutate(date = bbc_texts$published_date) %>%
  group_by(date) %>%
  summarise(n = sum(`premier league`)) 

# plot
ggplot(totals, aes(x = date, y = n)) + 
  geom_point(shape = 21, size = 3, fill = 'grey', color = 'black') + 
  geom_line(linewidth = .75, linetype = 'dashed', color = 'grey20') +
  labs(x = '', y = 'Number of instances', title = '"Premier League" mentions in BBC News') + 
  theme_bw()

#### Dictionary method ####

## use off the shelf dictionary

# import lexicoder topic dictionary (comparative agenda topics)
# https://www.snsoroka.com/data-lexicoder

dictionary_LTD2013 <- readRDS(url("https://mscottodivettimo.github.io/liss2117/lexicoder_topic_dictionary.rds"))
#dictionary_LTD2013 <- readRDS('lexicoder_topic_dictionary.rds')

dictionary_LTD2013 # object has a "dictionary" structure: it's a list of lists
# again, "dictionary" is used in a generic sense in text analysis,
# but in R it indicates a very specific way of arranging keywords
# we could store a "dictionary" in columns of excel spreadsheet (that's what I do),
# but then to use them in R one needs to convert them to a dictionary object
# each list has a name, which is the name of the "topic" or "dimension" or "concept" 
# captured by the keywords in the list

names(dictionary_LTD2013) # in our case: 28 topics

# to be used in R, we have to make it a dictionary object using dictionary() function
dictionary_LTD2013 <- dictionary(dictionary_LTD2013)
dictionary_LTD2013

# apply on our tokens (after preparing them)

bbc_tokensx <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_compound(pattern = phrase(dictionary_LTD2013), concatenator = ' ', window = 0)

# see in phrase(): we are again creating ngrams/multi-tokens expression by 
# expanding only those cases that match the terms in our dictionary.
# For dictionary methods, there's no need to expand/concatenate other terms

# lookup functions are the essence of dictionary methods. We using here the 
# tokens_lookup function, that works on -- guess what? -- tokens. There is also a 
# dfm_lookup function that has the same scope (counting keywords) but is implemented 
# on data structured as dfm. Preferring one or the other is mostly a matter of personal 
# taste and convenience.

bbc_tlookup <- tokens_lookup(bbc_tokensx, dictionary = dictionary_LTD2013, case_insensitive = TRUE)

# What is this bbc_tlookup object now?

# adjust results
ltd_dictionary_results <- convert(dfm(bbc_tlookup), to = 'data.frame')
# Note: still using dfm() function, but not with %>% pipe operator now. Doesn't matter, just wRiting style.

view(ltd_dictionary_results)

# How do our results look like? What information does this data contain?
# What if we want to use these results to  classify texts to topics?
# What can be the pros and cons of doing so?

## create variable recording most mentioned topic

# convenience function to convert dictionary matching into topic classification
get_topic <- function(x, cols) {
  u <- x[,c(cols)]
  x$topic <- NA
  for(i in 1:nrow(u)){
    if(all(u[i,]==0)){
      x$topic[i] <- NA
    } else {
      if(length(which(u[i,] == max(u[i,]))) > 1){
        x$topic[i] <- sample(cols[which(u[i,] == max(u[i,]))],1)
      } else {
        x$topic[i] <- cols[which(u[i,] == max(u[i,]))]
      }
    }
  }
  return(x)
}

ltd_dictionary_results <- get_topic(ltd_dictionary_results, names(dictionary_LTD2013))

# What has changed in our data?
# table(ltd_dictionary_results$topic)

# merge with our original dataset of news texts
ltd_dictionary_results <- cbind(bbc_texts, ltd_dictionary_results)

ggplot(ltd_dictionary_results, aes(x = topic)) + 
  geom_bar(colour = 'black', fill = 'grey') + theme_bw() +
  labs(x = 'Topic from LTD2013 Dictionary', y = 'Number of news') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # adjust label orientation for readability
  geom_text(stat='count', aes(label=..count..), vjust=-1) + ylim(0,510)

# get total number of mentions, for each topic, by day of publication
ltd_totals <- ltd_dictionary_results %>%
  group_by(published_date) %>%
  summarise(across(where(is.numeric), sum))

ggplot(ltd_totals, aes(x = published_date, y = crime)) + 
  geom_point(shape = 21, size = 3, fill = 'grey', color = 'black') + 
  geom_line(linewidth = .75, linetype = 'dashed', color = 'grey20') +
  labs(x = '', y = 'Number of keywords matched',
       title = 'Daily coverage of crime issues by BBC News (March 2023)') + 
  theme_bw()

ggplot(ltd_totals, aes(x = published_date)) + 
  geom_line(aes(y = immigration, colour = 'darkblue'), linewidth = 1) +
  geom_line(aes(y = labour, colour = 'red'), linewidth = 1) +
  geom_line(aes(y = defence, colour = 'gold'), linewidth = 1) +
  scale_color_manual(name = "Topic", 
                     values = c("darkblue","red",'gold'), 
                     labels = c('Immigration','Labour','Defence')) +
  labs(x = '', y = 'Number of keywords matched', 
       title = 'Coverage of immigration, labour, and defence issues by BBC News (March 2023)') + 
  theme_bw()

# Dictionary methods as sort of cheap "mixture models"?

      #########################################
      #### Let's create our own dictionary ####
      #########################################

# make sure you match the correct dictionary structure
# nested lists + convert to dictionary R object

our_dictionary <- list() # empty object
our_dictionary[['politics']] <- c('prime minister','government','parliament','labour party','conservative party','mp?','member* of parliament','westminster','politicians','house of commons','house of lords')
our_dictionary[['economy']] <- c('econom*','unemployment','financial','budget*','gdp','inflation')
our_dictionary[['football']] <- c('football','premier league','fa cup','championship','uefa')
our_dictionary <- dictionary(our_dictionary)
our_dictionary

bbc_tokensx <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_compound(pattern = phrase(our_dictionary), concatenator = ' ', window = 0)

bbc_tlookup2 <- tokens_lookup(bbc_tokensx, dictionary = our_dictionary, case_insensitive = TRUE)

our_dictionary_results <- convert(dfm(bbc_tlookup2), to = 'data.frame')

our_dictionary_results <- get_topic(our_dictionary_results, names(our_dictionary))

our_dictionary_results <- cbind(bbc_texts, our_dictionary_results)

# create variable recording most mentioned topic

our_totals <- our_dictionary_results %>%
  group_by(published_date) %>%
  summarise(across(where(is.numeric), sum))

ggplot(our_totals, aes(x = published_date)) + 
  geom_line(aes(y = politics, colour = 'darkblue'), linewidth = 1) +
  geom_line(aes(y = economy, colour = 'red'), linewidth = 1) +
  geom_line(aes(y = football, colour = 'gold'), linewidth = 1) +
  scale_color_manual(name = "Topic", 
                     values = c("darkblue","red","gold"), 
                     labels = c('Politics','Economy','Football')) +
  labs(x = '', y = 'Number of keywords matched', 
       title = 'Coverage of politics and economy by BBC News (March 2023)') + 
  theme_bw()

      #############################################
      #### Scaling of sentiment via dictionary ####
      #############################################

# lexicoder dictionary for sentiment (LSD). It comes with quanteda package
# https://quanteda.io/reference/data_dictionary_LSD2015.html
# It is called: "data_dictionary_LSD2015" and can be accessed directly
# when quanteda package is loaded

data_dictionary_LSD2015 # explore
names(data_dictionary_LSD2015)[1:2]

# let's inspect the sentiment of news in our corpus

bbc_tokensx <- bbc_tokens0 %>% 
  tokens_tolower() %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_compound(pattern = phrase(data_dictionary_LSD2015), concatenator = ' ', window = 0)

# we can use this example also to understand better how tokens_lookup deals with ngrams

bbc_sentiment <- tokens_lookup(bbc_tokensx, dictionary = data_dictionary_LSD2015, case_insensitive = TRUE)

bbc_sentiment <- convert(dfm(bbc_sentiment), to = 'data.frame')

# join with previous results

our_dictionary_results <- merge(our_dictionary_results, bbc_sentiment, by='doc_id')

ggplot(our_dictionary_results, aes(x = published_date, y = log(negative + .5), color = topic)) +
  geom_smooth(aes(fill = topic), alpha = .2) + 
  geom_jitter(alpha = .3) + theme_bw() +
  scale_color_manual(name = "Topic", 
                     values = c("darkblue","red","gold"), 
                     labels = c('Politics','Economy','Football')) +
  scale_fill_manual(name = "Topic", 
                    values = c("darkblue","red","gold"), 
                    labels = c('Politics','Economy','Football'))

our_dictionary_results$posneg_ratio <- bbc_sentiment$positive/(bbc_sentiment$positive + bbc_sentiment$negative + 1)

ggplot(our_dictionary_results, aes(x = published_date, y = posneg_ratio, color = topic)) +
  geom_smooth(aes(fill = topic), alpha = .2) + 
  geom_jitter(alpha = .3) + theme_bw() +
  scale_color_manual(name = "Topic", 
                     values = c("darkblue","red","gold"), 
                     labels = c('Politics','Economy','Football')) +
  scale_fill_manual(name = "Topic", 
                    values = c("darkblue","red","gold"), 
                    labels = c('Politics','Economy','Football'))

      #####################################################
      ####                Validation                   ####
      #### (an introduction, more in the next session) ####
      #####################################################

## Import labelled validation data
# We will be using the BBC dataset collected in 2005 by Greene and Cunningham 
# for their paper on unsupervised classification
# D. Greene and P. Cunningham. "Practical Solutions to the Problem of Diagonal 
# Dominance in Kernel Document Clustering", Proc. ICML 2006.
# http://mlg.ucd.ie/datasets/bbc.html 
# http://mlg.ucd.ie/files/publications/greene06icml.pdf

bbc2005_topics <- readRDS(url("https://mscottodivettimo.github.io/liss2117/bbc_news_topics_gc2005.rds"))

# what topics do the news cover?
table(bbc2005_topics$topic)

# let's create a dictionary to pick up the same topics

our_dictionary <- list()
our_dictionary[['business']] <- c('econom*','unemployment','financ*','gdp','inflation','business')
our_dictionary[['entertainment']] <- c('cinema','theatre','music','film','movie','concert')
our_dictionary[['politics']] <- c('prime minister','government','parliament','mp?','member* of parliament','house of commons')
our_dictionary[['sport']] <- c('football','premier league','fa cup','championship','uefa','cricket','rugby')
our_dictionary[['tech']] <- c('computer*','phone*','teleco*','technol*')
our_dictionary <- dictionary(our_dictionary)
our_dictionary

# make corpus
bbc2005_corpus <- corpus(bbc2005_topics,text_field = 'text')

summary(bbc2005_corpus,5)

# Tokenise
bbc_tokens2005 <- tokens(bbc2005_corpus) %>% 
  tokens_tolower() %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_compound(pattern = phrase(our_dictionary), concatenator = ' ', window = 0)

bbc_tlookup2005 <- tokens_lookup(bbc_tokens2005, dictionary = our_dictionary, case_insensitive = TRUE)

our_dictionary_results <- convert(dfm(bbc_tlookup2005), to = 'data.frame')

our_dictionary_results <- get_topic(our_dictionary_results, names(our_dictionary))

view(our_dictionary_results)

## make a "confusion matrix"
cmat <- table(our_dictionary_results$topic,bbc2005_topics$topic,
              dnn = c('Dictionary','Human coding'))

print(cmat)

# How can we use this matrix to assess how good our dictionary at classification?

sum(diag(cmat))
sum(cmat)

round(sum(diag(cmat))/sum(cmat),3) # rough accuracy measure

cmat_na <- table(our_dictionary_results$topic,bbc2005_topics$topic, 
                 dnn = c('Dictionary','Human coding'), useNA = 'always')

print(cmat_na)

sum(diag(cmat_na))
sum(cmat_na)

round(sum(diag(cmat_na))/sum(cmat_na),3)

# the ratio of correct classification over total number of instances is a very basic
# measure of how good our dictionary (or any method) is in a classification task.
# This was just an introduction to the underlying logic of validation with
# the use of labelled data. We will cover more measures and understand 
# their pros and cons in Session 2.

      ###############################################
      #### Deeper dive into pre-processing steps ####
      ###############################################

## Let's go back to the pre-processing steps.
# Not so central to dictionary approaches, but very useful for any other method
# that relies on bag-of-words representation and uses texts to learn  
# relations between tokens and labels.

# In particular, let's focus on lemmatisation/stemming and n-gram generation

# As the tokens-lemmas table does not have compound tokens, it won't lemmatise our ngrams!
# this might not be a problem depending on what we want lemmatisation for (and how our keywords look like)
# alternatively, it is probably best to lemmatise before creating ngrams,
# and then use the same lemmatisation on the keywords/dictionary you are using
# stemming, instead, should work even on  ngrams

# see following example:

our_sentence <- "This academic article focuses on European Union's policy making."
our_tokens <- tokens(our_sentence)

# look for key-word-in-context:
kwic(our_tokens,'policy making', window = 0) 
# as expected, this is not picked up if we don't create ngrams

# minimal pre-processing omitting some steps to ease the example
our_ngrams <- our_tokens %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_ngrams(n = 1:2, concatenator = ' ') # <<---- create ngrams (n=2)

kwic(our_ngrams,'policy making', window = 0)

# what if we lemmatise AFTER creating the ngrams?
our_lemmatised_ngrams <- our_tokens %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_ngrams(n = 1:2, concatenator = ' ') %>%
  # adding following line
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) 

kwic(our_lemmatised_ngrams,'policy making', window = 0) 
# this works, as lemmatisation did not affect the ngrams already created

# note that stemming, instead, does affect them, so also "policy making" will be stemmed 
# and we will be unable to match it properly:
our_stemmed_ngrams <- our_tokens %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_ngrams(n = 1:2, concatenator = ' ') %>%
  tokens_wordstem(language = 'en')

kwic(our_stemmed_ngrams,'policy making', window = 0) 

# have a look at the lemmatised and stemmed ngrams:
our_lemmatised_ngrams[[1]]
our_stemmed_ngrams[[1]]

# alternatively you can lemmatise before creating ngrams (doesn't make any difference for stemming)
# but then you need to adapt your keywords accordingly:
our_tokens %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_replace(pattern = hash_lemmas$token, replacement = hash_lemmas$lemma) %>%
  tokens_ngrams(n = 1:2, concatenator = ' ') %>% .[[1]]

# as a general rule: make always sure that the resulting tokens match those 
# we want to pick up via keywords/dictionary
# if tokens have been stemmed (also ngrams), we want to have apply stemming 
# on the keywords as well,or use wildcards to be sure we capture stemmed/lemmatised 
# tokens (unigrams and bigrams and so on alike):

kwic(our_stemmed_ngrams,'polic* mak*', window = 0) 

# a (bit convoluted) way of stemming your keywords starting from raw terms:
# It is helpful not to stem keywords manually. In this way, you apply the same
# stemming process that is going to be applied to the remaining corpus
print(stemmed_keyword <- paste(tokens_wordstem(tokens("policy making"))[[1]],collapse = ' '))

kwic(our_stemmed_ngrams,stemmed_keyword, window = 0) # this is equivalent to line 344

# a perhaps more efficient and more elegant solution, is to create targeted ngrams
# this has two main advantages: 
# i. makes sure that the multi-tokens keywords we are looking for are properly 
# converted in ngrams in our tokens
# ii. it is more efficient as it does not create ngrams that we won't use, 
# but that increase the size of our dfm
# however, it implies that the (list of) keyword(s) we have anticipate most 
# of the possible interesting occurrences 

our_tokens %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  # following line creates ngrams only when they match the phrase
  tokens_compound(pattern = phrase(c('policy making')), concatenator = ' ', window = 0)

# so if you have a list of keywords to use for keyword matching, 
# you can compound for those:

our_keywords <- c('european union*','policy making')

our_tokens %>% 
  tokens_tolower() %>% 
  tokens(remove_punct = TRUE) %>% 
  tokens_compound(pattern = phrase(our_keywords), concatenator = ' ', window = 0)

# Still, in the end if you perform lemmatisation or stemming make sure that you do that on your keywords as well 

      ##########################
      #### That's all folks ####
      ##########################