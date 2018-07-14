# Including needed libraries
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(tidyverse)
library(wordcloud)

start.time <- Sys.time()

# Preparing parameters
n <- 1000
lang <- "es"

path_training <- "~/Descargas/Text Mining en Social Media/pan-ap17-bigdata/training"  	# Your training path
path_test <- "~/Descargas/Text Mining en Social Media/pan-ap17-bigdata/test"							# Your test path
k <- 10
r <- 1  #repeticiones

# List of frequent words to be removed
list <- c("si","q","d","x","hoy","gracias","vía","ser") 

# Auxiliar functions
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the bow representation
GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) 
  {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    # Preprocessing the text
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      }
      line <- paste(line, ",", thefreq, sep="")
    }
   
    # Concatenating the corresponding class: variety or gender
    if (class=="variety") {
      line <- paste(variety, ",", line, sep="")
    } else {
      line <- paste(gender, ",", line, sep="")
    }
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}

#Getting the vocabulary for the most frequent terms by class (gender/variety), for data exploration
GenerateVocabularyPerClass <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", class="variety", classValue="colombia", verbose = TRUE) {
  setwd(path)
  
  # Reading the truth file
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  if(class=="variety") {
    truth<-truth[truth$variety==classValue,]
  }
  else {
    truth<-truth[truth$gender==classValue,]
  }
  
  truth$file <- paste(truth$author, ".xml", sep="")
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    if(is.element(file, truth$file)) {
      xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
      corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      if (verbose) print(paste(i, " ", file))
      i <- i + 1
    }
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitespaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")  {
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (length(swlist)>0) {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
  
}

#Getting the vocabulary for the most frequent by women not used by men and viceversa
GenerateGenderVocabulary <- function() {
  
  genderVocabulary = data.frame()
  words <- NULL
  freqs <- NULL
  
  for(word in vocabularyFEM$WORD) {
    
    if(!is.element(c(word), vocabularyMAL$WORD)) {

      words <- rbind(words ,word)
      freqs <- rbind(freqs ,vocabularyFEM[vocabularyFEM$WORD==word,]$FREQ)
    }
    
  }
  
  for(word in vocabularyMAL$WORD) {
    
    if(!is.element(c(word), vocabularyFEM$WORD)) {

      words <- rbind(words ,word)
      freqs <- rbind(freqs ,vocabularyMAL[vocabularyMAL$WORD==word,]$FREQ)
    }
  }
  
  genderVocabulary <- cbind(words,freqs)
  colnames(genderVocabulary) <- c("WORD","FREQ")
  
  return(data.frame(genderVocabulary))
}

#Data Exploration. Creation of vocabularies for each class of gender and variety to generate wordclouds and plots 
#This is done to get a better sense of how each class behaves
vocabularyCOL <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="variety", classValue="colombia", verbose = FALSE)
vocabularySPA <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="variety", classValue="spain", verbose = FALSE)
vocabularyVEN <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="variety", classValue="venezuela", verbose = FALSE)
vocabularyMEX <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="variety", classValue="mexico", verbose = FALSE)
vocabularyPER <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="variety", classValue="peru", verbose = FALSE)
vocabularyCHL <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="variety", classValue="chile", verbose = FALSE)

vocabularyFEM <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="gender", classValue="female", verbose = FALSE)
vocabularyMAL <- GenerateVocabularyPerClass(path_training, n, swlang=lang,swlist=list,class="gender", classValue="male", verbose = FALSE)

#Wordclouds by class
set.seed(142)
dark<-brewer.pal(6,"Dark2")

png(filename="0Colombia.png")
wordcloud(vocabularyCOL$WORD,vocabularyCOL$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

png(filename="0Spain.png")
wordcloud(vocabularySPA$WORD,vocabularySPA$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

png(filename="0Venezuela.png")
wordcloud(vocabularyVEN$WORD,vocabularyVEN$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

png(filename="0Mexico.png")
wordcloud(vocabularyMEX$WORD,vocabularyMEX$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

png(filename="0Peru.png")
wordcloud(vocabularyPER$WORD,vocabularyPER$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

png(filename="0Chile.png")
wordcloud(vocabularyCHL$WORD,vocabularyCHL$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

png(filename="0Female.png")
wordcloud(vocabularyFEM$WORD,vocabularyFEM$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

png(filename="0Male.png")
wordcloud(vocabularyMAL$WORD,vocabularyMAL$FREQ,max.words=100, rot.per=0.2, colors=dark)
dev.off()

#Plots by class
#First, convert vocabulary to dataframes
wfCOL <- data.frame(word=vocabularyCOL$WORD, freq=vocabularyCOL$FREQ)   
wfSPA <- data.frame(word=vocabularySPA$WORD, freq=vocabularySPA$FREQ)   
wfVEN <- data.frame(word=vocabularyVEN$WORD, freq=vocabularyVEN$FREQ)   
wfMEX <- data.frame(word=vocabularyMEX$WORD, freq=vocabularyMEX$FREQ)   
wfPER <- data.frame(word=vocabularyPER$WORD, freq=vocabularyPER$FREQ)   
wfCHL <- data.frame(word=vocabularyCHL$WORD, freq=vocabularyCHL$FREQ)   

wfFEM <- data.frame(word=vocabularyFEM$WORD, freq=vocabularyFEM$FREQ)   
wfMAL <- data.frame(word=vocabularyMAL$WORD, freq=vocabularyMAL$FREQ)   

png(filename="00Colombia-barras.png")
ggplot(subset(wfCOL, freq>750), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

png(filename="00Spain-barras.png")
ggplot(subset(wfSPA, freq>750), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

png(filename="00Venezuela-barras.png")
ggplot(subset(wfVEN, freq>750), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

png(filename="00Mexico-barras.png")
ggplot(subset(wfMEX, freq>750), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

png(filename="00Peru-barras.png")
ggplot(subset(wfPER, freq>750), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

png(filename="00Chile-barras.png")
ggplot(subset(wfCHL, freq>750), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

png(filename="00Female-barras.png")
ggplot(subset(wfFEM, freq>2000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

png(filename="00Male-barras.png")
ggplot(subset(wfMAL, freq>2000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
dev.off()

# GENERATE VOCABULARY
vocabulary <- GenerateVocabulary(path_training, n, swlang=lang,swlist=list)

genderVocabulary2 <- GenerateGenderVocabulary()

# GENDER IDENTIFICATION
#######################
# GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TRAINING SET

#bow_training_gender <- GenerateBoW(path_training,n, vocabulary, class="gender") #For test with general vocabulary
bow_training_gender <- GenerateBoW(path_training,genderVocabulary2, class="gender",verbose=FALSE) #For tests with gender exclusive vocabulary

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training_gender <- concat.split(bow_training_gender, "V1", ",")
training_gender <- cbind(training_gender[,2], training_gender[,4:ncol(training_gender)])
names(training_gender)[1] <- "theclass"

# Learning a SVM and evaluating it with k-fold cross-validation0

train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
model_SVM_gender <- train( theclass~., data= training_gender, trControl = train_control, method = "svmLinear")
print(model_SVM_gender)
model_c50_gender <- train( theclass~., data= training_gender, trControl = train_control, method = "C5.0Tree")
print(model_c50_gender)
model_rf_gender <- train( theclass~., data= training_gender, trControl = train_control, method = "rf")
print(model_rf_gender)

# Learning a SVM with the whole training set and without evaluating it
#train_control <- trainControl(method="none")
#model_SVM_gender <- train( theclass~., data= training_gender, trControl = train_control, method = "svmLinear")

# GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TEST SET
#bow_test_gender <- GenerateBoW(path_test, vocabulary, class="gender")
bow_test_gender <- GenerateBoW(path_test, genderVocabulary2, class="gender")

# Preparing the vector space model and truth for the test set
test_gender <- concat.split(bow_test_gender, "V1", ",")
truth_gender <- unlist(test_gender[,2])
test_gender <- test_gender[,4:ncol(test_gender)]

# Predicting and evaluating the prediction

pred_SVM_gender <- predict(model_SVM_gender, test_gender)
pred_c50_gender <- predict(model_c50_gender, test_gender)
pred_rf_gender <- predict(model_rf_gender, test_gender)

confusionMatrix(pred_SVM_gender, truth_gender)
confusionMatrix(pred_c50_gender, truth_gender)
confusionMatrix(pred_rf_gender, truth_gender)


# VARIETY IDENTIFICATION
########################
# GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TRAINING SET
bow_training_variety <- GenerateBoW(path_training, vocabulary, n=1000,class="variety")

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training_variety <- concat.split(bow_training_variety, "V1", ",")
training_variety <- cbind(training_variety[,2], training_variety[,4:ncol(training_variety)])
names(training_variety)[1] <- "theclass"

# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl( method="repeatedcv", number = k , repeats = r)
model_SVM_variety <- train( theclass~., data= training_variety, trControl = train_control, method = "svmLinear")
print(model_SVM_variety)
model_c50_variety <- train( theclass~., data= training_variety, trControl = train_control, method = "C5.0Tree")
print(model_c50_variety)
model_rf_variety <- train( theclass~., data= training_variety, trControl = train_control, method = "rf")
print(model_rf_variety)

# Learning a SVM with the whole training set and without evaluating it
#train_control <- trainControl(method="none")
#model_SVM_variety <- train( theclass~., data= training_variety, trControl = train_control, method = "svmLinear")

# GENERATING THE BOW FOR THE GENDER SUBTASK FOR THE TEST SET
bow_test_variety <- GenerateBoW(path_test, vocabulary, n=1000,class="variety")

# Preparing the vector space model and truth for the test set
test_variety <- concat.split(bow_test_variety, "V1", ",")
truth_variety <- unlist(test_variety[,2])
test_variety <- test_variety[,4:ncol(test_variety)]

# Predicting and evaluating the prediction
pred_SVM_variety <- predict(model_SVM_variety, test_variety)
pred_c50_variety <- predict(model_c50_variety, test_variety)
pred_rf_variety <- predict(model_rf_variety, test_variety)
confusionMatrix(pred_SVM_variety, truth_variety)
confusionMatrix(pred_c50_variety, truth_variety)
confusionMatrix(pred_rf_variety, truth_variety)

# Baseline
# N         GENDER  VARIETY JOINT   TIME
# 10        0.5875  0.2608  0.1442  3.62m
# 50        0.6850  0.3167  0.2142  4.32m      
# 100       0.7375  0.3383  0.2525  5.36m
# 500       0.7358  0.5717  0.4175  9.16m
# 1000      0.6983  0.6167  0.4325  12.11m      
# 5000      0.7550  0.7275  0.5517  51.81m     
# 10000     IMPOSSIBLE, RSTUDIO CRASHES