install.packages("tm")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("quanteda")
install.packages("syuzhet")
install.packages("stringr")
install.packages("dplyr")
install.packages("quanteda.textstats")
library(quanteda.textstats)
library(dplyr)
library(tm)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(syuzhet)
library(stringr)

setwd("/Users/ash/Documents/BDA3Proj")
file_path <- "APrincessOfMars.txt"
book_lines <- readLines(file_path, encoding = "UTF-8")

# Extracting Chapter 1 from APrincessOfMars.txt
indx_ch1 <- which(book_lines == "CHAPTER I")
indx_ch2 <- which(book_lines == "CHAPTER II")
book_chapter1 <- book_lines[(indx_ch1 + 1):(indx_ch2 - 1)]
book_chapter1[1:10]

dir.create("chapters")
write.table(book_chapter1, file = "chapters/book_chapter1.txt", sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE)

# Extracting other chapters from APrincessOfMars.txt
clist <- c("CHAPTER II","CHAPTER III","CHAPTER IV","CHAPTER V","CHAPTER VI","CHAPTER VII","CHAPTER VIII","CHAPTER IX","CHAPTER X","CHAPTER XI", "CHAPTER XII")

for (chapterIdx in seq_len(length(clist)-1)) {
  file_path <- "APrincessOfMars.txt"
  book_lines <- readLines(file_path, encoding = "UTF-8")
  
  indx_chx <- which(book_lines == clist[chapterIdx])
  indx_chy <- which(book_lines == clist[chapterIdx+1])
  
  book_chapter <- book_lines[(indx_chx + 1):(indx_chy - 1)]
  write.table(book_chapter, file = paste0("chapters/book_chapter", as.character(chapterIdx + 1), ".txt"), sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
}

BCorpus <- VCorpus(DirSource("chapters", ignore.case=TRUE, mode="text"))
str(BCorpus)
BCorpus

# Finding 10 longest words and 10 longest sentences in each chapter
book_words <- tidy(BCorpus) %>%
  unnest_tokens(word, text)%>% 
  select(id, word) %>% 
  mutate(word_length = nchar(word)) %>%
  arrange(desc(word_length))

book_sentences <- tidy(BCorpus) %>%
  unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>%
  select(id, sentence) %>%
  mutate(sentence_length = nchar(sentence)) %>%
  arrange(desc(sentence_length))

for (i in 1:11){
  file_path <- paste0("book_chapter", as.character(i), ".txt")
  print(book_words %>% filter(id == file_path))
  print(book_sentences %>% filter(id == file_path))
}


btext <- BCorpus[[1]]
btext
btext[1]

BookDTM <- DocumentTermMatrix(BCorpus)
BookDTM
inspect(BookDTM)
str(BookDTM)

BookTDM <- TermDocumentMatrix(BCorpus)
BookTDM
inspect(BookTDM)
str(BookTDM)

Bookdf <- data.frame(btext[1])
Bookdf[1]

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
Bookcl <- tm::tm_map(BCorpus, content_transformer(removeNumPunct))
Bookcl
str(Bookcl)
inspect(Bookcl)
content(Bookcl[[1]])



BookLow <- tm::tm_map(Bookcl, content_transformer(tolower))
content(BookLow[[1]])
str(BookLow)
inspect(BookLow)

# Document Term Matrix of the entire File
BookDTM <- DocumentTermMatrix(BookLow)
BookDTM
inspect(BookDTM)
str(BookDTM)
as.matrix(BookDTM)

# Term Document Matrix of the entire file
BookTDM <- TermDocumentMatrix(BookLow)
BookTDM
inspect(BookTDM)
str(BookTDM)
as.matrix(BookTDM)

myStopwords <- c(tm::stopwords("english"))
myStopwords
BookStop <- tm::tm_map(BookLow, tm::removeWords, myStopwords)
tm::inspect(BookStop[[1]])
str(BookStop)

myCustomStopwords <- c("can","will","one","shall","two")
BookStop <- tm::tm_map(BookStop, tm::removeWords, myCustomStopwords)
tm::inspect(BookStop[[1]])

BookStopTDM <- tm::TermDocumentMatrix(BookStop)
BookStopTDM
inspect(BookStopTDM)
str(BookStopTDM)

BookStopDTM <- tm::DocumentTermMatrix(BookStop)
BookStopDTM
inspect(BookStopDTM)
str(BookStopDTM)

# Word Frequency
wordfreq=colSums (as.matrix (BookStopDTM))
wordfreq

freqTerms <- tm::findFreqTerms(BookStopTDM, lowfreq = 5)
freqTerms
nchar(freqTerms[3])
freqTerms[3]

freqTerms <- tm::findFreqTerms(BookStopTDM, lowfreq = 4)
freqTerms
nchar(freqTerms[3])
freqTerms[3]

freqTerms <- tm::findFreqTerms(BookStopTDM, lowfreq = 8)
freqTerms
nchar(freqTerms[3])
freqTerms[3]

freqTerms <- tm::findFreqTerms(BookStopTDM, lowfreq = 10)
freqTerms
nchar(freqTerms[3])
freqTerms[3]

# Words Length Calculation
words <- Terms(BookStopTDM)
word_lengths <- vector("numeric", length(words))
for (i in seq_along(words)) {
  word_lengths[i] <- nchar(words[i])
}
word_lengths

Booktf <- tm::termFreq(BookStop[[1]])
Booktf
tm::inspect(BookStopTDM)

# Dendrogram
Bookdf <- as.data.frame(BookStopTDM[[1]])
BookDist <- dist(Bookdf)
BookDG <- hclust(BookDist, method = "ward.D2")
str(BookDG)
plot(BookDG)

# Dendrogram by removing words with high sparsity

# Convert the term-document matrix to a matrix format
NoHSTDM = as.matrix(BookStopTDM)
# Set the sparsity threshold
sparsity_threshold <- 0.85
# Calculate the sparsity of each row in the matrix
sparsity <- rowSums(NoHSTDM > 0)/ncol(NoHSTDM)
# Select the rows where the sparsity is greater than the threshold
new_words <- which(sparsity > sparsity_threshold)
# Subset the original matrix by selecting only the selected rows
filtered_TDM <- NoHSTDM[new_words,]
# Convert the filtered matrix to a data frame format
NoHSTDM_DF <- as.data.frame(filtered_TDM)
# Calculate the distance matrix for the filtered data frame
NoHSTDM_dist <- dist(NoHSTDM_DF)
# Create a hierarchical clustering object using the distance matrix and "ward.D2" linkage method
NoHSTDM_DG <- hclust(NoHSTDM_dist, method = "ward.D2")
# Print the structure of the hierarchical clustering object
str(NoHSTDM_DG)
# Create a dendrogram plot of the hierarchical clustering object
plot(NoHSTDM_DG)

# Wordcloud
words <- names(Booktf)
words
pal <- brewer.pal(9, "BuGn")
str(pal)

# Plotting Wordcloud
wordfreq = Booktf
BookWC <- wordcloud(words, wordfreq, colors = pal[-(1:4)])
str(BookWC)

pal2 <- brewer.pal(9,"Spectral")
BookWC <- wordcloud(words, wordfreq, colors = pal2)
str(BookWC)

# Quanteda
BookText <- Bookcl[[1]]
BookText$content[1:10]

BookTokens <- quanteda::tokens(BookText$content[1:10])
str(BookTokens)

# Remove blank lines
remove_blanks_and_tokenize <- function(text) {
  non_blank_lines <- grep("^\\s*$", text, invert = TRUE)
  text_no_blanks <- text[non_blank_lines]
  tokens <- quanteda::tokens(text_no_blanks)
  return(tokens)
}

BookTokensNB <- remove_blanks_and_tokenize(BookText$content)
str(BookTokensNB)

# Removing the stop words from Document Feature Matrix
stop_words <- quanteda::stopwords("english")
BookDFM <- quanteda::dfm(BookTokensNB, remove = stop_words)
str(BookDFM)

BookDocFreq <- quanteda::docfreq(BookDFM)
str(BookDocFreq)
BookDocFreq

BookWeights <- quanteda::dfm_weight(BookDFM)
str(BookWeights)
BookWeights

# Computing TF-IDF Scores (Term Frequency - Inverse Document Frequency)
BookTFIDF <- quanteda::dfm_tfidf(BookDFM, scheme_tf = "count", scheme_df = "inverse")
str(BookTFIDF)
BookTFIDF

# Sentiment Analysis

BSDictionary <- get_sentiment_dictionary()
BSDictionary

BSDictionaryBing <- get_sentiment_dictionary("bing")
BSDictionaryBing

BSDictionaryNRC <- get_sentiment_dictionary("nrc")
BSDictionaryNRC

# Creating a table and computing sentiments

computeSentiment <- function(text) {
  sentences <- get_sentences(text)
  sentimentValues <- get_sentiment(sentences, "syuzhet")
  sentimentValuesBing<-get_sentiment(sentences, "bing")
  sentimentValuesNRC<-get_sentiment(sentences, "nrc")
  sentimentSum <- sum(sentimentValues)
  sentimentSumBing <- sum(sentimentValuesBing)
  sentimentSumNRC <- sum(sentimentValuesNRC)
  sentimentMean <- mean(sentimentValues)
  sentimentMeanBing <- mean(sentimentValuesBing)
  sentimentMeanNRC <- mean(sentimentValuesNRC)
  
  list(syuzhetSum = sentimentSum, syuzhetMean = sentimentMean, bingSum = sentimentSumBing, bingMean = sentimentMeanBing, nrcSum = sentimentSumNRC, nrcMean = sentimentMeanNRC )
}

AllChapters <- list.files(path = "chapters/", pattern = "book_chapter.*\\.txt$", full.names = TRUE)

# Compute Sentiment Values, Mean and Sum Values
results <- lapply(AllChapters, function(chapter) {
  text <- get_text_as_string(chapter)
  computeSentiment(text)
})

# Creating a Table
chapter_names <- basename(AllChapters)

table <- tibble(
  Chapter = chapter_names,
  SentimentSum_Syuzhet = sapply(results, function(res) res$syuzhetSum),
  SentimentMean_Syuzhet = sapply(results, function(res) res$syuzhetMean),
  SentimentSum_Bing = sapply(results, function(res) res$bingSum),
  SentimentMean_Bing = sapply(results, function(res) res$bingMean),
  SentimentSum_NRC = sapply(results, function(res) res$nrcSum),
  SentimentMean_NRC = sapply(results, function(res) res$nrcMean)
)

print(table)

# Sentiment Analysis for Chapter - 1 ----------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter1.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 2 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter2.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 3 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter3.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 4 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter4.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 5 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter5.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 6 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter6.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 7 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter7.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 8 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter8.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 9 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter9.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 10 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter10.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")


# Sentiment Analysis for Chapter - 11 -------------------------------------------------------------------------

BookAsString <- get_text_as_string("chapters/book_chapter11.txt")
BookAsString
BS <- get_sentences(BookAsString)
BS[1:5]

BSSentiment <- get_sentiment(BS, "syuzhet")
BSSentiment

BSBing <- get_sentiment(BS, "bing")
BSBing

BSSum <- sum(BSSentiment)
BSSum
BSBingSum <- sum(BSBing)
BSBingSum

BSMean <- mean(BSSentiment)
BSMean
BSBingMean <- mean(BSBing)
BSBingMean

# Sentiment Analysis on extracted sentences using get_nrc_sentiment
BSSentimentNRC <- get_sentiment(BS, "nrc")
BSSentimentNRC

BSSumNRC <- sum(BSSentimentNRC)
BSSumNRC

BSMeanNRCMean <- mean(BSSentimentNRC)
BSMeanNRCMean

summary(BSSentimentNRC)

# Narrative vs Emotional Valence Plot 
plot(BSSentiment, main = "Book Plot Trajectory", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSBing, main = "Book Plot Trajectory - Bing", xlab = "Narrative", ylab = "Emotional Valence")
plot(BSSentimentNRC, main = "Book Plot Trajectory - NRC", xlab = "Narrative", ylab = "Emotional Valence")

# With 10 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 10)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 10 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")

# With 20 Bins
BSSentimentPctValue <- get_percentage_values(BSSentiment, bins = 20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main = "Book PCTValue 20 Bins", xlab = "Narrative", ylab = "Emotional Valence", col = "red")



# Quanteda Package

TrimDFM <- dfm_trim(BookDFM, min_termfreq = 5, min_docfreq = 2)
TrimDFM
BookDFM

mySimilarity <- textstat_simil(BookDFM, method = "cosine")
mySimilarity

mySummary <- textstat_summary(BookDFM)
mySummary

topFeatures <- topfeatures(BookDFM, n = 10)
topFeatures

# Topic Models Package

bookLDA <- topicmodels::LDA (BookStopDTM, k = 5)
topic_word_dist <- as.data.frame(bookLDA@beta)
topic_word_dist

document_topic_dist <- as.data.frame(bookLDA@gamma)
document_topic_dist

bookCTM <- topicmodels::CTM(BookStopDTM, k = 5)
topic_word_dist <- as.data.frame(bookCTM@beta)
topic_word_dist

document_topic_dist <- as.data.frame(bookCTM@gamma)
document_topic_dist

BookSparse <- removeSparseTerms(BookStopTDM, 0.4)
inspect(BookSparse)
str(BookSparse)

# Work Cloud Package

words = names(Booktf)
pal <- brewer.pal(9,"Spectral")
PoMarsWC = wordcloud(words, Booktf, colors = pal)

colors <- brewer.pal (9, "Set1")
BookStopTDMMat = as.matrix(BookStopTDM)
comparison.cloud(BookStopTDMMat, colors = colors, scale = c(3, 0.5), random.order = FALSE, title.size = 0.6)

commonality.cloud(BookStopTDMMat, colors = colors, scale = c (3, 0.5), random.order = FALSE)


# Syuzhet

NRCSentiment <- get_nrc_sentiment(BS)
NRCSentiment


barplot(
  sort(colSums(prop.table(NRCSentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)


ftValues <- get_transformed_values(
  BSSentiment, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)


plot(
  ftValues, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "green"
)


dctValues <- get_dct_transform(
  BSSentiment, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
plot(
  dctValues, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "green"
)

