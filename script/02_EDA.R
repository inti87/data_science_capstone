# Exploratory Data Analysis (EDA)

rm(list =ls())
graphics.off()

library(tm)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(ggwordcloud)

source("./script/sample_lines.R")
source("./script/import_text.R")
source("./script/corpus_cleaning.R")
source("./script/term_freq_vec_df.R")

# Import clean data
ch.import <- "sample" # sample or full data?

if(ch.import == "sample"){
  
  load("./data/clean_sample_data.RData")
  
}else if(ch.import == "clean.news"){
  
  load("./data/news_corpus_clean.RData")
  
}else(load("./data/clean_data.RData"))

# EDA

## Check summaries - size of corpus
length(data.en_news.corp.clean)
length(data.en_blogs.corp.clean)
length(data.en_twitter.corp.clean)


# Count-based evaluation

## Build Term-Document Matrix
set.seed(11235)
news.TDM <- TermDocumentMatrix(data.en_news.corp.clean)
# news.corp.dtm <- TermDocumentMatrix(data.en_news.corp.clean)
# news.corp.dtm <- as.matrix(news.corp.dtm) # convert to matrix

## Return term that occur at least k-times
freq100 <- findFreqTerms(x = news.TDM, lowfreq = 100, highfreq = Inf) # occurred at least 100 times
freq100

# Build term frequency vector & term frequency vector stored as data frame
news.TFV <- sapply(data.en_news.corp.clean, termFreq) # TFV vector-list
TFV.df <- term_freq_vec_df(news_TFV = news_TFV, parallel = TRUE) # TFV vector-data frame

# Term frequency vector word level summarized
TFV.df.sum <- TFV.df %>% 
  group_by(word) %>% 
  summarise(in_docs = n(), 
            frequency = sum(frequency)) %>% 
  ungroup() %>% 
  arrange(desc(frequency)) %>% 
  mutate(frequency_total_cum_sum = cumsum(frequency),
         frequency_total = sum(frequency),
         rank_word = row_number(),
         percent_words_covered = frequency_total_cum_sum / frequency_total) %>% 
  select(rank_word, everything())


# Question 1: Some words are more frequent than others - 
#             what are the distributions of word frequencies? 

# Distribution of top 50 most frequent words
TFV.df.sum %>% 
  filter(rank_word <= 50) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,15000,500)) +
  xlab("Word") +
  ylab("Frequency (word count in documents)") +
  ggtitle("Top 50 most frequent words in the corpus") +
  theme(axis.text.x = element_text(angle = 90))

# Word cloud
set.seed(135) # randomness in positioning labels in the cloud
TFV.df.sum %>% 
  filter(rank_word <= 30) %>% 
  ggplot(aes(label = word, 
             size = rank_word, 
             #angle = angle1,
             #color = manufacturer
             )) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 30) +
  #scale_color_viridis_d(option = "magma") +
  theme_minimal()

# Question 3: How many unique words do you need in a frequency sorted dictionary to cover 
#             50% of all word instances in the language? 90%? 
TFV.df.sum %>% 
  ggplot(aes(x = rank_word, y = percent_words_covered)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_point(data = TFV.df.sum %>% 
               filter(percent_words_covered <= 0.5) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "red", size = 3) +
  geom_hline(yintercept = 0.9, color = "blue", size = 1) +
  geom_point(data = TFV.df.sum %>% 
               filter(percent_words_covered <= 0.9) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "blue", size = 3) +
  scale_x_continuous(breaks = c(1250, seq(5000,100000,2500))) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  xlab("Number of uinique words (words sorted - occurence)") +
  ylab("Total percentage % of all words covered in documents") +
  ggtitle("Coverage of word instances in the corpus")


