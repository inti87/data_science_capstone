# Exploratory Data Analysis (EDA)

rm(list =ls())
graphics.off()

library(tm)
library(RWeka)
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
source("./script/n_gram_df_build.R")

# Import clean data
ch.import <- "clean.sample.news" # sample or full data?

if(ch.import == "sample"){
  load("./data/clean_sample_data.RData")
}else if(ch.import == "clean.news"){
  load("./data/news_corpus_clean.RData")
}else if(ch.import == "clean.sample.news"){
  load("./data/news_corpus_clean_sample.RData")
}else(load("./data/clean_data.RData"))

# EDA

## Check summaries - size of corpus
length(data.en_news.corp.clean)
# length(data.en_blogs.corp.clean)
# length(data.en_twitter.corp.clean)


# Count-based evaluation

# Build Term-Document Matrix
set.seed(11235)
news.TDM <- TermDocumentMatrix(data.en_news.corp.clean)
# news.corp.dtm <- TermDocumentMatrix(data.en_news.corp.clean)
# news.corp.dtm <- as.matrix(news.corp.dtm) # convert to matrix

## Return term that occur at least k-times
freq100 <- findFreqTerms(x = news.TDM, lowfreq = 100, highfreq = Inf) # occurred at least 100 times
freq100

## Build term frequency vector & term frequency vector stored as data frame
news.TFV <- sapply(data.en_news.corp.clean, termFreq) # TFV vector-list
TFV.df <- term_freq_vec_df(news_TFV = news.TFV, parallel = TRUE) # TFV vector-data frame

# save.image("./data/TFV_built.RData")
# load("./data/TFV_built.RData")

## Term frequency vector word level summarized
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


# Build n-grams (multiple words) - we will also check frequencies of n-grams
set.seed(123)
random_lines <- sample(x = 1:length(data.en_news.corp.clean), size = 50000, replace = F)
one.Grams.df <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 1)
two.Grams.df <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 2)
three.Grams.df <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 3)
four.Grams.df <- n_gram_df_build(corpus = data.en_news.corp.clean[random_lines], n = 4)


# Question 1: Some words are more frequent than others - 
#             what are the distributions of word frequencies? 

# Distribution of top 50 most frequent words
TFV.df.sum %>% 
  filter(rank_word <= 50) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,100000,1000)) +
  xlab("Word") +
  ylab("Frequency (word count in documents)") +
  ggtitle("Top 50 most frequent words in the corpus") +
  theme(axis.text.x = element_text(angle = 90))

# Word cloud
set.seed(135) # randomness in positioning labels in the cloud
TFV.df.sum %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  filter(rank_word <= 250) %>% 
  ggplot(aes(label = word, 
             size = rank_word, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  theme_minimal()


# Question 2: What are the frequencies of 2-grams and 3-grams in the dataset? 
two.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  #scale_y_continuous(breaks = seq(0,100000,1000)) +
  xlab("2-gram word") +
  ylab("Frequency (2-gram count in documents)") +
  ggtitle("Top 50 most frequent 2-grams in the corpus") +
  theme(axis.text.x = element_text(angle = 90))

three.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  #scale_y_continuous(breaks = seq(0,100000,1000)) +
  xlab("2-gram word") +
  ylab("Frequency (3-gram count in documents)") +
  ggtitle("Top 50 most frequent 3-grams in the corpus") +
  theme(axis.text.x = element_text(angle = 90))

four.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  #scale_y_continuous(breaks = seq(0,100000,1000)) +
  xlab("4-gram word") +
  ylab("Frequency (4-gram count in documents)") +
  ggtitle("Top 50 most frequent 4-grams in the corpus") +
  theme(axis.text.x = element_text(angle = 90))



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

save.image(file = "./data/eda.RData")
