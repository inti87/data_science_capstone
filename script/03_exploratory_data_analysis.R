# Exploratory Data Analysis (EDA)

# - explore word frequencies
# - explore unique word text coverage
# - create word clouds of most frequent words
# - explore n-grams words (frequencies, word clouds)

# author: Marko Intihar

rm(list =ls())
graphics.off()


# Load functions
source("./script/func_sample_lines.R")
source("./script/func_import_text.R")
source("./script/func_corpus_cleaning.R")
source("./script/func_term_freq_vec_df.R")
source("./script/func_n_gram_df_build.R")
source("./script/func_term_freq_vec_freq_sum.R")
source("./script/func_load_libraries.R")

# Load libraries
libraries <- c("tm", "RWeka", "stringr", "dplyr", "tidyr", "ggplot2", "forcats", "ggwordcloud")
load_lib(libraries)

# Import data (from prep procedure step)
load("./data/data_proc/02_data_prep_news_all_objects.RData")
load("./data/data_proc/02_data_prep_twit_all_objects.RData")
load("./data/data_proc/02_data_prep_blog_all_objects.RData")


# EDA

## Check summaries - size of corpus
length(data.en_news.corp.clean)
length(data.en_blogs.corp.clean)
length(data.en_twitter.corp.clean)

## Questions to answer ???

# Question 1: Some words are more frequent than others - 
#             what are the distributions of word frequencies? 

# Distribution of top 50 most frequent words - news
news.TFV.df.sum %>% 
  filter(rank_word <= 50) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Word") +
  ylab("Frequency (word count in documents)") +
  ggtitle("Top 50 most frequent words in the corpus (news)") +
  theme(axis.text.x = element_text(angle = 90))

# Word cloud - news
set.seed(135) # randomness in positioning labels in the cloud
news.TFV.df.sum %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  filter(rank_word <= 500) %>% 
  ggplot(aes(label = word, 
             size = frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +
  theme_minimal()

# Distribution of top 50 most frequent words - twit
twit.TFV.df.sum %>% 
  filter(rank_word <= 50) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,10000,500)) +
  xlab("Word") +
  ylab("Frequency (word count in documents)") +
  ggtitle("Top 50 most frequent words in the corpus (twitter)") +
  theme(axis.text.x = element_text(angle = 90))

# Word cloud - twit
set.seed(135) # randomness in positioning labels in the cloud
twit.TFV.df.sum %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  filter(rank_word <= 500) %>% 
  ggplot(aes(label = word, 
             size = frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +
  theme_minimal()

# Distribution of top 50 most frequent words - blog
blog.TFV.df.sum %>% 
  filter(rank_word <= 50) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,20000,2500)) +
  xlab("Word") +
  ylab("Frequency (word count in documents)") +
  ggtitle("Top 50 most frequent words in the corpus (blogs)") +
  theme(axis.text.x = element_text(angle = 90))

# Word cloud - blog
set.seed(135) # randomness in positioning labels in the cloud
blog.TFV.df.sum %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  filter(rank_word <= 500) %>% 
  ggplot(aes(label = word, 
             size = frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +
  theme_minimal()



# Question 2: What are the frequencies of 2-grams and 3-grams in the dataset? 

# bi-gram

# news
news.bi.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,2000,250)) +
  xlab("2-gram word") +
  ylab("Frequency (2-gram count in documents)") +
  ggtitle("Top 50 most frequent 2-grams in the corpus (news)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
news.bi.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(150) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +
  theme_minimal()

# twitter
twit.bi.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,1000,100)) +
  xlab("2-gram word") +
  ylab("Frequency (2-gram count in documents)") +
  ggtitle("Top 50 most frequent 2-grams in the corpus (twitter)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
twit.bi.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(150) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +
  theme_minimal()


# blogs
blog.bi.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,1000,100)) +
  xlab("2-gram word") +
  ylab("Frequency (2-gram count in documents)") +
  ggtitle("Top 50 most frequent 2-grams in the corpus (blogs)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
blog.bi.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(100) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +
  theme_minimal()


# three-gram

# news
news.three.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,1000,10)) +
  xlab("2-gram word") +
  ylab("Frequency (3-gram count in documents)") +
  ggtitle("Top 50 most frequent 3-grams in the corpus (news)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
news.three.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(150) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  theme_minimal()

# twitter
twit.three.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,1000,10)) +
  xlab("2-gram word") +
  ylab("Frequency (3-gram count in documents)") +
  ggtitle("Top 50 most frequent 3-grams in the corpus (twitter)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
twit.three.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(150) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  theme_minimal()

# blogs
blog.three.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,100,10)) +
  xlab("2-gram word") +
  ylab("Frequency (3-gram count in documents)") +
  ggtitle("Top 50 most frequent 3-grams in the corpus (blogs)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
blog.three.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(150) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  theme_minimal()


# four-grams

# news
news.four.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,100,2)) +
  xlab("4-gram word") +
  ylab("Frequency (4-gram count in documents)") +
  ggtitle("Top 50 most frequent 4-grams in the corpus (news)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
news.four.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  theme_minimal()

# twitter
twit.four.Grams.df %>%
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(word = as.character(Word)) %>% 
  mutate(word = fct_inorder(f = word)) %>% 
  ggplot(aes(x = word, y = Frequency)) +
  geom_col(color = "black") +
  scale_y_continuous(breaks = seq(0,100,1)) +
  xlab("4-gram word") +
  ylab("Frequency (4-gram count in documents)") +
  ggtitle("Top 50 most frequent 4-grams in the corpus (twitter)") +
  theme(axis.text.x = element_text(angle = 90))

set.seed(135) # randomness in positioning labels in the cloud
twit.four.Grams.df %>% 
  arrange(desc(Frequency)) %>% 
  head(50) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(0.7, 0.3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1,1,4,1,1))) %>% 
  ggplot(aes(label = Word, 
             size = Frequency, 
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 15) +
  theme_minimal()


# Question 3: How many unique words do you need in a frequency sorted dictionary to cover 
#             50% of all word instances in the language? 90%? 

# news
news.TFV.df.sum %>% 
  ggplot(aes(x = rank_word, y = percent_words_covered)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_point(data = news.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.5) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "red", size = 3) +
  geom_hline(yintercept = 0.9, color = "blue", size = 1) +
  geom_point(data = news.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.9) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "blue", size = 3) +
  scale_x_continuous(breaks = seq(0,100000,2500)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  xlab("Number of uinique words (words sorted - occurence)") +
  ylab("Total percentage % of all words covered in documents") +
  ggtitle("Coverage of word instances in the corpus (news)")

news.TFV.df.sum %>% 
  ggplot(aes(x = rank_word, y = percent_words_covered)) +
  geom_area(fill = "gray61", color = "black", alpha = 0.3) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_point(data = news.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.5) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "red", size = 3) +
  geom_hline(yintercept = 0.9, color = "blue", size = 1) +
  geom_point(data = news.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.9) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "blue", size = 3) +
  scale_x_continuous(breaks = seq(0,100000,2500)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  xlab("Number of uinique words (words sorted - occurence)") +
  ylab("Total percentage % of all words covered in documents") +
  ggtitle("Coverage of word instances in the corpus (news)")

# twitter
twit.TFV.df.sum %>% 
  ggplot(aes(x = rank_word, y = percent_words_covered)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_point(data = twit.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.5) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "red", size = 3) +
  geom_hline(yintercept = 0.9, color = "blue", size = 1) +
  geom_point(data = twit.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.9) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "blue", size = 3) +
  scale_x_continuous(breaks = seq(0,100000,2500)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  xlab("Number of uinique words (words sorted - occurence)") +
  ylab("Total percentage % of all words covered in documents") +
  ggtitle("Coverage of word instances in the corpus (twitter)")

twit.TFV.df.sum %>% 
  ggplot(aes(x = rank_word, y = percent_words_covered)) +
  geom_area(fill = "gray61", color = "black", alpha = 0.3) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_point(data = twit.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.5) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "red", size = 3) +
  geom_hline(yintercept = 0.9, color = "blue", size = 1) +
  geom_point(data = twit.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.9) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "blue", size = 3) +
  scale_x_continuous(breaks = c(seq(0,100000,2500))) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  xlab("Number of uinique words (words sorted - occurence)") +
  ylab("Total percentage % of all words covered in documents") +
  ggtitle("Coverage of word instances in the corpus (twitter)")

# blogs
blog.TFV.df.sum %>% 
  ggplot(aes(x = rank_word, y = percent_words_covered)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_point(data = blog.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.5) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "red", size = 3) +
  geom_hline(yintercept = 0.9, color = "blue", size = 1) +
  geom_point(data = blog.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.9) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "blue", size = 3) +
  scale_x_continuous(breaks = seq(0,100000,2500)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  xlab("Number of uinique words (words sorted - occurence)") +
  ylab("Total percentage % of all words covered in documents") +
  ggtitle("Coverage of word instances in the corpus (blogs)")

blog.TFV.df.sum %>% 
  ggplot(aes(x = rank_word, y = percent_words_covered)) +
  geom_area(fill = "gray61", color = "black", alpha = 0.3) +
  geom_hline(yintercept = 0.5, color = "red", size = 1) +
  geom_point(data = blog.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.5) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "red", size = 3) +
  geom_hline(yintercept = 0.9, color = "blue", size = 1) +
  geom_point(data = blog.TFV.df.sum %>% 
               filter(percent_words_covered <= 0.9) %>% 
               tail(1), aes(x = rank_word, 
                            y = percent_words_covered), 
             color = "blue", size = 3) +
  scale_x_continuous(breaks = seq(0,100000,2500)) +
  scale_y_continuous(breaks = seq(0,1,0.05)) +
  xlab("Number of uinique words (words sorted - occurence)") +
  ylab("Total percentage % of all words covered in documents") +
  ggtitle("Coverage of word instances in the corpus (blogs)")
