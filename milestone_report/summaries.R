rm(list =ls())


library(tm)
library(tidyverse)

load("C:/ROOT/0100_E_LEARNING/Coursera/10_Data_Science_Capstone/data_science_capstone/data/data_proc/strange_chars_not_removed/02_data_prep_news_all_objects.RData")
load("C:/ROOT/0100_E_LEARNING/Coursera/10_Data_Science_Capstone/data_science_capstone/data/data_proc/strange_chars_not_removed/02_data_prep_twit_all_objects.RData")
load("C:/ROOT/0100_E_LEARNING/Coursera/10_Data_Science_Capstone/data_science_capstone/data/data_proc/strange_chars_not_removed/02_data_prep_blog_all_objects.RData")

# word counts
wc1 <- news.TFV.df.sum %>% pull(frequency) %>% sum()
wc2 <- twit.TFV.df.sum %>% pull(frequency) %>% sum()
wc3 <- blog.TFV.df.sum %>% pull(frequency) %>% sum()

# characters count
cc1 <- news.TFV.df.sum %>% mutate(nchar = nchar(word), nchars = nchar * frequency) %>% pull(nchars) %>% sum()
cc2 <- twit.TFV.df.sum %>% mutate(nchar = nchar(word), nchars = nchar * frequency) %>% pull(nchars) %>% sum()
cc3 <- blog.TFV.df.sum %>% mutate(nchar = nchar(word), nchars = nchar * frequency) %>% pull(nchars) %>% sum()

# line or documents count
lc1 <- news.TDM$dimnames$Docs %>% length()
lc2 <- twit.TDM$dimnames$Docs %>% length()
lc3 <- blog.TDM$dimnames$Docs %>% length()

# object size on disk
os1 <- file.info("C:/ROOT/0100_E_LEARNING/Coursera/10_Data_Science_Capstone/data_science_capstone/data/Coursera-SwiftKey/final/en_US/en_US.news.txt")$size / 1e6
os2 <- file.info("C:/ROOT/0100_E_LEARNING/Coursera/10_Data_Science_Capstone/data_science_capstone/data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")$size / 1e6
os3 <- file.info("C:/ROOT/0100_E_LEARNING/Coursera/10_Data_Science_Capstone/data_science_capstone/data/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")$size / 1e6



# Build table
table.summaries <- tibble(corpus = c("news", "twitter", "blogs"),
                          `object size MB` = c(os1, os2, os3),
                          `line counts`    = c(lc1, lc2, lc3),
                          `word counts`    = c(wc1, wc2, wc3),
                          `character counts` = c(cc1, cc2, cc3))

save(table.summaries, file = "C:/ROOT/0100_E_LEARNING/Coursera/10_Data_Science_Capstone/data_science_capstone/milestone_report/table_summaries.RData")

