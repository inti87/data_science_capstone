# Sample text (lines sampling)
set.seed(12345)

data.en_news.sample <- sample_lines(text = data.en_news, type = "number", number = 1000)
data.en_news.sample.corp <- Corpus(VectorSource(data.en_news.sample))

inspect(data.en_news.sample.corp)






# Quiz 1

# Q2
data.en_twitter %>% length()

# Q3
news.lines    <- data.en_news %>% sapply(X = ., FUN = nchar)
blogs.lines   <- data.en_blogs %>% sapply(X = ., FUN = nchar)
twitter.lines <- data.en_twitter %>% sapply(X = ., FUN = nchar)
names(news.lines) <- NULL; names(blogs.lines) <- NULL; names(twitter.lines) <- NULL
c(max(news.lines), max(blogs.lines), max(twitter.lines))

# Q4
sum(data.en_news %>% str_detect(string = ., pattern = "love")) / sum(data.en_news %>% str_detect(string = ., pattern = "hate"))
sum(data.en_news %>% str_to_lower(string = .) %>% str_detect(string = ., pattern = "love")) / sum(data.en_news %>% str_to_lower(string = .) %>%  str_detect(string = ., pattern = "hate"))

# Q5
data.en_twitter %>% str_subset(string = ., pattern = "biostats")

# Q6
data.en_twitter %>% str_subset(string = ., pattern = "A computer once beat me at chess, but it was no match for me at kickboxing")
