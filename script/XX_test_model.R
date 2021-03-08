# Test model

rm(list = ls())
graphics.off()

# load prerequsities
library(tm)
library(dplyr)

source("./script/func_corpus_cleaning.R")
source("./script/func_predict_next_word.R")

load("./data/data_for_model/sample_100k_docs_cleaning_stop_bad_stem.RData")


# testing
predict_next_word("dead or")
predict_next_word("paradise")
predict_next_word("paradise city")
predict_next_word("car")
predict_next_word("car accident")
predict_next_word("bus")


predict_next_word("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")

predict_next_word("You're the reason why I smile everyday. Can you follow me please? It would mean the")

predict_next_word("Hey sunshine, can you follow me and make me the")

predict_next_word("Very early observations on the Bills game: Offense still struggling but the")

predict_next_word("Go on a romantic date")


