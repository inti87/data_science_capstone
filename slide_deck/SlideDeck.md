Predict Next word - Shiny App
========================================================
author: Marko Intihar
date: 2021 / 03 /09
autosize: true

The Idea
========================================================

Have you ever wondered how your mobile phone tries to guess the next best word when you are writing messages?

The story behind the scene is that phone applies an algorithm that is predicting the next word using the most recent word or words you type into your phone.

The same logic has been used to create our Shiny App that tries to predict the next word considering a word or a phrase provided by the app user!


The Algorithm - idea
========================================================

- an input: a suitable text source (corpus) with a lot of words

- algorithm cleans the corpus and builds a **n-gram word token frequency tables**

- where **n-gram word token** is a sequence of **n**-words that occur together in the text

- frequency: how many times a **n-gram word token** is found in the corpus (complete word sequence!)


The Algorithm - procedure
========================================================

- user of the app enters a word or a phrase
- algorithm extracts last **n**-words from given phrase
- and estimates the probability of the next word (after given phrase) based on **n-gram word token frequency tables**
- if given combination is not found, algorithm uses **n-1** words for the search
- and if search is still not successful, algorithm is removing words until only one final word remains for the search
- if suitable **n-gram word tokens** are found, the algorithm picks the one with the highest probability 
- and so the **next best word** is found


Shiny App
========================================================

To test our "Next Word Prediction" - Shiny App please visit: <https://inti.shinyapps.io/PredictNextWord>.

App usage:
- write your word or a sequence of words (a phrase) in the left window
- hit **Submit**
- the algorithm will try to predict next best word
- prediction is shown in the right window
- if no match found the app will return a mesage: "Sorry no matching found, please insert a different phrase!"




