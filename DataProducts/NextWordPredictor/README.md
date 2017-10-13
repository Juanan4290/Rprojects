## Overview

This is the final project for the Coursera Data Science Capstone. The goal of the capstone project is to create a predictive text model using a large text corpus of documents as training data. 

For example, if a user types: 

*"I went to the ..."*

the app would suggest the three most likely words that would replace "...", i.e. *gym, store or mall.*  

<div class="white">
Extra space
</div>

* The source codes of the app, the N-gram datasets and the algorithm available on GitHub repo at: https://github.com/Juanan4290/NextWordPredictor

<div class="white">
Extra space
</div>

* The shiny application developed for this course is available at: https://juanan4290.shinyapps.io/nextwordpredictor/


## N-grams summary

Head of *"fourgrams"* and *"trigrams"* data frames loaded by Next Word Predictor App:

```{r n-grams, echo=FALSE, warning=FALSE, out.height=300}

df3 = readRDS("data/trigrams.rds")
df4 = readRDS("data/fourgrams.rds")


head(df4[,1:3])

head(df3[,1:3])

```


## Application Funcionality

Main steps to achieve next word predictions:

<div class="white">
Extra space
</div>

1. Loading 4 data frames contained **n-grams** combinations with 4-words, 3-words, 2-words, and 1-word previously generated.
2. Reading user input (a word or sentence).
3. Cleaning user input (lowering, tokenization of input words: the last four)
4. Call to prediction model function:
    + Search in the **fourgram** data frame. If found, shows top 5 most probable matches. Otherwise:
    + Search in the **trigram** data frame, by the same way above. Otherwise:     
    + Search in **bigram** data frame, by the same way above. 
    + Else, at last, if none matching, displays the most frequent words in the **unigram** data frame.

## Viewing the Shiny App

This is how the Next Word Predictor Application looks like:

```{r image, fig.width=8.5, fig.height = 3, echo=FALSE}
library(png)
library(grid)
appimg <- readPNG('figure/image.png')
grid.raster(appimg)
```

In the left side you can insert a word or sentence and once you click on **Predict** button or press Enter the app will display some words suggested and other information like which N-gram next word was predict with, time invested to procces the algorithm or cleaned text for prediction.
