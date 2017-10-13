library(tm)

maxNgrams = 3
topFreq = 5

# Read N-grams
#load("freq1.Rdata"); load("freq2.Rdata"); load("freq3.Rdata")

df2 = readRDS("bigrams.rds")
df3 = readRDS("trigrams.rds")
df4 = readRDS("fourgrams.rds")
msg <- ""

# Functions to clean str
removeWhiteSpaces = function(str) {
    return( gsub("\\s+"," ",str) )
}

clean = function(input) {
    input = removePunctuation(input)
    input = removeNumbers(input)
    input = removeWhiteSpaces(input)
    input = tolower(input) 
    return(input)
}

# Getting last N words of the text
getLastNwords = function(txt, n) {
    txt = clean(txt)
    txtElems = strsplit(txt," ")[[1]]
    if (length(txtElems) < n) {
        return (txt)
    } else {
        lowerBound = (length(txtElems) - n + 1)
        txtElems = txtElems[lowerBound:length(txtElems)]
        lastWords = paste(txtElems, collapse = " ")
        return (lastWords)
    }
}

# Collect the top frequent words from unigram table and return in a vector
getTop <- function(n=topFreq) {
    df1 <- readRDS("unigrams.rds")
    nrows <- nrow(df1)
    vTop <- vector("list", length=25)
    for(i in seq_len(25))
        vTop[i] <- as.character(df1[i,1])
    return(head(vTop,n))
}


# Most probable word in a n-gram dataframe , given a group of n last words
findGrams = function(lastWord, n) {
    lastWord = paste0(lastWord,' ')

    # subset 'n-gram' dataframe to find the most probable occurrences 
    if(n==3)
        dfsub = subset(df4, grepl(lastWord, df4$Word))
    else
        if(n==2)
            dfsub = subset(df3, grepl(lastWord, df3$Word))
    else
        if(n==1)
            dfsub = subset(df2, grepl(lastWord, df2$Word))
    
    
    if(nrow(dfsub) > 0) {
        topWords = head(gsub(lastWord,"",dfsub$Word),topFreq)
        msg <<- sprintf("Next word was predicted with %1d-gram dataframe",(n+1))
        #print(msg)
        return (topWords)
    }
    else{
        n = n-1;
        if(n > 0) { 
            lastWord = getLastNwords(lastWord, n)
            findGrams(lastWord, n)
        }
        else {
            #lastWord = substr(lastWord,1,nchar(lastWord)-1)
            msg <<- paste("Next word not found in 2, 3 or 4-grams dataframes.\nReturning the",TOP_FREQ,"most frequent words of uni-gram:")
            #print(msg)
            return (getTop(topFreq))
        }
    }
}

#Model
predictModel <- function(userInput) {
    return( findGrams(getLastNwords(userInput,maxNgrams), n = maxNgrams) )
}
