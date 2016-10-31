---
title: "Twitter Sentiment Analysis"
author: "Callin Switzer"
date: "10/30/2016"
output: html_document
---

This is a twitter sentiment analysis, modified largely from this location:
https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/R/sentiment.R

Install packages
```{r}
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "twitteR", 'ROAuth', 'RCurl', "stringr", 'tm', 
              "devtools", "rjson", "bit64", "httr", "RJSONIO",   'plyr', 
              'stringr', 'XML')
ipak(packages)

# set working directory to script directory
setwd('~/Documents/RDI')

```

```{r}
# get info for using twitter's API
keys = cbind(read.table('keys.txt'))
```



#### Sentiment Analysis ####
```{r}
# twitter setup
setup_twitter_oauth(consumer_key = as.character(keys[1,1]), consumer_secret = as.character(keys[1,2]),
                    access_token = as.character(keys[1,3]), access_secret = as.character(keys[1,4])
                    )

# retrieve the first 100 tweets (or all tweets if fewer than 100)
# from the user timeline of @rdatammining
rdmTweets <- userTimeline("rdatamining", n=100)
n <- length(rdmTweets)
rdmTweets[1:3]

# search around a hashtag.
rdmTweets <- searchTwitter('$AAPL', n=500)

#Create a dataframe based around the results
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
#Here are the columns
names(df)
#And some example content
head(df,3)


## function for sentiment scoring 
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence, "[^[:alnum:]]", " ")

    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

## get sentiment words ##
hu.liu.pos = scan("positive-words.txt", what="character", comment.char=";")
hu.liu.neg = scan("negative-words.txt", what="character", comment.char=";")

## add a few of my own words
pos.words <- c(hu.liu.pos, "upgrade")
neg.words <- c(hu.liu.neg, "wtf", "fail", "lose")
```


```{r}
## now we search ##
aaplTw <- searchTwitter('$AAPL', n=500)

#Create a dataframe based around the results
df <- do.call("rbind", lapply(aaplTw, as.data.frame))
#Here are the columns
names(df)
#And some example content
head(df$text,3)


## score sentiment
aaplScore <- score.sentiment(df$text, pos.words, neg.words, .progress = "text")
aaplScore$stock <- "AAPL"
#aaplScore <- cbind(aaplScore, df)
head(aaplScore)


## let's try google ##
googTw <- searchTwitter('$GOOG', n=500)


#Create a dataframe based around the results
df <- do.call("rbind", lapply(googTw, as.data.frame))
#Here are the columns
names(df)
#And some example content
head(df$text,3)
df$text

googScore <- score.sentiment(df$text, pos.words, neg.words, .progress = "text")
googScore$stock <- "GOOG"
head(googScore)


## let's try yahoo
yhooTw <- searchTwitter('$YHOO', n=500)


#Create a dataframe based around the results
df <- do.call("rbind", lapply(yhooTw, as.data.frame))
#Here are the columns
names(df)
#And some example content
head(df$text,3)
df$text

yhooScore <- score.sentiment(df$text, pos.words, neg.words, .progress = "text")
yhooScore$stock <- "YHOO"
head(yhooScore)

all.scores <- rbind(aaplScore, googScore, yhooScore)
ggplot(all.scores) + 
     geom_bar(mapping = aes(x=score, fill = stock),binwidth = 1) + 
     facet_grid(stock~.) + 
     theme_bw() 


ggplot(all.scores, aes(x = score, fill = stock)) + 
     geom_histogram(binwidth = 1, aes(y = ..density..)) + 
     geom_density(adjust = 3, fill = NA) +
     facet_grid(stock~.) + 
     theme_bw() + 
     labs(x = "Sentiment Score for different companies") + 
     scale_fill_brewer()

ggsave("AAPL_GOOG_YAHOO.png")

```

![A_G_Y](https://github.com/callinSwitzer/RDataScience/blob/master/AAPL_GOOG_YAHOO.png)


## plot locations of tweets
```{r}
# Houston TX
aaplTw_H <- do.call("rbind", lapply(searchTwitter('$AAPL', n=500, geocode='29.708291,-95.425137,200mi'), as.data.frame))

aaplScore_H <- score.sentiment(aaplTw_H$text, pos.words, neg.words, .progress = "text")
aaplScore_H$Location <- "Houston"
aaplScore_H <- cbind(aaplScore_H, aaplTw_H)
head(aaplScore_H)


# SF
aaplTw_SF <- do.call("rbind", lapply(searchTwitter('$AAPL', n=500, geocode='37.847967,-122.097696,200mi'), as.data.frame))

aaplScore_SF <- score.sentiment(aaplTw_SF$text, pos.words, neg.words, .progress = "text")
aaplScore_SF$Location <- "San Francisco"
aaplScore_SF <- cbind(aaplScore_SF, aaplTw_SF)
head(aaplScore_SF)

all.scores2 <- rbind(aaplScore_H, aaplScore_SF)
ggplot(all.scores2) + 
     geom_bar(mapping = aes(x=score, fill = Location),binwidth = 1) + 
     facet_grid(Location~.) + 
     theme_bw() 

ggplot(all.scores2, aes(x = score, fill = Location)) + 
     geom_histogram(binwidth = 1, aes(y = ..density..)) + 
     geom_density(adjust = 3, fill = NA) +
     facet_grid(Location~.) + 
     theme_bw() + 
     labs(x = "Sentiment Score for $AAPL from different locations") + 
     scale_fill_brewer(palette="Dark2")

ggsave("SF_VS_Houston.png")


```

![SF_HOUS](https://github.com/callinSwitzer/RDataScience/blob/master/SF_VS_Houston.png)
