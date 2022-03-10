
library(rtweet)
library(twitteR)
library(ROAuth)
library(dplyr)
library(tidytext)
library(tidyr)
require(twitteR)
require(ROAuth)
#require(RCurl)

library(stringr)
library(plyr)
library(purrr)
library(ggplot2)
library(wordcloud)
library(httr)
library(rtweet) 
#library(sentiment)
library(syuzhet)
library(openssl)
library(httpuv)
library(ROAuth)
library(base64enc)
library(plyr)
#twitter authentication

auth_setup_default()
## install remotes package if it's not already
#retrieving data about keyword Iphone
rt <- search_tweets("#iPhone 13 ", n = 100,include_rts = FALSE , lang="en")
#display no of tweets using length function
length.rt <- length(rt)
length.rt

rt2 <-search_tweets("#SamsungS22Ultra", n = 100,include_rts = FALSE, lang="en") 

#process each set of tweets into a tidy text or corpus objects
tweets.apple <- rt %>% select(screen_name,text)
tweets.samsung <- rt2 %>% select(screen_name,text)

#pre processing text transformations

head(tweets.apple$text)
head(tweets.samsung$text)

#cleaning data

# removing html links in tweets
tweets.apple$stripped_text1 <- gsub("http\\s+","",tweets.apple$text)
tweets.samsung$stripped_text2 <- gsub("http\\s+","",tweets.samsung$text)

# removing punctuations from list of words
tweets.apple_stem <- tweets.apple %>% select(stripped_text1) %>% unnest_tokens(word,stripped_text1)
tweets.samsung_stem <- tweets.samsung %>% select(stripped_text2) %>% unnest_tokens(word,stripped_text2)

head(tweets.apple_stem)
head(tweets.samsung_stem)

# remove stop words from stemmed word list

cleaned_tweets.apple <- tweets.apple_stem %>% anti_join(stop_words)
cleaned_tweets.samsung <- tweets.samsung_stem %>% anti_join(stop_words)

head(cleaned_tweets.apple)
head(cleaned_tweets.samsung)

# find out most commonly used words about apple iphone 13 and sanmsung ultra max 22
cleaned_tweets.apple %>% count(word,sort=TRUE)%>% top_n(10) %>% mutate(word=reorder(word,n)) %>% ggplot(aes(x=word,y=n)) + geom_col()+ coord_flip()+theme_light()+labs(x="Count", y="Unique Words",title="Unique Word Count found in Iphone tweets")
cleaned_tweets.samsung %>% count(word,sort=TRUE)%>% top_n(10) %>% mutate(word=reorder(word,n)) %>% ggplot(aes(x=word,y=n)) + geom_col()+ coord_flip()+theme_light()+labs(x="Count", y="Unique Words",title="Unique Word Count found in Samsung tweets")

# perform sentiment analaysis of tweets

#using bing 

install.packages("textdata")
library(textdata)
get_sentiments("bing") %>% filter(sentiment=="positive")
get_sentiments("bing") %>% filter(sentiment=="negative")

bing_productApple <- cleaned_tweets.apple %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort = TRUE) %>% ungroup()
bing_productApple

bing_productSamsung <- cleaned_tweets.samsung%>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort = TRUE) %>% ungroup()
bing_productSamsung





# using afinn
get_sentiments("afinn") %>% filter(value=="3")
get_sentiments("afinn") %>% filter(value=="3")


#vizualize the data

bing_productApple %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word=reorder(word,n)) %>% ggplot(aes(word,n,fill=sentiment))+ geom_col(show.legend = FALSE)+ facet_wrap(~sentiment,scales = "free_y") + labs(title = "Tweets containing Apple",y="contribution to sentiment",x=NULL)+ coord_flip() +theme_light()

bing_productSamsung %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word=reorder(word,n)) %>% ggplot(aes(word,n,fill=sentiment))+ geom_col(show.legend = FALSE)+ facet_wrap(~sentiment,scales = "free_y") + labs(title = "Tweets containing Samsung",y="contribution to sentiment",x=NULL)+ coord_flip() +theme_light()

#using nrc
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
# nrc emotion analysis
cleaned_tweets.apple %>% inner_join(nrc_joy) %>%count(word, sort = TRUE)
cleaned_tweets.samsung %>% inner_join(nrc_joy) %>%count(word, sort = TRUE)

# calculate score upon positive and negative words.
typeof(cleaned_tweets.apple)

score.sentiment <- function(sentences,pos.words,neg.words,.progress='none')
{
  require(plyr)
  require(stringr)
  scores <-laply(sentences,function(sentence,pos.words,neg.words){
    #remove punctuation are replace with ""
    sentence <-gsub('[[:punct:]]',"",sentence)
    #remove control space and replace with ""
    sentence <-gsub('[[:cntrl:]]',"",sentence)
    #remove digits and replace with ""
    sentence <-gsub('\\d+',"",sentence)
    # convert into lowercase
    sentence <-tolower(sentence)
    word.list <-str_split(sentence,'\\s+')
    words <-unlist(word.list)
    pos.matches <-match(words,pos.words)
    neg.matches <-match(words,neg.words)
    pos.matches<-!is.na(pos.matches)
    neg.matches <-!is.na(neg.matches)
    score <-sum(pos.matches)-sum(neg.matches)
    return(score)
  },pos.words,neg.words,.progress = .progress)
  scores.df <-data.frame(score=scores,text=sentences)
  return(scores.df)
}

# positive words and negative words 
pos.words <-scan('D:/twitter/positive_words.txt',what='character',comment.char=";")
neg.words <-scan('D:/twitter/negative_words.txt',what='character',comment.char=";")


apple_score<- score.sentiment(tweets.apple$text,pos.words,neg.words ,.progress = 'text')
samsung_score<- score.sentiment(tweets.samsung$text,pos.words,neg.words ,.progress = 'text')
hist(apple_score$score)
hist(samsung_score$score)
