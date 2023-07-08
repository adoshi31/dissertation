###########################################
## Sentiment Analysis with R
## Analyzing news articles ################
###########################################

install.packages("tidyverse")
install.packages("tidytext")
library('tidyverse')
library('tidytext')
library('lubridate')
library(tidyverse)

# Read data with three fields: articleid, date, text
dataset = read.csv(file.choose(), stringsAsFactors=FALSE)

# Build document corpus
tidy_dtm = dataset %>% select(story.id,story,submitted) %>% 
  unnest_tokens("word", story)

# Estimate sentiment
senti_df = tidy_dtm %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(story.id, sentiment, submitted) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

senti_df$submitted <- mdy(senti_df$submitted) # from lubridate pacakge - note date syntax
#senti_df$date <- as.Date(senti_df$date)

# Sentiment range
summary(senti_df$sentiment)

# Simple plot of aggregate sentiment in document corpus
# Make sure to use zoom feature to see the entire plot distribution
plot(table(senti_df$sentiment))

ggplot(senti_df,aes(x=submitted, y=sentiment)) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%B %Y")



## Sentiment analysis using NRC lexicon for emotions
##############################################################

install.packages("syuzhet")
library(syuzhet)
library(tm)

## read in the same news file
text_df <- read.csv(file.choose(), header=TRUE, stringsAsFactors = FALSE, encoding = "iso-8859-1") 
Encoding(text_df$story) <- "latin1"
iconv(text_df$story, "latin1", "ASCII", sub="")
dashData <- as.character(text_df$story)

## see examples of NRC emotions categorization for two words
get_nrc_sentiment('happy')
get_nrc_sentiment('excitement')

sentimentMatrix <- get_nrc_sentiment(dashData)

dashSentiment <- cbind(text_df$story, sentimentMatrix)

## Note the barplot does not reflect sentiment / emotion over time, but rather 
## overall emotions reflected in the entire corpus of articles

barplot(colSums(dashSentiment[,-(1)]), col= rainbow(10), ylab= 'count', 
        main = "Emotions reflected in Harvard DASH user stories")

###############################################
### Control Group: WSJ Economic News articles##

## read in the same news file
text_df <- read.csv(file.choose(), header=TRUE, stringsAsFactors = FALSE, encoding = "iso-8859-1") 
Encoding(text_df$text) <- "latin1"
iconv(text_df$text, "latin1", "ASCII", sub="")
newsData <- as.character(text_df$text)

sentimentMatrix <- get_nrc_sentiment(newsData)

newsSentiment <- cbind(text_df$text, sentimentMatrix)

## Note the barplot does not reflect sentiment / emotion over time, but rather 
## overall emotions reflected in the entire corpus of articles

barplot(colSums(newsSentiment[,-(1)]), col= rainbow(10), ylab= 'count', 
        main = "Emotions reflected in news articles")

###################################
#### BIGRAMS / N-GRAMS OF DASH DATA
###################################


dash_bigrams <- dataset %>% 
  unnest_tokens(bigram, story, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  unite(bigram, word1, word2, sep=" ")

dash_bigrams

bigram_counts <- dash_bigrams %>% count(bigram, sort=TRUE)
bigram_counts
bigram_counts.df <- as.data.frame(bigram_counts)

####

bigram_counts$concatenated <- paste(bigram_counts$word1, bigram_counts$word2)

wordcloud(bigram_counts$concatenated, bigram_counts$n, 
          min.freq=5, random.order=FALSE)

### color the bigrams from highest to lowest frequency using color palette

bigram_counts %>% with(wordcloud(concatenated, n, 
                                   min.freq=5, 
                                   random.order=FALSE,
                                   colors=brewer.pal(8, "Dark2")))

### coloring different sentiments
sentiments <- get_sentiments("nrc")
sentiments %>% count(sentiment)

harvardDash <- dataset %>%
                unnest_tokens(word, story) %>%
                anti_join(stop_words)
harvardDash
  
dash_sentiments <- harvardDash %>% inner_join(sentiments)
dash_sentiments

## positive sentiments
dash_sentiments %>% 
  filter(sentiment=="positive") %>%
  count(word, sort=TRUE)      

## total numbers of sentiments (NRC lexicon)
dash_sentiments %>% count(sentiment)

## plot of sentiments in DASH data
dash_sentiments %>%
  count(sentiment) %>%
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(n, sentiment)) + geom_col() + labs(y=NULL)


dash_sentiments <- harvardDash %>% 
              inner_join(sentiments) %>% 
              count(word, sentiment, sort=TRUE) %>% 
              pivot_wider(names_from=sentiment, 
                    values_from=n, 
                    values_fill=0)

## We could now draw word clouds for specific sentiments by 
## picking out specific columns, 
##  e.g. here are all of the common words associated with “joy”, "trust", etc.

dash_sentiments %>% with(wordcloud(word, joy, 
                                   min.freq=10,
                                   random.color = FALSE,
                                   colors = brewer.pal(8, "PuOr")))

dash_sentiments %>% with(wordcloud(word, trust, 
                                       min.freq=20,
                                       random.color = FALSE,
                                       colors = brewer.pal(8, "PuOr")))

dash_sentiments %>% with(wordcloud(word, fear, 
                                   min.freq=10,
                                   random.color = FALSE,
                                   colors = brewer.pal(8, "PuOr")))

dash_sentiments %>% with(wordcloud(word, anticipation, 
                                   min.freq=10,
                                   random.color = FALSE,
                                   colors = brewer.pal(8, "PuOr")))

dash_sentiments %>% with(wordcloud(word, anger, 
                                   min.freq=5,
                                   random.color = FALSE,
                                   colors = brewer.pal(8, "PuOr")))

### TRI-GRAMS

dash_trigrams <- dataset %>% 
  unnest_tokens(trigram, story, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  filter(!is.na(word3)) %>% 
  unite(trigram, word1, word2, word3, sep=" ")

dash_trigrams

trigram_counts <- dash_trigrams %>% count(trigram, sort=TRUE)
trigram_counts

### ### ### ### ### ### ### ### ### ### ### ### 
###### ### ###  QUAD-GRAMS ### ### ### ### ### 

dash_quadgrams <- dataset %>% 
  unnest_tokens(quadgram, story, token = "ngrams", n = 4) %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  filter(!is.na(word3)) %>%
  filter(!is.na(word4)) %>% 
  unite(quadgram, word1, word2, word3, word4, sep=" ")

dash_quadgrams

quadgram_counts <- dash_quadgrams %>% count(quadgram, sort=TRUE)
quadgram_counts

############################################################
### Counting bigrams is a very good way of finding 
## names of important concepts in a text, 
## plus commonly used described items (e.g. “free access”). 
## This is because there is a high degree of correlation between the 
## number of times the two words in a name or common description appear.
## We could examine this in more detail. For example, we know that "free"
## is an important characteristic of the material situation of life. 
## We could find these by filtering on word2, using a 
## regular expression to look for free;
#########################################################

dash_free <- dataset %>%
  unnest_tokens(bigram, story, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  filter(str_detect(word1, "free")) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  count(bigram, sort=TRUE)

dash_free

dash_open <- dataset %>%
  unnest_tokens(bigram, story, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  filter(str_detect(word1, "open")) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  count(bigram, sort=TRUE)

dash_open

##### VISUALIZING

bigram_counts <- dataset %>% 
  unnest_tokens(bigram, story, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  count(word1, word2, sort=TRUE)

bigram_counts

library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n >= 7) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(2, "mm")), 
                 end_cap = circle(1, "mm")) +
  geom_node_point(color = "lightblue", size = 2) +
  geom_node_text(aes(label = name), size = 2) +
  theme_void()

####The arrows show the direction of the correlation, 
## with the weights of the arrows showing the number of 
## times each bigram was used. We can clearly see that 
## “student” and "access" are heavily-used bigrams. 
## But we can also get more information about surrounding words, 
## e.g. “free digital access”, “nursing student” 
## and “providing access” were also common phrases. 
## This has also surfaced 
## extra information, e.g. "family history"
####

bigram_graph <- bigram_counts %>%
  filter(n >= 3) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(1, "mm")), 
                 end_cap = circle(0.5, "mm")) +
  geom_node_point(color = "lightblue", size = 0.5) +
  geom_node_text(aes(label = name), size = 1) +
  theme_void()

