#install.packages("tm")
#install.packages('topicmodels')
#install.packages('slam')

library('stringr')
library('tm')
library('topicmodels')
library('slam')

# Read data 
dataset = read.csv(file.choose(), stringsAsFactors=FALSE)
dataset2 <- dataset[-c(153, 1120, 1562, 1699, 1749, 2274, 2496, 2851),]

# word count

foo <- str_split(dataset$story, " ") # split each element of your vector by the space sign
sapply(foo,length) # just a quick test: how many words has each element?
sum(sapply(foo,length))/length(foo) # calculate sum and divide it by the length of your original object
median(sapply(foo, length)) # median word length in DASH dataset
max(sapply(foo, length)) # maximum word length
min(sapply(foo, length)) # minimum word length

hist(sapply(foo, length),   ## Attempt at a histogram 
     main="Number of words in DASH Stories",
     xlab="Total words",
     xlim=c(10,200),
     ylab="DASH Stories",
     ylim = c(0,3500),
     col="darkmagenta",
     freq=FALSE
)

# Build corpus
corpus = Corpus(VectorSource(enc2utf8(dataset2$story)))

# Pre-processing
corpus = tm_map(corpus,  content_transformer(tolower)) # lower case
replacePunctuation = function(x) {return (gsub("[[:punct:]]"," ", x))}
corpus = tm_map(corpus, content_transformer(replacePunctuation)) # remove punctuations
corpus = tm_map(corpus, removeWords, stopwords("english")) # remove stop words
corpus = tm_map(corpus, content_transformer(stripWhitespace)) # remove whitespaces

max(nchar(corpus))
min(nchar(corpus))

# Get document-term matrix from processed texts
dtm = DocumentTermMatrix(corpus) 

# Calculate TFIDF score for each word 
dtm2 = DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf)) 
term_tfidf = col_means(dtm2)

# Remove words whose TFIDF score is below average
dtm = dtm[,term_tfidf>=mean(term_tfidf)]

# Fit LDA model
topic_model=LDA(dtm, k=5, control = list(seed = 123))
topic_model2 = LDA(dtm, k=7, control = list(seed = 123))
topic_model3 = LDA(dtm, k=3, control = list(seed = 123))
topic_model4 = LDA(dtm, k=4, control = list(seed = 123))


# Extract top 10 words in each topic
words_in_topic = terms(topic_model, 20)
words_in_topic

words_in_topic2 = terms(topic_model2, 15)
words_in_topic2
broom::glance(topic_model2)

words_in_topic3 = terms(topic_model3, 25)
words_in_topic3
broom::glance(topic_model3)

words_in_topic4 = terms(topic_model4, 40)
words_in_topic4
Xbroom::glance(topic_model4)
