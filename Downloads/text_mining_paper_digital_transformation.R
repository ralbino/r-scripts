# install.packages("ape", type = "binary")
require("tm")
require("stm")
require("quanteda")
require("readtext")
require("wordcloud")
require("ape")
require("ggdendro")

# Set file path 
setwd("/Users/raphaelalbino/Downloads/")

# Reading digital transformation definitions from csv file
dt_definitions <- readr::read_csv("dt_definitions.csv")

# Transforming all definitions in lower case
dt_definitions$text <- tolower(dt_definitions$text)
dt_definitions$doc_id <- dt_definitions$author 

# Removing words like digital, transformation and new
wordList <- c("new","digital","transformation")
dt_definitions$text = removeWords(dt_definitions$text,wordList)     

# Transforming senteces into documments
dt_corpus <- corpus(dt_definitions) 

# Summary of main statics
dt_corpus.stats <- summary(dt_corpus)

# Transforming data for futher analysis
token <-
  tokens(
    dt_corpus,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_hyphens = TRUE,
    include_docvars = TRUE
  )

# Creating data frequency matrix
dt.dfm <- dfm(token,
             tolower = TRUE,
             stem = TRUE,
             remove = stopwords("english")
)

# Printing the results of feature observations inside the senteces.
y<- head(dfm_sort(dt.dfm, decreasing = TRUE, margin = "both"),
     n = 20,
     nf = 10)


# Creating a word cloud based on the founded digital transformation definitions
docs <- Corpus(VectorSource(dt_definitions$text))

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("and", "that","ways"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# similarity analysis
tstat_dist <- as.dist(textstat_dist(dt.dfm))
clust <- hclust(tstat_dist)
plot(clust, main="Dendrogram of Digital Transformation definitions", ylab = "Distance", xlab="Author")