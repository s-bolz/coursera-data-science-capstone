# Tasks to accomplish
#####################
#
#   1. Tokenization
#       identifying appropriate tokens such as words, punctuation, and numbers.
#       Writing a function that takes a file as input and returns a tokenized
#       version of it.
#
#   2. Profanity filtering
#       removing profanity and other words you do not want to predict.

library(tm)

file.blogs <- "data/final/en_US/en_US.blogs.txt"
file.news <- "data/final/en_US/en_US.news.txt"
file.tweets <- "data/final/en_US/en_US.twitter.txt"

# reads a text file line by line, removes all characters that don't match the
# group [ a-z0-9.,;!?:'"/()$€@£\t\n\r&%<>*_+~#-], and splits the text into a
# vector of words which is then returned
tokenize.file <- function(file, lan = "en", enc = "UTF-8", lines = -1) {
    raw <- readLines(con = file, n = lines, encoding = enc)
    raw <- tolower(raw)
    raw <- gsub("[^ a-z0-9.,;!?:'\"/()$€@£\t\n\r&%<>*_+~-]", "", raw)
    split <- unlist(strsplit(raw, "[ \r\n\t.,;:'\"()?!]"))
    return (
        split[split != ""]
    )
}

blogs <- tokenize.file(file.blogs)
news <- tokenize.file(file.news)
tweets <- tokenize.file(file.tweets)

counts <- table(c(blogs, news, tweets))

special.chars <- c("/", "\\$", "€", "@", "£", "&", "%", "<", ">", "\\*", "_", "\\+", "~", "-")

special.char.counts <- sapply(special.chars, function(char) { length(grep(char, names(counts))) })

sort(special.char.counts, decreasing = TRUE)

grep("[a-z]", grep("-", names(counts), value = TRUE), value = TRUE)
# of 207884 can further words be split off

grep("^[a-z]+-[a-z]+$", names(counts), value = TRUE)
# out of which 136988 are assembled words (e.g. beat-boxer)

x <- c("word", grep("^[a-z]+-[a-z]+$", names(counts), value = TRUE)[9997:10000], "term")
y <- gsub("-", " ", x)
unlist(strsplit(y, "[ \r\n\t.,;:'\"()?!]"))
n <- names(counts)
a <- grep("^[a-z]+-[a-z]+$", n, value = TRUE)
# replace "word-word" with "word word"
n[grep("^[a-z]+-[a-z]+$", n)] <- gsub("-", " ", n[grep("^[a-z]+-[a-z]+$", n)])
b <- grep("^[a-z]+ [a-z]+$", n, value = TRUE)


grep("/", names(counts), value = TRUE)

grep("\\*", names(counts), value = TRUE)

grep("^([0-9]+([.,][0-9]+)?\\$|\\$[0-9]+([.,][0-9]+)?)-([0-9]+([.,][0-9]+)?\\$|\\$[0-9]+([.,][0-9]+)?)$", names(counts), value = TRUE)
# 793 can be split into dollar ranges

grep("&", names(counts), value = TRUE)

grep("~", names(counts), value = TRUE)

grep("<", names(counts), value = TRUE)

grep("@", names(counts), value = TRUE)

grep("_", names(counts), value = TRUE)

grep(">", names(counts), value = TRUE)

grep("\\+", names(counts), value = TRUE)

grep("%", names(counts), value = TRUE)

grep("£", names(counts), value = TRUE)

grep("€", names(counts), value = TRUE)

filter.words <- function(x, pattern) {
    return (
        lapply(blog.sentences, function(x) {x[grep(pattern, x, invert = TRUE)]})
    )
}

# bad words list used by google in http://www.wdyl.com/
# taken from https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
bad.words <- readLines("data/profanity_words.txt")
filter.pattern <- paste("^(", paste(bad.words, collapse = "|"), ")$", sep = "")

# test with first three lines of blog posts
#(blog.sentences.filtered <- filter.words(blog.sentences, filter.pattern))
