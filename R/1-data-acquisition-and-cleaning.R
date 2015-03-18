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

file.blogs <- "data/final/en_US/en_US.blogs.txt"
file.news <- "data/final/en_US/en_US.news.txt"
file.tweets <- "data/final/en_US/en_US.twitter.txt"

# reads a text file line by line, removes all characters that don't match the
# group [ a-z0-9.,;!?:'"/()$€@£\t\n\r&%<>*_+~#-], and splits the text into a
# vector of words which is then returned
tokenize.file <- function(file, lan = "en", enc = "UTF-8", lines = -1) {
    raw <- readLines(con = file, n = lines, encoding = enc)
    raw <- tolower(raw)
    # unify several common non-word-entities
    # (based upon http://www.regular-expressions.info/)
    # unify all URL's
    raw <- gsub("(https?|ftp)://[^ ]+", "URL", raw)
    # unify all email addresses
    raw <- gsub("[^ ]+@[^ ]+\\.[^ \r\n\t.,;:'\"()?!]{2,}", "EMAIL", raw)
    # unify all dates in format yyyy-[m]m-[d]d or [d]d-[m]m-yyyy
    raw <- gsub (
      paste (
        "^((19|20)[0-9]{2}[- /.](0?[1-9]|1[012])[- /.](0?[1-9]|[12][0-9]|3[01])|",
        "(0?[1-9]|[12][0-9]|3[01])[- /.](0?[1-9]|1[012])[- /.](19|20)[0-9]{2})$",
        sep = ""
      ),
      "DATE",
      raw
    )
    # replace all "$##" with "## dollars"
    raw <- gsub("\\$([0-9]+)", " \\1 dollars", raw)
    # replace all remaining currency characters with currency string
    raw <- gsub("\\$", " dollars ", raw)
    raw <- gsub("€", " euros ", raw)
    raw <- gsub("£", " pounds ", raw)
    # replace all dashes, slashes, and &'s with a space so that compound words are split
    raw <- gsub("(-|/|&)", " ", raw)
    # delete all rare special characters that might complicate our dictionary
    raw <- gsub("[^ a-zA-Z0-9.,;!?:'\"/()$€@£\t\n\r&%<>_+~-]", "", raw)
    split <- unlist(strsplit(raw, "[ \r\n\t.,;:'\"()?!]"))
    return (
        split[split != ""]
    )
}

blogs <- tokenize.file(file.blogs)
news <- tokenize.file(file.news)
tweets <- tokenize.file(file.tweets)

counts <- table(c(blogs, news, tweets))

counts[c("EMAIL", "URL", "DATE")]

special.chars <- c("@", "%", "<", ">", "_", "\\+", "~")

special.char.counts <- sapply(special.chars, function(char) { i <- grep(char, names(counts)) ; c(length(i), sum(counts[i])) })

special.char.counts

grep("@", names(counts), value = TRUE)

grep("%", names(counts), value = TRUE)

grep("<", names(counts), value = TRUE)

grep(">", names(counts), value = TRUE)

grep("_", names(counts), value = TRUE)

grep("\\+", names(counts), value = TRUE)

grep("~", names(counts), value = TRUE)

# TODO number handling (numbers might already be split due to punctuation)
# TODO language handling (words might already be split due to non-a-z characters)

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
