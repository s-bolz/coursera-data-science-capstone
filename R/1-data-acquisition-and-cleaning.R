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
    # (some regexes based upon http://www.regular-expressions.info/)
    # unify all URL's
    raw <- gsub("(https?|ftp)://[^ ]+", " URL ", raw)
    # unify all email addresses
    raw <- gsub("[^ ]+@[^ ]+\\.[^ \r\n\t.,;:'\"()?!]{2,}", " EMAIL ", raw)
    # unify all twitter user
    raw <- gsub("@[a-z0-9_]+", " TWITTERUSER ", raw)
    # replace all remaining @ characters with "at"
    raw <- gsub("@", " at ", raw)
    # unify all dates in format yyyy-[m]m-[d]d or [d]d-[m]m-yyyy
    raw <- gsub (
      paste (
        "^((19|20)[0-9]{2}[- /.](0?[1-9]|1[012])[- /.](0?[1-9]|[12][0-9]|3[01])|",
        "(0?[1-9]|[12][0-9]|3[01])[- /.](0?[1-9]|1[012])[- /.](19|20)[0-9]{2})$",
        sep = ""
      ),
      " DATE ",
      raw
    )
    # unify all tags
    raw <- gsub("</*[a-z]>", " TAG ", raw)
    # replace all "$##" with "## dollars"
    raw <- gsub("\\$([0-9.,]+)", " \\1 dollars ", raw)
    # replace all "##%" and "%##" with "## percent"
    raw <- gsub("([0-9.,]+)%", " \\1 percent ", raw)
    raw <- gsub("%([0-9.,]+)", " \\1 percent ", raw)
    # replace all remaining currency characters with currency string
    raw <- gsub("\\$", " dollars ", raw)
    raw <- gsub("€", " euros ", raw)
    raw <- gsub("£", " pounds ", raw)
    # split words compounded with a "+"
    raw <- gsub("([a-z]+)\\+([a-z]+)", " \\1 \\2 ", raw)
    # replace multiple "+" characters with a space
    raw <- gsub("\\+\\++", " ", raw)
    # replace remaining "+" characters with "plus" string
    raw <- gsub("\\+", " plus ", raw)
    # replace several characters with a space so that compound words are split
    raw <- gsub("(-|/|&|<|>|%|_)", " ", raw)
    # unify all numbers
    raw <- gsub("[0-9.,]+", " NUMBER ", raw)
    # delete all rare special characters that might complicate our dictionary
    raw <- gsub("[^ a-zA-Z0-9.,;!?:'\"/()$€@£\t\n\r&%<>_+-]", "", raw)
    split <- unlist(strsplit(raw, "[ \r\n\t.,;:'\"()?!]"))
    return (
        split[split != ""]
    )
}

blogs <- tokenize.file(file.blogs)
news <- tokenize.file(file.news)
tweets <- tokenize.file(file.tweets)
all <- c(blogs, news, tweets)
rm(blogs, news, tweets)

counts <- table(all)

counts[c("EMAIL", "URL", "DATE", "TAG", "TWITTERUSER", "NUMBER")]

# bad words list used by google in http://www.wdyl.com/
# taken from https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
bad.words <- readLines("data/profanity_words.txt")
filter.pattern <- paste("^(", paste(bad.words, collapse = "|"), ")$", sep = "")

# merge lists of english words taken from
#   - http://www-01.sil.org/linguistics/wordlists/english/
#   - http://www-personal.umich.edu/~jlawler/wordlist
dict.en <- c (
    readLines("data/wordsEn.txt", encoding = "us-ascii"),
    readLines("data/wordlist", encoding = "binary")
)
dict.en <- unique(c(dict.en, grep("[A-Z]", names(counts), value = TRUE)))
dict.en <- unique(gsub("\\(.*\\)", "", dict.en))
dict.en <- dict.en[-grep("[^a-zA-Z'.-]", dict.en)]
dict.en <- unique(unlist(strsplit(dict.en, "-")))

table(names(counts) %in% dict.en)
sum(counts[names(counts) %in% dict.en])
sum(counts[!names(counts) %in% dict.en])

sort(counts[!names(counts) %in% dict.en], decreasing = TRUE)[1:100]

# all words that are in dictionary remain the same
words.in.dict <- all[all %in% dict.en]
words.not.in.dict <- all[!all %in% dict.en]

# all words that are in dictionary with removal of .' are changed to dictionary value
mappings <- lapply (
    unique(words.not.in.dict[words.not.in.dict %in% gsub("[.']", "", dict.en)]),
    function(w) {
        c(w, dict.en[grep(paste("^", w, "$", sep = ""), gsub("[.']", "", dict.en))])
    }
)
mappings.not.in.dict <- lapply (
    mappings,
    function(m) {
        i <- grep(paste("^", m[1], "$", sep = ""), words.not.in.dict)
        data.frame(index = i, replacement = rep("november's", length(i)))
    }
)
mappings.not.in.dict
length(words.not.in.dict)

# all words with removed trailing s that are in dictionary are changed to 's
# check all remaining words that are not in dictionary

# removes all words from vector that match a given pattern
filter.words <- function(x, pattern) {
    return (
        lapply(blog.sentences, function(x) {x[grep(pattern, x, invert = TRUE)]})
    )
}

###############################################################################

library(XML)

# split the large files into smaller chunks manageable by Stanford's CoreNLP
system("split -l 50000 data/final/en_US/en_US.blogs.txt blogs-part-")
system("split -l 50000 data/final/en_US/en_US.news.txt news-part-")
system("split -l 100000 data/final/en_US/en_US.twitter.txt tweets-part-")
system("mv *-part-* data/final/en_US/")

get.xml.as.data.frame <- function(file) {
    xml <- xmlTreeParse(file, useInternalNodes = TRUE)
    sentences <- numeric()
    tokens <- numeric()
    words <- character()
    sentence.ids <- as.numeric (
        xpathSApply(xml, "//sentence/@id")
    )
    for( sid in sentence.ids ) {
        token.ids <- as.numeric (
            xpathSApply (
                xml,
                paste("//sentence[@id='", sid, "']/tokens/token/@id", sep = "")
            )
        )
        for( tid in token.ids ) {
            word <- xpathSApply (
                xml,
                paste (
                    "string(//sentence[@id='", sid,
                    "']/tokens/token[@id='", tid,
                    "']/word/text())",
                    sep = ""
                )
            )
            words <- c(words, word)
        }
        sentences <- c(sentences, rep(sid, length(token.ids)))
        tokens <- c(tokens, token.ids)
    }
    data.frame (
        sentence.id = sentences,
        token.id = tokens,
        word = words,
        file = rep(file, length(words))
    )
}

cp <- paste("lib", paste(dir("lib/"), collapse = ":lib/"), sep = "/")

data <- data.frame (
    sentence.id = numeric(),
    token.id = numeric(),
    word = character(),
    file = character(),
    stringsAsFactors = FALSE
)

for( file in grep("-part-[a-z]{2}$", dir("data/final/en_US/"), value = TRUE) ) {
    system (
        paste (
            "java",
            " -cp ", cp,
            " -Xmx2g",
            " edu.stanford.nlp.pipeline.StanfordCoreNLP",
            " -props conf/CoreNLP.properties",
            " -file data/final/en_US/",
            file,
            sep = ""
        )
    )
    print(paste("tagged file", file))
    data <- rbind (
        data,
        get.xml.as.data.frame(paste("data/final/en_US/", file, ".xml", sep = ""))
    )
    print(paste("loaded file", file))
}

df <- get.xml.as.data.frame("data/final/en_US/tweets-part-bv.xml")

head(df, n = 20)
readLines("data/final/en_US/tweets-part-bv", 1)
