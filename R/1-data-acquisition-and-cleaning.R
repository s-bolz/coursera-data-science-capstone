#readLines("../data/final/en_US/en_US.news.txt", 10)
#readLines("../data/final/en_US/en_US.twitter.txt", 10)

tokenize.file <- function(file, lines = -1) {
  lines <- tail(readLines(file, lines), 1)
  lines <- gsub("(\"|\u201c|\u201d)", "", lines)
  lines <- gsub("([,.:!?])", " \\1 ", lines)
  print(lines)
  strsplit(lines, "[[:blank:]]+")
}

tokenize.file("../data/final/en_US/en_US.blogs.txt", 3)

x <- readLines("../data/final/en_US/en_US.blogs.txt", 1)
gsub("\u201c|\u201d", "", x)

readLines("../data/final/en_US/en_US.blogs.txt", 3)
