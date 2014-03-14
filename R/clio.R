
# Read in the history file from some of the hoops analysis.
hist <- readLines('exHist')

last <- function(str) substr(str, nchar(str), nchar(str))
first <- function(str) substr(str, 1, 1)

# Does not account for for loops

# Break the code up into assignments
assignments <- grep('<-', hist, value = TRUE)
assignments <- grep("'<-'", assignments, value = TRUE, invert = TRUE)
assignments <- grep('"<-"', assignments, value = TRUE, invert = TRUE)
assignments <- strsplit(assignments, '<-')

name <- lapply(assignments, `[`, 1)
value <- lapply(assignments, `[`, 2)

history <- list()
history$code <- hist


# Need better cleaning of leading and trailing spaces
name <- ifelse(last(name) == ' ', substr(name, 1, nchar(name)-1), name)
value <- ifelse(first(value) == ' ', substr(value, 2, nchar(value)), value)

history$name <- name
history$value <- value

rm(name, value, hist)

idx <- grep('res', name)
val[idx]

peel <- function(exp) {
  a <- gregexpr('(', exp, fixed = T)[[1]][1]
  b <- tail(gregexpr(')', exp, fixed = T)[[1]], 1)
  if (a == -1 & b == -1) {
    'Nothing to peel'
  } else {
    list(inner = substr(exp, a+1, b-1), func = substr(exp, 1, a-1))
  }
}


origin <- function(var, all = F) {
  x <- which(var == name)
  y <- grep(var, val, invert = T)
  if (all) val[intersect(x, y)] else tail(val[intersect(x, y)], 1)
}


aa <- origin('res')
origin('details')
origin('detail')


# many others here but being exhaustive will be difficult. 
sources <- c('read.', 'readHTMLTable', 'GET')


b <- grep('res', xx, value = T)
intersect(a, b)