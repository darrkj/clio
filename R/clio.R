
# Read in the history file from some of the hoops analysis.
#hist <- readLines('R/exHist')


read.hist <- function() {
  hist <- readLines('.Rhistory')

  # Comments id
  comments_index <- gregexpr('^#', hist, perl = T) == 1
  comments <- hist[comments_index]
  hist <- hist[!comments_index]

  # Does not account for for loops

  # Break the code up into assignments
  assignments <- grep('<-', hist, value = TRUE)
  assignments <- grep("'<-'", assignments, value = TRUE, invert = TRUE)
  assignments <- grep('"<-"', assignments, value = TRUE, invert = TRUE)
  assignments <- strsplit(assignments, '<-')

  history <- list(code = hist, comments = comments, 
                  name = lapply(assignments, `[`, 1),
                  value = lapply(assignments, `[`, 2))
  
  # Need better cleaning of leading and trailing spaces
  last <- function(str) substr(str, nchar(str), nchar(str))
  first <- function(str) substr(str, 1, 1)
  
  history$name <- ifelse(last(history$name) == ' ', 
                         substr(history$name, 1, nchar(history$name) - 1), 
                         history$name)
  history$value <- ifelse(first(history$value) == ' ', 
                          substr(history$value, 2, nchar(history$value)), 
                          history$value)
  
  history$name <- unlist(history$name)
  history$value <- unlist(history$value)
  history
}

history <- read.hist()


special <- c('for', 'while', 'if', 'else if', 'else')


differ <- function(string) {
  x <- gregexpr('{', string, fixed = T)[[1]]
  y <- gregexpr('}', string, fixed = T)[[1]]
  x <- if (x == -1) 0 else length(x)
  y <- if (y == -1) 0 else length(y)
  x - y
} 

starts <- intersect(grep('^for', history$code), grep('{$', history$code, perl = T))
ends <- c()

for (i in starts) {
  complete <- 1
  j <- i + 1
  while (complete > 0 & j < length(hist)) {
    j <- j + 1
    complete <- complete + differ(history$code[j])
  }
  ends <- c(ends, j)
}


gap_filler <- function(s, e) {
  stopifnot(length(s) == length(e))
  r <- c()
  for (i in seq(s)) {
    r <- c(r, seq(s[i], e[i]))
  }
  r
}




idx <- grep('data2', history$name)
history$value[idx]

peel <- function(exp) {
  a <- gregexpr('(', exp, fixed = T)[[1]][1]
  b <- tail(gregexpr(')', exp, fixed = T)[[1]], 1)
  if (a == -1 & b == -1) {
    'Nothing to peel'
  } else {
    list(inner = substr(exp, a+1, b-1), func = substr(exp, 1, a-1))
  }
}

#   y <- grep(var, history$value, invert = T)
#   if (all) {
#     history$value[intersect(x, y)] 
#   } else { 
#     tail(history$value[intersect(x, y)], 1)
#   }


origin <- function(var, all = F) {
  name <- lapply(strsplit(history$name, split = '$', fixed = T), `[`, 1)
  x <- which(var == name)
  if (all) {
    unique(paste(history$name[x], '<-', history$value[x]))
  } else { 
    head(paste(history$name[x], '<-', history$value[x]), 1)
  }
}


origin('myData2')
origin('myData2', T)
origin('myData')
origin('myData', T)

all.names(parse(text = origin('data2')))
all.vars(parse(text = origin('data2')))


condense <- function(str) {
  # Just take the first and call recursively.
  elem <- gregexpr('$', str, fixed = T)[[1]][1]
  if (elem > 0) {
    chars <- strsplit(str, NULL)[[1]]
    ind <- chars %in% c(letters, LETTERS, as.character(0:9))
    # The plus one is to offset the dollar sign
    ind[1:elem] <- TRUE
    str <- paste(chars[c(1:(elem-1), min(which(!ind)):nchar(str))], collapse = '')
  }
  str
}


depends <- function(str) {
  x <- all.names(parse(text = condense(origin(str))), unique = TRUE)
  x <- setdiff(x, str)
  x <- x[!unlist(lapply(x, exists))]
  if (length(x) > 0) c(x, depends(x)) else x
}

depends(condense(origin('myData2')))



syms <- c('<-', '>', '<', '-', '+', '/', '*', '[', '[[')


origin('details')
origin('detail')


# many others here but being exhaustive will be difficult. 
sources <- c('read.', 'readHTMLTable', 'GET')


b <- grep('res', xx, value = T)
intersect(a, b)
