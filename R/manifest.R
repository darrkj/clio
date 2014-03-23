library(plyr)

x <- 1
y <- 2
z <- 3

data <- cars

data$date <- Sys.Date() + sample(1:10, nrow(data), replace = TRUE)

data2 <- data[data$date > Sys.Date()+1, ]

data2$rand <- sample(c(x, y, z), nrow(data2), replace = TRUE)



info <- function(x) {
  UseMethod("info", x)
}


info.numeric <- function(x) {
  x
}

info.data.frame <- function(x) {
  y <- list()
  y$names <- names(x)
  y$head <- head(x)
  y$str <- str(x)
  y
}



manifest <- function(..., hist = F, file = stop("'file' must be specified")) {
  loc <- setdiff(ls(envir = .GlobalEnv), 'manifest')
  evl <- function(x) eval(parse(text = x))
  print('Comments: ')
  notes <- scan(what = character(), quiet = T)
  #summary <- lapply(loc, function(a) list(a, class(a), eval(parse(text = a))))
  manifest <- list(summary = ldply(loc, function(a) c(name = a, 
                                                      class = class(evl(a)), 
                                                      type = typeof(evl(a)), 
                                                      mode = mode(evl(a)), 
                                                      size = object.size(evl(a)))),
                   session = sessionInfo(),
                   time = Sys.time(),
                   #history = readLines('.RHistory'),
                   createdby = Sys.getenv()[['USER']],
                   notes = paste(notes, collapse = ' '))
  
  manifest$history <- if (hist) readLines('.RHistory') else NULL
  
  loc <- c(loc, 'manifest')
  #if(length(list(...)) == 0) {
    save(list = loc, file = file)
  #} else {
  #  save(list = ..., file = file)
  #}
}

manifest(file = 'x.RData')

manifest(x, y, file = 'x.RData')



