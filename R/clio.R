hist <- readLines('exHist')

# Does not account for for loops
a <- grep('<-', hist, value = T)

b <- strsplit(a, '<-')

name <- lapply(b, `[`, 1)
val <- lapply(b, `[`, 2)

# Need better cleaning of leading and trailing spaces
name <- substr(name, 1, nchar(name)-1)
val  <- substr(val, 2, nchar(val))

idx <- grep('res', name)
val[idx]




origin <- function(var, all = F) {
  x <- which(var == name)
  y <- grep(var, val, invert = T)
  if (all) val[intersect(x, y)] else tail(val[intersect(x, y)], 1)
}


b <- grep('res', xx, value = T)
intersect(a, b)