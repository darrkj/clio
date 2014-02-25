hist <- readLines('exHist')

a <- grep('<-', hist, value = T)



b <- grep('res', xx, value = T)
intersect(a, b)