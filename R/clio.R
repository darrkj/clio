hist <- readLines('/Users/kdarrell/Desktop/r/clio/exHist')

a <- grep('<-', hist, value = T)



b <- grep('res', xx, value = T)
intersect(a, b)