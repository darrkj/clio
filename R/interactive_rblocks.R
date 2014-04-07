
library(rblocks)
library(rCharts)


l <- 25

block = make_block(l,l, type = 'matrix')

for (i in 1:l) {
  dev.hold()
  block[i, i] = 'red'
  display(block)
  ani.pause()
}



grid7 <- block_grid(5, 5, type = "data.frame")
grid7[1] <- "dodgerblue3"
grid7


grid8 = apply(grid7, 2, function(df) {
  if (sum(df == "dodgerblue3") == 5) {
    df[] = "darkslategrey"
  } else {
    df[] = "olivedrab"
  }
  return(df)
})
display(grid8)
display_d3(grid8)











