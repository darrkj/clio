height <- 10
width <- 10

maze <- matrix(0, height, width)

sides <- sample(1:4, 2, replace = FALSE)

maze[, 1] <- 1
maze[, width] <- 1
maze[1, ] <- 1
maze[height, ] <- 1

point <- function(x) {
  y <- if      (x == 1) c(sample(seq(height), 1), 1)
       else if (x == 2) c(1, sample(seq(width), 1))
       else if (x == 3) c(sample(seq(width), 1), height)
       else             c(width, sample(seq(width), 1))
  list(x=y[1], y=y[2])
}


start <- point(sides[1])

finish <- point(sides[2])

# if(abs(start$y - finish) < 2)
start$x <- 1
start$y <- 1
finish$x <- height
finish$y <- width

maze[start$x, start$y] <- 1

maze[finish$x, finish$y] <- 1




maze
