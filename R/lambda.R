# Functional Programming in lambda.r


library(lambda.r)


fib(0) %as% 1
fib(1) %as% 1
fib(n) %when% {n > 1} %as% { fib(n-1) + fib(n-2) }

Point(x,y) %as% list(x=x,y=y)
point.1 <- Point(2,3)
point.2 <- Point(5,7)

distance (a, b) %::% Point : Point : numeric
distance(a,b) %as% { ((a$x - b$x)^2 + (a$y - b$y)^2)^.5 }

distance(point.1, point.2)
