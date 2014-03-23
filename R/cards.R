suits <- c('Hearts', 'Spades', 'Diamonds', 'Clubs')
values <- c('Ace', '2', '3', '4', '5', '6', '7', '8', '9',
            '10', 'Jack', 'Queen', 'King')

deck <- data.frame(card = 1:52,
                   value = rep(values, 4),
                   suit = rep(suits, each = 13))




shuffle <- function(cards) {
  id <- sample(1:nrow(cards))
  return(cards[id, ])
}

draw <- function(deck, num = 1) {
  cards <- deck[1:(0 + num), ]
  deck <<- deck[(1 + num):nrow(deck), ]
  return(cards)
}

drawP <- function(deck, num = 1) {
  cards <- deck[1:(0 + num), ]
  return(cards)
}

flush <- function(cards) length(unique(cards$suit)) == 1

royalFlush <- function(cards) length(unique(cards$suit)) == 1 & 
  all(cards$value %in% c('Ace', 'King', 'Queen', 'Jack', '10'))

i <- 0
x <- 50000
for (j in seq(x)) {
  if (royalFlush(drawP(shuffle(deck), 5))) i <- i + 1
}
i/x

sum(unlist(lapply(1:50000, function(x) royalFlush(drawP(shuffle(deck), 5)))))

new_deck <- function() {
  data.frame(card = 1:52, value = rep(values, 4),
             suit = rep(suits, each = 13))[sample(1:52),]
}


deck <- shuffle(deck)
c <- draw(deck, 5)
nrow(deck)



deck <- data.frame(card = 1:52,
