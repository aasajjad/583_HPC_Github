set.seed(123)
Rprof(interval = 0.005)
system.time({
  ntrials <- 150000
  win.ct <- numeric(ntrials)
  start.cards <- rep(1:25, 2)
  
  for (trial in 1:ntrials) {
    cards <- sample(start.cards)
    rd.ct <- 0
    seen <- c()
    
    while (length(cards) > 0) {
      rd.ct <- rd.ct + 1
      
      if (rd.ct == 1) {
        flipped <- sample(1:length(cards), size = 2, replace = FALSE)
        
        if (cards[flipped[1]] == cards[flipped[2]]) {
          cards <- cards[-flipped]
        } else {
          seen <- cards[flipped]
          cards <- cards[-flipped]
        }
      } else {
        flipped <- sample(1:length(cards), size = 1)
        value <- cards[flipped]
        
        if (value %in% seen) {
          seen <- seen[-which(seen == value)]
          cards <- cards[-flipped]
        } else {
          cards <- cards[-flipped]
          if (length(cards) > 0) {
            flipped2 <- sample(1:length(cards), size = 1)
            value2 <- cards[flipped2]
            
            if (value == value2) {
              cards <- cards[-flipped2]
            } else {
              seen <- c(seen, value, value2)
              cards <- cards[-flipped2]
            }
          }
        }
      }
    }
    
    if (length(seen) %% 2 != 0) {
      stop("An error occurred: the seen vector has an odd number of elements, which should not be possible.")
    }
    
    win.ct[trial] <- rd.ct + length(seen) / 2
  }
  
  print(mean(win.ct >= 40))
})
Rprof(NULL)
summaryRprof()$by.self
