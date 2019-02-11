SD.plus.FUN <-
function(x){

# function by Cheung et al.
  #a <- x$Age[which.max(x$dx)]
  rank <- order(x$dx, decreasing = TRUE)
  a <- x$Age[rank[which(rank > 100)][1]]
  M <- a + ((x$dx[a]-x$dx[a-1])/(x$dx[a]-x$dx[a-1])+(x$dx[a]-x$dx[a+1]))
  b <- seq(ceiling(M),last(x$Age),0.1)
  part.one <- c(rep(0,length(b)))

  ## for loop (for now)
  for (k in b) {
    part.one <- (sum(b[k]-M))^2/length(b)
    }
  SD.plus <- sqrt(part.one)
  ### !!! For comparison with other estimates - IF decimal ages are used
  #SD.plus <- SD.plus*10
  return(SD.plus)
}
