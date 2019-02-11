## Function to calculate the mode (after age 5) 
#  x is the age with the highest number of deaths in the life table at the time
MDA.fun <- function(lt) {
  
  # fine if we work with integer ages above age 10
  #x <- lt$Age[which.max(lt$dx)]
  #M <- x + ((lt$dx[x]-lt$dx[x-1])/(lt$dx[x]-lt$dx[x-1])+(lt$dx[x]-lt$dx[x+1]))
  
  
  # our version to work with decimal ages from 0
  ranks <- order(lt$dx, decreasing = TRUE)
  i <- 1
  idx <- ranks[i]
  x <- lt$Age[idx]
  while(x < 10){
    i <- i + 1
    idx <- ranks[i]
    x <- lt$Age[idx]
  }
  M <- x
  
  return(M)
}

dump("MDA.fun","R/functions/MA5_FUN.R")