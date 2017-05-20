jac_similarity <- function(nWords, nDim, matrix, rotate=FALSE){
  
  if (missing(rotate)) {
    original <- matrix$tk[,1:(nDim)]
    orderVec <- ordered.lsa(original)
  }
  else {
    original <- promax(scale(matrix$tk[,1:(nDim)]),m = 2)
    orderVec <- ordered.lsa(original$loadings)
  }
  
  vector <- 1:length(orderVec)
  n <- length(vector)
  odd <- vector[seq(n)%%2==1]
  Jac <- as.vector(1:(length(odd)))
  for (j in 1:length(odd)){
    v1 <- orderVec[1:nWords,odd[j]]
    for (i in 1:length(odd)){
      v2 <- orderVec[1:nWords,odd[i+1]]
      I <- length(intersect(v1,v2))
      S <- I/(length(v1)+length(v2)-I)
      Jac[i] <- S
    }
  }
  MN <- mean(Jac)
  return(MN)
}