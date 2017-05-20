ordered.lsa <-
function(decomp.matrix){

lsa.ordered <- as.data.frame(matrix(0, nrow = nrow(decomp.matrix), ncol = 2*ncol(decomp.matrix)))

j <- 0
for(i in seq(2, (2*ncol(decomp.matrix)), 2))
  {
  j <- j+1
    lsa.ordered[ ,i-1] <- names(decomp.matrix[order(decomp.matrix[ ,i-j], decreasing = T),i-j])
    lsa.ordered[ ,i] <- decomp.matrix[order(decomp.matrix[ ,i-j], decreasing = T),i-j]
  }

  colnames(lsa.ordered) <- rep(1:ncol(decomp.matrix), each = 2)
  return(lsa.ordered)
}

