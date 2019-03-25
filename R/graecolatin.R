#' graeco latin squares
#'
#' In combinatorics, a Graeco-Latin square or Euler square or orthogonal Latin squares of order n over two sets S and T, each consisting of n symbols, is an n×n arrangement of cells, each cell containing an ordered pair (s,t), where s is in S and t is in T, such that every row and every column contains each element of S and each element of T exactly once, and that no two cells contain the same ordered pair.
#'
#' @param a,b,c,d,y,i,j
#'
#' @return
#'
#' @examples
#'
#' @export

# graeco latin squares
greco.latin<-function(a,b,c,d,y,i,j){
  mydata <- data.frame(a,b,c,d,y)
  print(matrix(paste(mydata$c,mydata$d),i,j))
  print(matrix(mydata$y,i,j))
  anova(lm(y~ a+b+c+d, mydata))
}
