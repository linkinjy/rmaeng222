#' Latin Squares
#'
#' In combinatorics and in experimental design, a Latin square is an n × n array filled with n different symbols, each occurring exactly once in each row and exactly once in each column.
#'
#' @param a,b,c,d,i,j
#'
#' @return
#'
#' @examples
#'
#' @export

latin<-function(a,b,c,d,i,j){
  mydata <- data.frame(a,b,c,d)
  print(matrix(mydata$c, i,j))
  print(matrix(mydata$d, i,j))
  anova(lm(d~ a+b+c, mydata))
}
