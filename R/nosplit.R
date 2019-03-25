#' can not split
#'
#' the factor can not split
#'
#' @param pplot,splot,y
#'
#' @return
#'
#' @examples
#'
#' @export
#'
sp.plot3<-function(pplot,splot,y){
  name.y <- paste(deparse(substitute(y)))
  name.p <- paste(deparse(substitute(pplot)))
  name.sp <- paste(deparse(substitute(splot)))

  pplot<-as.factor(pplot)
  splot<-as.factor(splot)
  cat("\nClass level information\n\n")
  nrep<- length(unique(y))
  np  <- length(unique(pplot))
  nsp <- length(unique(splot))
  cat(name.p,  "\t: ",unique(as.character(pplot)),"\n")
  cat(name.sp, "\t: ",unique(as.character(splot)),"\n")
  cat("\nNumber of observations: ", length(y), "\n\n")
  v<-anova(lm(y~pplot*splot))
  v[1,4]<-v[1,3]/v[3,3]
  v[2,4]<-v[2,3]/v[3,3]
  v[3,4]<-v[3,3]/v[4,3]
  N<-NULL
  N[1]<- name.p
  N[2]<- name.sp
  N[3]<- "E1"
  N[4]<- "E2"
  rownames(v)<-N
  v
}
