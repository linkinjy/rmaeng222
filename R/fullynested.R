#' Fully Nested
#'
#' If all arguments are stake
#'
#' @param pplot,splot,aploy,rplot,y
#'
#' @return
#'
#' @examples
#'
#' @export

# fully nested
sp.plot2<-function(pplot,splot,aplot,rplot,y){
  mydata <- data.frame(pplot,splot,aplot,y)
  name.y <- paste(deparse(substitute(y)))
  name.p <- paste(deparse(substitute(pplot)))
  name.sp <- paste(deparse(substitute(splot)))
  name.ap <- paste(deparse(substitute(aplot)))
  name.rp <- paste(deparse(substitute(rplot)))

  pplot<-as.factor(pplot)
  splot<-as.factor(splot)
  aplot<-as.factor(aplot)
  rplot<-as.factor(rplot)
  cat("\nClass level information\n\n")
  nrep<- length(unique(y))
  np  <- length(unique(pplot))
  nsp <- length(unique(splot))
  nap <- length(unique(aplot))
  nrp <- length(unique(rplot))
  cat(name.p,  "\t: ",unique(as.character(pplot)),"\n")
  cat(name.sp, "\t: ",unique(as.character(splot)),"\n")
  cat(name.ap, "\t: ",unique(as.character(aplot)),"\n")
  cat(name.rp, "\t: ",unique(as.character(rplot)),"\n")
  cat("\nNumber of observations: ", length(y), "\n\n")
  mo<-anova(lm(y~pplot/splot/aplot ))
  N<-NULL
  N[1]<- name.p
  N[2]<- paste(name.sp,"(",name.p,")",sep="")
  N[3]<- paste(name.ap,"(",name.p,name.sp,")",sep="")
  N[4]<- "E"
  rownames(mo)<-N
  mo
}
