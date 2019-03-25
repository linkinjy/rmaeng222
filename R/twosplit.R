#' split(two way)
#'
#' split of factor
#'
#' @param block,pplot,splot,aplot,Y
#'
#' @return
#'
#' @examples
#'
#' @export
#'
sp.plot1<-function(block,pplot,splot,aplot,Y){
  name.y <- paste(deparse(substitute(Y)))
  name.r <- paste(deparse(substitute(block)))
  name.p <- paste(deparse(substitute(pplot)))
  name.sp <- paste(deparse(substitute(splot)))
  name.ap <- paste(deparse(substitute(aplot)))

  block<-as.factor(block)
  pplot<-as.factor(pplot)
  splot<-as.factor(splot)
  aplot<-as.factor(aplot)

  cat("\nClass level information\n\n")
  nrep<- length(unique(block))
  np  <- length(unique(pplot))
  nsp <- length(unique(splot))
  nap <- length(unique(aplot))
  cat(name.p,  "\t: ",unique(as.character(pplot)),"\n")
  cat(name.sp, "\t: ",unique(as.character(splot)),"\n")
  cat(name.ap, "\t: ",unique(as.character(aplot)),"\n")
  cat(name.r,  "\t: ",unique(as.character(block)),"\n")
  cat("\nNumber of observations: ", length(Y), "\n\n")
  model<- aov(Y ~ block*pplot*splot*aplot)
  B<-suppressWarnings(anova(model))
  W<-NULL
  W<-B[c(1,2,3,7,16,4,9,10,14,16),]
  W
  for (j in 1:2){
    W[5,j]<-B[5,j]+B[6,j]+B[11,j]
    W[10,j]<-B[8,j]+B[12,j]+B[13,j]+B[15,j]
  }
  W[,3]<-W[,2]/W[,1]
  W[1:4,4]<-W[1:4,3]/W[5,3]
  W[6:9,4]<-W[6:9,3]/W[10,3]
  W[5,4]<-W[5,3]/W[10,3]
  # Pvalue
  W[1:4,5]<-1-pf(W[1:4,4],W[1:4,1],W[5,1])
  W[6:9,5]<-1-pf(W[6:9,4],W[6:9,1],W[10,1])
  N<-NULL
  N[1]<- name.r
  N[2]<- name.p
  N[3]<- name.sp
  N[4]<- paste(name.p,":",name.sp,sep="")
  N[5]<- "E1"
  N[6]<- name.ap
  N[7]<- paste(name.p,":",name.ap,sep="")
  N[8]<- paste(name.sp,":",name.ap,sep="")
  N[9]<- paste(name.p,":",name.sp,":",name.ap,sep="")
  N[10]<- "E2"
  rownames(W)<-N
  print(W)
}
