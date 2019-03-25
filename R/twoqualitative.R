#' qualitative data (two way)
#'
#' development of concepts which help us to understand social phenomena in natural settings, giving due emphasis to the meanings, experiences and views of the participants
#'
#' @param A,B,c,Y,r,i,j
#'
#' @return
#'
#' @examples
#'
#' @export
#'
qual2<-function(A,B,c,Y,r,i,j){
  v<-matrix(k$Y,i,j)
  print(v)

  name.y <- paste(deparse(substitute(Y)))
  name.p <- paste(deparse(substitute(A)))
  name.sp <- paste(deparse(substitute(B)))
  name.ap <- paste(deparse(substitute(C)))
  A<-as.factor(A)
  B<-as.factor(B)
  c<-as.factor(C)
  cat("\nClass level information\n\n")
  np  <- length(unique(A))
  nsp <- length(unique(B))
  nap <- length(unique(B))
  cat(name.p,  "\t: ",unique(as.character(A)),"\n")
  cat(name.sp, "\t: ",unique(as.character(B)),"\n")
  cat(name.ap, "\t: ",unique(as.character(C)),"\n")
  cat("\nNumber of observations: ", length(Y), "\n\n")

  ct<-sum(c(v[,2],v[,4],v[,6],v[,8]))^2/(length(unique(A))*length(unique(B))*120)
  st<-sum(c(v[,2],v[,4],v[,6],v[,8]))-ct
  sa<-sum(c(sum(v[,2]),sum(v[,4]),sum(v[,6]),sum(v[,8]))^2)/(120*length(unique(B)))-ct
  sb<-sum(sum(c(v[1,2],v[1,4],v[1,6],v[1,8]))^2,sum(c(v[2,2],v[2,4],v[2,6],v[2,8]))^2)/(120*length(unique(A)))-ct
  sab<-sum(v[1,2]^2,v[1,4]^2,v[1,6]^2,v[1,8]^2,v[2,2]^2,v[2,4]^2,v[2,6]^2,v[2,8]^2)/120-ct
  se1<-sab-sa-sb
  se2<-st-sab

  n<-lm(Y ~ A+B+c)
  m<-anova(n)

  e<-NULL
  e<-m[c(1,2,3,4),]
  e[1,2]<-round(sa,3)
  e[1,3]<-e[1,2]/e[1,1]
  e[2,2]<-round(sb,3)
  e[2,3]<-e[2,2]/e[2,1]
  e[3,1]<-(length(unique(A))-1)*(length(unique(B))-1)
  e[3,2]<-se1
  e[3,3]<-e[3,2]/e[3,1]
  e[1,4]<-e[1,3]/e[3,3]
  e[2,4]<-e[2,3]/e[3,3]
  e[4,1]<-(120*length(unique(A))*length(unique(B))-1-e[1,1]-e[2,1]-e[3,1])
  e[4,2]<-se2
  e[4,3]<-e[4,2]/e[4,1]
  e[3,4]<-e[3,3]/e[4,3]

  f<-as.numeric(e[1,4])
  l<-as.numeric(e[2,4])
  p<-as.numeric(e[3,4])
  g<-as.numeric(e[1,1])
  j<-as.numeric(e[2,1])
  h<-as.numeric(e[3,1])
  o<-as.numeric(e[4,1])

  e[1,5]<-1-pf(f,g,h)
  e[2,5]<-1-pf(l,j,h)
  e[3,5]<-1-pf(p,h,o)

  n<-NULL
  n[1]<- name.p
  n[2]<- name.sp
  n[3]<- "E1(A*B)"
  n[4]<- "E2"
  rownames(e)<-n
  print(e)
}
