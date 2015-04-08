#' Tajima's D
#'
#' This function takes an unfolded site frequency spectrum and returns Tajima's D
#'
#' @param sfs The unfolded site frequency spectrum
#'
#' @references
#' Tajima, F. 1989. Statistical method for testing the neutral mutation hypothesis 
#' by DNA polymorphism. Genetics. 123:585-595.
#'
#' @keywords sfs TD Tajima's D
#' @export
#' @examples
#' TD_sfs(sfs)




TD_sfs <-function(sfs){
  F <- fold_sfs(sfs)
  n <- length(sfs) + 1
  S<-sum(F)
  
  b1<-(n+1) / (3*(n-1))
  b2<-(2*(n^2+n+3))   /  (9*n*(n-1))
  
  a1temp<-sapply(X=1:(n-1), FUN=function(X)(1/X))
  a1<-sum(a1temp)
  
  a2temp<-sapply(X=1:(n-1), FUN=function(X)(1/(X^2)))
  a2<-sum(a2temp)
  
  e1<-(b1-(1/a1))/a1
  e2<-(b2-((n+2)/(a1*n))+(a2/a1^2))/(a1^2+a2)
  
  D2temp<-sapply(X=1:(length(F)), FUN=function(X) {(((2*X*(n-X))  /  (n*(n-1))) - (1/a1))   /  sqrt(e1*S + e2*S*(S-1))})
  Dtemp<-D2temp*F
  D<-sum(Dtemp)
  
  return(D)
}


  
