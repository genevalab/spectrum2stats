#' Fay and Wu's H
#'
#' This function takes an unfolded site frequency spectrum and returns Fay and Wu's H
#'
#' @param sfs The unfolded site frequency spectrum
#'
#' @references
#' Fay, JC and Wu, CI. 2000. Hitchiking under positive Darwinian selection. 
#' Genetics. 155:1405-1413 
#'
#' @keywords sfs FWH
#' @export
#' @examples
#' FWH_sfs(sfs)



FWH_sfs <-function(sfs) {
  U <- sfs
  n <- (length(sfs) + 1)
  H2temp <- sapply(X=1:(n-1), FUN=function(X) {(2*X*((2*X)-n))/(n*(n-1))})
  Htemp <- U*H2temp
  H <- sum(Htemp)
  return(H)
}