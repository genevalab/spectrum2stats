#' McDonald-Kreitman test
#'
#' This function performs uses site frequency spectra to create a four
#' cell contingency table of fixed and segregating, selected and neutral
#' sites. This table is used to perform a McDonald-Kreitman test, and
#' caculate summaries (alpha, DoS)
#'
#' @param sel The unfolded site frequency spectrum for selected sites
#' @param neut The unfolded site frequency spectrum for neutral sites
#'
#' @references
#' McDonald, J., and M. Kreitman. 1991. Adaptive protein evolution at the Adh locus in Drosophila. Nature 351:652–654.
#' Eyre-Walker DoS
#' Smith and Eyre Walker alpha
#' @keywords sfs alpha
#' @export
#' @examples
#' mk_sfs(sel, neut)


mk_sfs <- function(sel, neut){
  if(nrow(sel)!=nrow(neut) & ncol(sel)!=ncol(neut)){
    cat("Neutral and Selected SFS are unequally sized")
    break
  }

  if(nrow(sel)>1){
    sel <- colSums(sel)
    neut <- colSums(neut)
  }

  length(sel) -> x
  dn <- sel[x]
  ds <- neut[x]
  pn <- sum(sel[1:(x-1)])
  ps <- sum(neut[1:(x-1)])

  alpha <- 1 - ((neut[x]*sel[1:(x-1)])/ (sel[x]*neut[1:(x-1)]))

  mk.test <-

  ni <-

  DoS <-

  result <- data.frame("MK.test" = mk.test, "neutrality.index" = ni ,"alpha" = alpha ,"DoS" = DoS)
  return(result)
}


