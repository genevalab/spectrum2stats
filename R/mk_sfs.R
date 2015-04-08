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
#' Charlesworth B. 1993. The effect of background selection against deleterious mutations on weakly selected, linked variants. Genetic Research 63:213-227.
#' Rand DM, Kann A. 1996. Polymorphism in mitochondrial DNA: contrasts among genes from Drosophila, mice, and humans. Molecular Biology and Evolution 13:735-748.
#' Stoletzki, N and Eyre-Walker, A. 2011. Estimation of the Neutrality Index. Molecular Biology and Evolution 28(1):63-70.


#' @keywords sfs alpha
#' @export
#' @examples
#' data(spectrum2stats)
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
  dn <- round(sel[x])
  ds <- round(neut[x])
  pn <- round(sum(sel[1:(x-1)]))
  ps <- round(sum(neut[1:(x-1)]))

  alpha <- 1 - (ds*pn)/(dn*ps)
  ni <- (ds*pn)/(dn*ps)
  DoS <- dn/(dn+ds) - pn/(pn+ps)

  mk.test <-

  result <- data.frame("MK.test" = mk.test, "neutrality.index" = ni ,"alpha" = alpha ,"DoS" = DoS)
  return(result)
}


