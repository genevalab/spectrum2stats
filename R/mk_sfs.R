#' McDonald-Kreitman test
#'
#' This function uses site frequency spectra to create a four
#' cell contingency table of fixed and segregating, selected and neutral
#' sites. This table is used to perform a McDonald-Kreitman test, and
#' caculate summaries (neutrality index, alpha, DoS)
#'
#' @param sel A vector containing the unfolded site frequency spectrum for selected sites
#' @param neut A vector containing the  unfolded site frequency spectrum for neutral sites
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
#'
#' mk_sfs(colSums(sel), colSums(neut)) #perform MK test on sum of all genes
#'
#' mk_sfs(sel[,1], neut[,1]) #perform MK test on single genes


mk_sfs <- function(sel, neut){
  if(length(sel)!=length(neut)){
    cat("Error: Neutral and Selected objects are unequally sized")
    break
  }

  length(sel) -> x
  dn <- round(sel[x])[[1]]
  ds <- round(neut[x])[[1]]
  pn <- round(sum(sel[1:(x-1)]))[[1]]
  ps <- round(sum(neut[1:(x-1)]))[[1]]

  alpha <- 1 - (ds*pn)/(dn*ps)
  ni <- (ds*pn)/(dn*ps)
  DoS <- dn/(dn+ds) - pn/(pn+ps)

  mk.tab <- data.frame(matrix(c(c(ds,dn),c(ps,pn)), ncol=2))
  mk.t <- fisher.test(mk.tab)
  if (is.na(alpha)){
    mk.t$p.value <- NA
    mk.t$estimate <- NA
  }

  mk.test <- list("table" = mk.tab, "p.value" = mk.t$p.value, "odds.ratio" = mk.t$estimate)
  result <- list("MK.test" = mk.test, "neutrality.index" = ni ,"alpha" = alpha ,"DoS" = DoS)
  return(result)
}


