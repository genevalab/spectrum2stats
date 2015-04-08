#' Bootstrap alpha
#'
#' This function calculates alpha_1 and alpha_0 values for a set of
#' user specified genes from a large user supplied set, then assesses
#' the significance of these values by perform a user-specified number
#' of resampling replicates, drawn from the full set of genes.
#' Operations are performed by calling the \code{asym_alpha} function.
#'
#' @param sel.sfs The unfolded site frequency spectrum for selected sites for a colletion of genes one per line
#' @param neut.sfs The unfolded site frequency spectrum for neutral sites for a collection of genes one per line
#' @param plot Should the points and fitted line be plotted
#' @param main Main title passed to plot()
#' @param ... Additional graphical parameters to pass to plot()
#'
#' @references
#' Messer, P. W., and D. Petrov. 2013. Frequent adaptation and the McDonald–Kreitman
#'  test. Proceedings of the National Academy of Sciences. 110(21):8615–8620.
#'
#' @keywords sfs alpha bootstrap
#' @export
#' @examples
#' data(spectrum2stats)
#' bootstrap_alpha(genes, sel.sfs, neut.sfs, reps=1000)


bootstrap_alpha <- function(genes, sel.sfs, neut.sfs, reps=1000){
  #genes <- read.table(pheno_set, stringsAsFactors = F)

  #calculate empirical alpha_1 and alpha_0 values
  sel.emp <- sel.sfs[genes,]
  sel.emp <- sel.emp[!is.na(sel.emp[,1]),]
  neut.emp <- neut.sfs[genes,]
  neut.emp <- neut.emp[!is.na(neut.emp[,1]),]
  empirical <- asym_alpha(colSums(sel.emp),colSums(neut.emp))

  # perform bootstrap
  bootstrap.sims <- data.frame(matrix(nrow=reps, ncol=2))
  colnames(bootstrap.sims) <- c("alpha_1","alpha_0")
  i <- 1
  while (i<=reps){
    boot <- sample(1:nrow(sel.sfs),nrow(sel.emp))
    sel.boot <- sel.sfs[boot,]
    sel.boot <- sel.boot[!is.na(sel.boot[,1]),]
    neut.boot <- neut.sfs[boot,]
    neut.boot <- neut.boot[!is.na(neut.boot[,1]),]
    out <- data.frame("alpha_1" = NA, "alpha_0" = NA)
    try(asym_alpha(colSums(sel.boot),colSums(neut.boot)) -> out)
    if (!is.na(out[,1]) & !is.na(out[,2])){
      bootstrap.sims[i,1] <- out$alpha_1
      bootstrap.sims[i,2] <- out$alpha_0
      i <- i + 1
    }
  }
  output <- list("empirical" = empirical, "bootstrap" = bootstrap.sims)
  return(output)
}

