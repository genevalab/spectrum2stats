#' Folded site frequency spectrum
#'
#' This function takes an unfolded site frequency spectrum and returns a folded sfs
#' @param sfs The unfolded site frequency spectrum
#' @keywords sfs folded_sfs
#' @export
#' @examples
#' fold_sfs(sfs)


fold_sfs <- function(sfs) {
    U <- sfs
    foldedL <- ceiling(length(U)/2)
    F <- rep(0,foldedL)
    for (i in 1:(foldedL-1)) {
      F[i] = U[i] + U[length(U)-i+1]
      if ((length(U) %% 2) == 0) {  # even
        F[foldedL] = U[foldedL] + U[foldedL+1]
      }
      else {  # odd
        F[foldedL] = U[foldedL]
      }
    }
    return(F)
}