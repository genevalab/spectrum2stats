#' Asymtotic alpha
#'
#' This function uses the asymptotic approach described by Messer & Petrov (2013)
#' to estimate the population genetic paramater alpha. This function returns
#' fittedalpha[0] and alpha[1] as well as an optional figure depicting
#' alpha[1/n], alpha[2/n],..., alpha[n-1] and the fitted line
#'
#' @param sel The unfolded site frequency spectrum for selected sites
#' @param neut The unfolded site frequency spectrum for neutral sites
#' @param plot Should the points and fitted line be plotted
#' @param main Main title passed to plot()
#' @param ... Additional graphical parameters to pass to plot()
#'
#' @references
#' Messer, P. W., and D. Petrov. 2013. Frequent adaptation and the McDonald–Kreitman
#'  test. Proceedings of the National Academy of Sciences. 110(21):8615–8620.
#'
#' @keywords sfs alpha
#' @export
#' @examples
#' asym_alpha(sel, neut, plot=FALSE, main="", ...)


asym_alpha <- function(sel, neut, plot=FALSE, main="", ...){
  length(sel) -> x
  alpha <- 1 - ((neut[x]*sel[1:(x-1)])/ (sel[x]*neut[1:(x-1)]))
  data <- data.frame("frequency"=(1:(x-1))/x, "alpha" = alpha)

  mod <- nls(alpha ~ a - (b*exp(-(c*frequency))),data = data, start = list(a = 1, b = 1, c=1), control = nls.control(maxiter= 500, minFactor = 1/1000000))
  alpha1 <- summary(mod)$parameters[1,1] - summary(mod)$parameters[2,1] * exp(-1*summary(mod)$parameters[3,1])
  alpha0 <- summary(mod)$parameters[1,1] - summary(mod)$parameters[2,1] * exp(0*summary(mod)$parameters[3,1])
  result <- data.frame("alpha_1" = alpha1, "alpha_0" = alpha0)
  if (plot){
    plot(c(0,(1:(x-1))/x, 1), c(alpha0,predict(mod, list(x = (1:(x-1))/x, 1)),alpha1), main=main, col="red", lwd=2, type="l",xlab="Derived allele frequency", ylab=expression(paste("Estimated ", alpha)),las=1, xlim=c(0,1), ...)
    points(data, pch=20)
  }
  return(result)
}



