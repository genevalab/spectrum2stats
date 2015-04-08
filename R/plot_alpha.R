#' Plot alpha
#'
#' This function plots the bootstrap distributions returned by boostrap_alpha
#'
#' @param df A dataframe returned by bootstrap_alpha
#' @param main An optional main title for each plot
#'

#' @keywords sfs alpha bootstrap
#' @export
#' @examples
#' plot_alphadf,main="alpha bootstrap")


plot_alpha <- function(df,main="alpha bootstrap"){
  stats <- vector(length=6)

  #plot alpha[1]
  h <- hist(df$bootstrap$alpha_1, plot=F,breaks=40)
  h$density = h$counts/sum(h$counts)
  plot(h,freq=F, xlab=expression(paste(alpha,"[1] estimate",sep="")), main=main, cex.main=3, cex.lab=1.5, las=1) #, xlim=c(0,1)
  rng <- par("usr")
  abline(v=quantile(df$bootstrap$alpha_1,c(0.025,0.975)), lty=3)
  abline(v=df$empirical$alpha_1, lwd=2, lty=1, col="red")
  box(lwd=2)

  #plot alpha[0]
  h <- hist(df$bootstrap$alpha_0, plot=F, ,breaks=50)
  h$density = h$counts/sum(h$counts)
  plot(h,freq=F, xlab=expression(paste(alpha,"[0] estimate",sep="")), main=main, cex.lab=1.5, cex.main=3,las=1) #xlim=c(-20,1)
  rng <- par("usr")
  abline(v=quantile(df$bootstrap$alpha_0,c(0.025,0.975)), lty=3)
  abline(v=df$empirical$alpha_0, lwd=2, lty=1, col="red")
  box(lwd=2)
  stats[1] <- sum(df$empirical$alpha_1>df$bootstrap$alpha_1)/length(df$bootstrap$alpha_1)
  stats[2] <- quantile(df$bootstrap$alpha_1, 0.025)
  stats[3] <- quantile(df$bootstrap$alpha_1, 0.975)
  stats[4] <- sum(df$empirical$alpha_0>df$bootstrap$alpha_0)/length(df$bootstrap$alpha_0)
  stats[5] <- quantile(df$bootstrap$alpha_0, 0.025)
  stats[6] <- quantile(df$bootstrap$alpha_0,0.975)
  return(stats)
}
