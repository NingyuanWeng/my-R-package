#' @Title myncurve
#'
#' @param x
#' @param mu mean
#' @param sigma sd
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(x,mu, sigma) {
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu-3*sigma,x,length=1000)

  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(mu-3*sigma,xcurve,x),c(0,ycurve,0),col="Yellow")
  prob=pnorm(x,mean=mu,sd=sigma)-pnorm(mu-3*sigma,mean=mu,sd=sigma)
  prob=round(prob,4)
  text(x,0.5*dnorm(x,mean=mu,sd=sigma), paste("Area = ", prob, sep=""))

  }


