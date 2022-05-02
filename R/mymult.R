#' @title mymult
#' @param iter a vector of numbers
#' @param n a vector of numbers
#' @param p a vector of numbers
#'
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @examples
#' mymult(iter=1000,n=10,p=c(1,2,3,4,2)/12)
mymult=function(iter=100,n=10, p=c(1,1,1,1)/4){
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  k=length(p)
  tab.mat=matrix(NA,nr=k,nc=iter, byrow=TRUE)
  for(i in 1:iter){
    sam.mat[,i]=sample(1:k,n,replace=TRUE, prob=p)
    tab.mat[,i]=table(factor(sam.mat[,i],levels=1:k))
  }
  freq=apply(tab.mat,1,sum)
  names(freq)=1:k
  barplot(freq/(n*iter),col=rainbow(k) )
  tab.mat
}
