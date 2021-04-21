#' how to make molecule to fullname
#'
#' @param x is the name or cid of molecule
#'
#' @return fullname
#' @export
#'
#' @examples
#' .m2t(x='FER',type='molecule')
#' .m2t(x='445858',type='cid')
#'  m2t(x=c('FER','pyrene'),type='molecule')
#'  m2t(x=c('445858','31423'),type='cid')
m2t<-function(x,type="cid")
{
  y<-sapply(x,.m2t,type=type)
  y
}
#' @export
.m2t<-function(x,type="cid"){
{
    type <- match.arg(type,c("cid","molecule"))
    if(length(x)>1)
      stop("Length of x must be 1!")
  }
  {
    if(type=="cid")
    y<-unique(chemtarget[cid==x,]$fullname)
    else
    y<-unique(chemtarget[molecule==x,]$fullname)
  }
  y
}

