#' how to make molecule to symbol or fullname
#'
#' @param x is the name or cid of molecule
#' @param type is the type of x
#' @param output is the type of output
#' @return symbol or fullname
#' @export
#'
#' @examples
#' .m2t(x='FER',type='molecule',output='symbol')
#' .m2t(x='445858',type='cid',output='fullname')
#'  m2t(x=c('FER','pyrene'),type='molecule',output='symbol')
#'  m2t(x=c('445858','31423'),type='cid',output='fullname')
m2t<-function(x,type="cid",output="symbol")
{
  y<-sapply(x,.m2t,type=type,output=output)
  y
}
#' @export
.m2t<-function(x,type="cid",output="symbol"){
  {
    type <- match.arg(type,c("cid","molecule"))
    output <- match.arg(output,c("symbol","fullname"))

    if(length(x)>1)
      stop("Length of x must be 1!")
  }
  {
    if(type=="cid")
    {
      if(output=="symbol")
        y<-na.omit(unique(chemtarget[cid==x,]$symbol))
      else
        y<-unique(chemtarget[cid==x,]$fullname)
    }
    else
    {
      if(output=="symbol")
        y<-na.omit(unique(chemtarget[molecule==x,]$symbol))
      else
        y<-unique(chemtarget[molecule==x,]$fullname)
    }
  }
  y
}

