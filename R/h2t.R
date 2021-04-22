#' How to make herb to symbol or fullname
#'
#' @param x is the information adout herb
#' @param type is the type of the x
#' @param output is the type of output
#'
#' @return symbol or fullname
#' @export
#'
#' @examples
#' .h2t(x='Ziziphi Spinosae Semen',type='latin',output='symbol')
#' .h2t(x='Ziziphi Spinosae Semen',type='latin',output='fullname')
#' h2t(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='symbol')
#' h2t(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='fullname')
h2t<-function(x,type="latin",output="symbol")
{
  y<-sapply(x,.h2t,type=type,output=output)
  y
}
#' @export
.h2t<-function(x,type="latin",output="symbol"){
  {
    type <- match.arg(type,c("latin","pinyin","chinese"))
    output <- match.arg(output,c("symbol","fullname"))
    if(length(x)>1)
      stop("Length of x must be 1!")
    else if(type=="pinyin")
      x<-drug[pinyin%in%tolower(x),]$latin
    else if(type=="chinese")
      x<-drug[chinese%in%x,]$latin
  }
  {
    if(length(x)==1)
    {
      if(output=="symbol")
        y <- na.omit(unique(drugtarget[herb==x,]$symbol))
      else
        y <- unique(drugtarget[herb==x,]$fullname)
    }
    else
      y<-NA
  }
  y
}
