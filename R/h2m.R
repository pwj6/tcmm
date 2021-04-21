#' how to make herb to molecule or cid of molecule
#'
#' @param x an variable
#'
#' @return molecule or cid
#' @export
#'
#' @examples
#' .h2m(x='Ziziphi Spinosae Semen',type='latin',output='molecule')
#' .h2m(x='Ziziphi Spinosae Semen',type='latin',output='cid')
#' h2m(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='molecule')
#' h2m(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='cid')
h2m<-function(x,type="latin",output="cid")
{
  y<-sapply(x,.h2m,type=type,output=output)
  y
}
.h2m<-function(x,type="latin",output="cid"){
  {
    type <- match.arg(type,c("latin","pinyin","chinese"))
    output <- match.arg(output,c("cid","molecule"))
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
      if(output=="cid")
        y <- drugchem[herb==x,]$cid
      else
        y <- drugchem[herb==x,]$molecule
    }
    else
      y<-NA
  }
  y
}


