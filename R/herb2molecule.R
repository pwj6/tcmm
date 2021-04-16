#' how to make herb to molecule
#'
#' @param x an variable
#'
#' @return molecule_name
#' @export
#'
#' @examples
#' .h2m(x='Ziziphi Spinosae Semen',type='latin')
#' .h2m(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin')
.h2m<-function(x,type="latin"){
  {
    type <- match.arg(type,c("latin","pinyin","chinese"))
    if(type=="pinyin")
      x<-drug[pinyin%in%tolower(x),]$latin
    else if(type=="chinese")
      x<-drug[chinese%in%x,]$latin
  }
  {
    if(length(x)==1)
      y<-drugchem[herb==x,]$molecule_name
    else if(length(x)>1)
      y<-sapply(x,.h2m)
    else
      y<-NA
  }
  y
}
