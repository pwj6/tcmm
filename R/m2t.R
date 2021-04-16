#' how to make molecule to target
#'
#' @param x is the name of molecule
#'
#' @return target_name
#' @export
#'
#' @examples
#' .m2t(x='FER')
#' .m2t(x=c('FER','pyrene'))
.m2t<-function(x){
  {
    if(length(x)==1)
      y<-unique(chemtarget[molecule_name==x,]$target_name)
    else if(length(x)>1)
      y<-sapply(x,.m2t)
    else
      y<-NA
  }
  y
}
