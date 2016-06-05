##' Obtain a cost string from a tree
##'
##' This is intended to be used on tree output from \code{\link{makeTree}} and recursively traverse through to generate a cost string, for likely use in \code{\link{string2fun}}.
##' 
##' @title Make cost string
##' @param x a tree
##' @return string
##' @author Pete Dodd
##' @export
getCstring <- function(x){
  ss <- x$c
  if(length(x$k)>0){
    ss <- paste0(ss,'+(',x$p[1],')*(',getCstring(x$k[[1]]),')')
    if(length(x$k)>1)
      for(i in 2:length(x$k))
        ss <- paste0(ss,'+(',x$p[i],')*(',getCstring(x$k[[i]]),')')
  }
  ss
}

##' Obtain a cost string from a tree
##'
##' This is intended to be used on tree output from \code{\link{makeTree}} and recursively traverse through to generate a cost string, for likely use in \code{\link{string2fun}}.
##' 
##' @title Make QoL string
##' @param x a tree
##' @return string
##' @author Pete Dodd
##' @export
getQstring <- function(x){
  ss <- x$q
  if(length(x$k)>0){
    ss <- paste0(ss,'+(',x$p[1],')*(',getQstring(x$k[[1]]),')')
    if(length(x$k)>1)
      for(i in 2:length(x$k))
        ss <- paste0(ss,'+(',x$p[i],')*(',getQstring(x$k[[i]]),')')
  }
  ss
}
