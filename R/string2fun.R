##'  @description This function is designed to be called on a string and retruns a function (via closure) that evaluates the input string.
##' @details The input string is most likely to be generated as output from \code{\link{getCstring}} or \code{\link{getQstring}}. A list of inputs is evaluated to a single number using the eval command. N.B. this means that the variables in the string must be present, and that the input string must be a valid (valuable) arithmetic expression. If a data.frame is supplied, this is detected, and a vector of outputs returned with each a value for each row in the input data.frame.
##' @title String to function
##' @param ss a string
##' @return a function
##' @author Pete Dodd
##' @export
string2fun <- function(ss){
    fn1 <- function(x){                 #function for list
        list2env(x,envir=environment())
        eval(parse(text=ss))
    }
    fn <- function(x){                  #function for df/list
        if(is.data.frame(x)){
            X <- split(x,seq(nrow(x)))
            unlist(lapply(X = X,FUN = fn1))
        } else {
            fn1(x)
        }
    }
    fn
}
