##' Main user function for \code{dtree} package, wrapping most other functions
##'
##' This function takes a string in the syntax described in the vignette (type \code{vignette('dtree')} to view) and returns various related data. If \code{view=TRUE}, there is not return, but a browser is opened to view the decision tree specified via the \code{DiagrammeR} package. If \code{view=FALSE}, strings defining the mean costs and QoLs for the decision tree are returned, as well as functions which apply these to PSA data in the form of \code{data.frame}s. A DOT string is also returned for export and generation of graphs via Graphviz, as is the original input string.
##' 
##' @title Main user function for \code{dtree} package
##' @param ss input string defining model
##' @param view logical for returning data or viewing graph
##' @return list of \itemize{
##'   \item \code{coststring} - string giving mean cost for model
##'   \item \code{qolstring} - string giving mean QoL for model
##'   \item \code{costfun} - function for calculating cost for model
##'   \item \code{qolfun} - function for calculating QoL for model
##'   \item \code{root} - string identifying the root of model
##'   \item \code{dot} - string in DOT language for export and visualization
##'   \item \code{inputstring} - the string used as input.
##' }
##' @author Pete Dodd
##' @import DiagrammeR 
##' @export
dtree <- function(ss,view=TRUE){
    ssv <- wrap4diag(ss)                #graph code
    if(view)
        return(DiagrammeR::grViz( ssv ))               #visualize
    anso <- parseData(ss)               #preprocess
    ansl <- data2catalog(anso)## make into catalogue
    anstree <- makeTree(ansl$root,ansl$catalog)         #catalog to tree
    coststring <- getCstring(anstree[[ansl$root]])    #tree to string (call on root)
    qolstring <- getQstring(anstree[[ansl$root]])    #tree to string (call on root)
    myCfun <- string2fun(coststring)        #string to function
    myQfun <- string2fun(qolstring)        #string to function
    list(coststring=coststring,
         qolstring=qolstring,
         costfun=myCfun,
         qolfun=myQfun,
         root=ansl$root,
         dot=ssv,
         inputstring=ss)
}
