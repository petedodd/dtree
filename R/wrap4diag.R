##' Convert model definition string as DOT language
##'
##' This is a helper function that takes the string defining the decision tree, strips out comments etc. and wraps it to make a valid DOT language string for use by \code{DiagrammeR} for visualisation, or export for use in Graphviz etc.
##' 
##' @title Convert model definition string as DOT language
##' @param x model definition string
##' @return string in DOT language defining tree
##' @author Pete Dodd
##' @export
wrap4diag <- function(x){
    ## get rid of comments
    lnz <- unlist(strsplit(trim(x),split='\n')) #lines
    lnz <- unlist(lapply(as.list(lnz),FUN=function(x) unlist(strsplit(x,split="\\\\"))[1] )) ## strip comment
    x <- paste0(lnz,collapse="\n")
    ## top/tail and substitute
    ssb <- "digraph dot {\ngraph [layout = dot,rankdir=LR];\nnode[shape=record]\n"
    sse <- "\n}"
    x <- gsub("\\[","[label=",x)
    x <- paste0(ssb,x,sse)
    x
}
