##' Turn tree data in catalog form into a tree data structure
##'
##' This would normally be applied to output from \code{\link{data2catalog}} (which determines the identity of root and reshapes data into catalog form). Catalog form (input) is node-by-node data with information on children and their probabilities. The tree structure returned is the same data in a recursive list, for onward use in the functions \code{\link{getQstring}} and \code{\link{getCstring}}.
##' 
##' @title Make a tree
##' @param nm a character specifying the tree root
##' @param x tree data in catalog form
##' @return tree
##' @author Pete Dodd
##' @export
makeTree <- function(nm,x){
    tree <- list()
    tree[[nm]] <- list(n=x[[nm]]$n,
                       c=x[[nm]]$c,
                       q=x[[nm]]$q,
                       p=x[[nm]]$p,
                       k=list()
                       )
    for(K in x[[nm]]$k)                 #recurse for children
        tree[[nm]]$k <- c(tree[[nm]]$k,makeTree(K,x=x))
    tree
}
