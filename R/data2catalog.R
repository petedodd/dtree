##' Turn parsed data around a tree and returns it in in catalog form
##' 
##' The input data format is that given as output from \code{\link{parseData}} and used as input by \code{\link{makeTree}}. A catalog (output) is a list of nodes with data on their associated cost, QoL, children and probabilities for each child. The parsed data (input) is a list of nodes (their costs/QoLs), edges and probabilities. The identity of the root is also returned as needed by \code{\link{makeTree}} in building the tree.
##' 
##' @title Convert parsed data to catalog
##' @param X parsed data
##' @return list of: \code{catalog} - a catalog;  and \code{root} - character id of root node.
##' @author Pete Dodd
##' @export
data2catalog <- function(X){
    ## node data
    ansl <- list()
    for(i in 1:length(X$nodes$names)){
        ansl[[X$nodes$names[i]]] <- list(n=X$nodes$names[i],
                                              c=X$nodes$costs[i],
                                              q=X$nodes$qols[i],
                                              k=list(),p=c())
    }

    ## edge data
    for(i in 1:length(X$edges$from)){        #go through edges
        frm <- X$edges$from[i]
        tom <- X$edges$to[i]
        pb <- X$edges$prob[i]
        ansl[[frm]]$k <- c(ansl[[frm]]$k,tom) #another child
        ansl[[frm]]$p <- c(ansl[[frm]]$p,pb) #another child prob
    }

    ## find root
    inkids <- haskids <- c()
    for(i in 1:length(ansl)){
        if(length(ansl[[i]]$k)>0){
            haskids <- c(haskids,ansl[[i]]$n)
            inkids <- c(inkids,unlist(ansl[[i]]$k))
        }
    }
    haskids <- haskids[! haskids %in% inkids]
    if(length(haskids)==0)
        stop('No root candidates!')
    if(length(haskids)>1)
        stop('Multiple root candidates!')

    return(list(catalog=ansl,root=haskids ))
}
