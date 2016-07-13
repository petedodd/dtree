
## parse the string and extract the data
parseData <- function(x){
    lnz <- unlist(strsplit(trim(x),split='\n')) #lines
    lnz <- lnz[lnz!=""]               #ditch empty lines
    lnz <- unlist(lapply(as.list(lnz),FUN=function(x) unlist(strsplit(x,split="\\\\"))[1] )) ## strip comment
    lnz <- trimall(lnz)
    lnz <- lnz[lnz!=""]                 #ditch empty lines
    edz <- grepl("->",lnz)
    edges <- lnz[edz]
    nodes <- lnz[!edz]
    ## ---edges---
    espl <- strsplit(edges,split='->')
    efrom <- unlist(lapply(espl,function(x)x[[1]]))  #the froms
    eto <- unlist(lapply(espl, function(x) unlist(strsplit(x[2],split='\\['))[1])) #the tos
    eprob <- unlist(lapply(espl, function(x) unlist(strsplit(x[2],split="'"))[2])) #the probabilities
    ## ---nodes---
    nnames <- unlist(lapply(nodes, function(x) unlist(strsplit(x,split='\\['))[1])) #the node names
    nct <- unlist(lapply(nodes, function(x) unlist(strsplit(x,split="'"))[2])) #node data
    ndat <- strsplit(nct,split="\\|")     #node data
    lbz <- unlist(lapply(ndat,function(x)x[1]))
    cstz <- unlist(lapply(ndat,function(x)x[2]))
    qlz <- unlist(lapply(ndat,function(x)x[3]))
    ## return values
    list(edges=list(from=efrom,to=eto,prob=eprob),
         nodes=list(names=nnames,labels=lbz,costs=cstz,qols=qlz))
}


## trimming utility functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trimall <- function (x) gsub("\\s+", "", x)
