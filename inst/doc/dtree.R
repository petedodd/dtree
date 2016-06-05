## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=FALSE)

## ----eval=FALSE----------------------------------------------------------
#  devtools::install_github('petedodd/dtree',dependencies=TRUE,build_vignettes=TRUE)

## ----eval=TRUE-----------------------------------------------------------
library(dtree)

## ----eval=TRUE-----------------------------------------------------------
## example syntax
trss0 <- "
\\nodes...(this line is  comment)
\\ (this line is another comment) syntax:
\\ nodename ['label | cost variable | QoL variable']
a[ 'description a | costA | qolA'] \\ a node
b[ 'description b | costB | qolB']
c[  'description c | costC | qolC']
d[  'description d | costD | qolD']
e[  'description e | costE | qolE']
f[  'description f | costF | qolF']
g[  'description g | costG | qolG']
h[  'description h | costH | qolH']
i[  'description i | costI | qolI']
\\edges
\\ from_nodename -> to_nodename['probability variable for this edge']
a -> b ['p1']
a -> c ['1-p1']
c -> e ['p2']
c -> f['p3']
c -> g['p4']
b -> h['p5']
b -> i['p6']
b -> d['p7']
\\ (whitespace is ignored in parsing for calculation)
"

## ------------------------------------------------------------------------
dtree(trss0,view=TRUE) ## visualize the overall tree

## ------------------------------------------------------------------------
decT <- dtree(trss0,view=FALSE) ##analyse tree
str(decT)

## ------------------------------------------------------------------------
costpars <- list(costA=1,costB=2,costC=3,costD=1,costE=2,costF=3,costG=2,costH=3,costI=3,
                 qolA=.4,qolB=1,qolC=0.9,qolD=.4,qolE=1,qolF=0.9,qolG=.4,qolH=1,qolI=0.9,
                 p1=1/3,p2=1/3,p3=1/3, p4=1/3,p5=1/3,p6=1/3, p7=1/3) #a list of input parameters

decT$costfun(costpars) #calculate the mean cost for these parameters
decT$qolfun(costpars) #calculate the mean QoL for these parameters

## ------------------------------------------------------------------------
costparz <- as.data.frame(costpars) #list to data.frame
costparz <- costparz[rep(1,1e2),] #a data.frame with 100 identical rows
costparz$cost <- decT$costfun(costparz) # output a vector of length 100 & augment input
costparz$qol <- decT$qolfun(costparz) #augment input data with outputs
summary(costparz$cost) #check makes sense

## ------------------------------------------------------------------------
csts <- paste0('cost',LETTERS[1:9]) # cost variables
for(cst in csts)
  costparz[,cst] <- costparz[,cst] * (1+0.2*runif(nrow(costparz))) #perturb
costparz$costB <- costparz$costB + costparz$costC # make sure these are correlated
cor(costparz$costB,costparz$costC) #correlation
plot(data=costparz,costB ~ costC) #view
qls <- paste0('qol',LETTERS[1:9]) # QoL variables
for(ql in qls)
  costparz[,ql] <- costparz[,ql] * runif(nrow(costparz)) #perturb
# renew cost data
costparz$cost <- decT$costfun(costparz) # costs
costparz$qol <- decT$qolfun(costparz) # QoL
plot(data=costparz,cost ~ qol,asp=1,xlim=c(0,2)) #view

