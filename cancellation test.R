library(ltm)
install.packages("sirt")
library(sirt)

## preparing data via calculation of proportion of correct responses
preparedata.freq <- function(x) {
totalscore2 <- rowSums(x)
xnew <- cbind(x, totalscore2)
orderedx <- xnew[order(xnew$totalscore2),]
correctresponses <- aggregate(orderedx, by=list(orderedx$totalscore2), sum)
correctresponses <- correctresponses[order(-correctresponses$Group.1),]
n <- ncol(x)
correctresponses <- correctresponses[,2:(n+1)]
totalresponses <- correctresponses[FALSE,]
for(i in 0:n) {
  totalresponses[(n+1-i),] <- rep(nrow(subset(orderedx,orderedx$totalscore2 == i)), n)
}
proportioncorrect <- correctresponses/totalresponses
data2 <- proportioncorrect[,order(colMeans(x))]
return(data2)
}

## prepare with jml
preparedata.jml <- function(x) {
  estimate <- rasch.jml(x) ## joint maximum likelihood estimation of parameters 
  items <- estimate$item
  ordereditems <- x[,order(-items$itemdiff)] 
  ## the persons are ordered according to theta in estimate$person
  persons <- estimate$person
  theta <- persons$theta
  data <- cbind(ordereditems, theta)
  data <- data[order(data$theta),] ## order people according to theta
  data <- aggregate(data, by=list(-data$theta), mean) ## calculate mean score for each group with particular total score
  n <- ncol(x)
  data <- data[,2:(n+1)]
  return(data)
}  

## prepare with mml
preparedata.mml <- function(x) {
  estimate <- rasch.mml2(x) ## joint maximum likelihood estimation of parameters 
  items <- estimate$item
  ordereditems <- x[,order(-items$b)] 
  ## the persons are ordered according to theta in estimate$person
  persons <- estimate$person
  theta <- persons$EAP
  data <- cbind(ordereditems, theta)
  data <- data[order(data$theta),] ## order people according to theta
  data <- aggregate(data, by=list(-data$theta), mean) ## calculate mean score for each group with particular total score
  n <- ncol(x)
  data <- data[,2:(n+1)]
  return(data)
} 

## function for testing double cancellation axiom
## outcome is evaluation of whether double cancellation is fulfilled across matrix
## if axioms fail to be fulfilled, locations are shown 
## data[i,j] marks the cell at the top of the arrow marking first antecedent

## PASS means antecedent is TRUE and ifthen statement is TRUE
## FAIL means antecedent if TRUE and ifthen statement is FALSE
## PASS as NA means antecedent is FALSE and hence ifthen statement is TRUE


doublecancellation <- function(x) {
n <- nrow(x)
m <- ncol(x)
a<-data.frame(row=as.numeric(0), column=as.numeric(0), result=as.numeric(0))

for(i in 1:(n-2)) {
  for(j in 2:(m-1)){
if(x[i, j] > x[i+1,j-1] & x[i+1, j+1] > x[i+2, j]) {
  if(x[i, j+1] > x[i+2, j-1]) {
    rivi <- c(i, j, "PASS")
    a <- rbind(a, rivi)
  } else {
    rivi <- c(i, j, "FAIL")
    a <- rbind(a, rivi)
  }
} else {
  rivi <- c(i, j, "PASS as NA")
  a <- rbind(a, rivi)
}
}
}
cancellation <- a[-1,]
## print(cancellation, row.names = FALSE)
nofails <- !is.element("FAIL", cancellation$result)
aretherenas <- is.element("PASS as NA", cancellation$result)
fails <- c(which("FAIL" == cancellation$result))
nas <- c(which("PASS as NA" == cancellation$result))

if(nofails == "FALSE") {
  print(c("Data fulfills axiom:", nofails))
  print(cancellation[fails,]) 
} else { 
  print(c("Data fulfills axiom:", nofails))
  print("No fails to show")
}

if(aretherenas == TRUE) {
  print(c("Review data"))
  print(cancellation[nas,]) 
} else {
  print(c("Data review not required"))
}

}

## Compare to
##library(ltm)
#install.packages("ConjointChecks")
#library(ConjointChecks)
#lsatprepare <- PrepareChecks(LSAT)
#checkedlsat <- ConjointChecks(lsatprepare$N, lsatprepare$n, n.3mat = 1)
