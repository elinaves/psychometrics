## example of preparing data

install.packages("sirt")
library(sirt)
estimate <- rasch.jml(LSAT) ## joint maximum likelihood estimation of parameters ? can I control discrimination ?
items <- estimate$item
itemorder <- LSAT[,order(-items$itemdiff)] 
totalscore <- rowSums(itemorder) ## calculate totalscores for each person
data <- cbind(itemorder, totalscore)
data <- data[order(data$totalscore),] ## order people according to totalscore
data <- aggregate(data, by=list(-data$totalscore), mean) ## calculate mean score for each group with particular total score
data <- data[,2:6]

## function for testing double cancellation axiom of conjoint measurement
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
    
