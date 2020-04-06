f.p.pk <- function(p, pk, sk, k){
##
## Transform between p and pi(k)
#
if(!missing(pk)){
	.p <- 1 - (1 - pk/sk)^(1/k)
	return(.p)
}
if(!missing(p)){
	.pk <- (1 - (1 - p)^k)*sk
	return(.pk)
}
}