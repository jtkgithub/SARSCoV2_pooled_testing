f.sens <- function(k){
# sens 0.99 at k=1, 0.90 at k=32
.b1 <- (qnorm(0.9) - qnorm(0.99))/(32 - 1)
.b0 <- qnorm(0.99) - .b1
.sens <- pnorm(.b0 + .b1*k)
return(.sens)
}