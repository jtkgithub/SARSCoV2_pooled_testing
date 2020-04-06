f.CI <- function(p, k, sk, N){
##
## Confidence intervals based on transformed Wilson
#
library(binom)
#
##
.pk <- f.p.pk(p = p, sk = sk, k = k)
##
.int <- binom.confint(x = .pk * N, n = N, methods = "wilson", conf.level = 0.9)
##
.ut <- f.p.pk(pk = c(p = .int$mean, lower = .int$lower, upper = .int$upper), sk = sk, k = k)
#
return(.ut)
}