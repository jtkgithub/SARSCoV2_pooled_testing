p.pow.git <- function(){
## CI curves
library(binom)
#
## Temporary file name
.tmpfile <- tempfile(pattern = "plot_", tmpdir = ".", fileext = ".pdf")
#
## Create pdf
.scale <- 0.45
pdf(file = .tmpfile, height = 21*.scale, width = 21*.scale)
on.exit(dev.off())
par(mfrow = c(2,2))
#
## Prevalences
.p <-    c(0.01, 0.005, 0.001, 0.0005)
## Lower limits y-axis
.ymin <- c(0.005, 0.002, 0.0002, 0.00005)
## Upper limits y-axis
.ymax <- c(0.020, 0.013, 0.005, 0.005)
## Get the scale roughly right
.gap <- c(2, 2, 2, 0.5)/1000
## Poling levels
# .k <- c(1,2,4,8,16,32)
.k <- 1:32
## Sensitivity by pooling level
.s <- f.sens(.k)
## Sample sizes (individual people/samples)
.n <- c(1000, 2500, 5000)
## Greytones
.col <- c(gray(0.8), gray(0.5), gray(0.3))
#
## Prep
.lo.w <- .up.w <- rep(NA, length(.k))
names(.lo.w) <- names(.up.w) <- .k
#
## Loop
for(k in seq(along = .p)){ # loop over prevalences
	.main <- paste0("Population prevalence: ", .p[k]*1000, "/1000")
	plot(0, 0, lwd = 2, type = "n", ylim = log10(c(.ymin[k], .ymax[k])), xlim = log10(c(1,40)), axes = F, xlab = "Pool size (k)", ylab = "Prevalence per 1000, 90% CI", main = .main, font.lab = 2)
		.at.x <- c(1,2,4,8,16,32)
		axis(side = 1, at = log10(.at.x), labels = .at.x, font.axis = 2)
		.at <- log10(seq(0.00001, 1, .gap[k]))
		.at <- .at - .at[6] + log10(.p[k]) # center
		axis(side = 2, at = (.at), labels = signif(10^.at*1000, 2), las = 1, font.axis = 2)
		box(bty = "o", lwd = 2)
	#
	for(j in seq(along = .n)){
		for(i in seq(along = .k)){
			#
			.tmp <- f.CI(p = .p[k], k = .k[i], sk = .s[i], N = .n[j]/.k[i])
			.up.w[i] <- .tmp["upper"]
			.lo.w[i] <- .tmp["lower"]
		}
		polygon(log10(c(.k, rev(.k))), log10(c(.lo.w, rev(.up.w))), lwd = 2, col = .col[j], border = NA)
		text(x = log10(c(33, 33) + 5), y = log10(c(.lo.w[length(.lo.w)], .up.w[length(.up.w)])), labels = c(.n[j], .n[j]), font = 2)

	}
	abline(a = log10(.p[k]), b = 0, lwd = 2, col = "black")
}

return(invisible(.up.w))

}