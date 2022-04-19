set.seed(10)
m = 100;  n = 20;  mu = 140;  sigma = 4;  conf.level = .95
x = rnorm(m*n, mu, sigma)
MAT = matrix(x, nrow=m)  # m x n matrix: each row a sample of size n
x.bar = rowMeans(MAT);  s = apply(MAT, 1, sd)

t.crit = qt(1-(1-conf.level)/2, n-1)
LCL = x.bar - t.crit*s/sqrt(n);  UCL = x.bar + t.crit*s/sqrt(n)
cover = LCL < mu & UCL > mu
HI = max(UCL); LO = min(LCL)  # to set dimensions of the plot


png("ic.png", units="in", width=8, height=4, res=600)

par(mar = c(4, 6, 2, 2))
plot(c(0, m+1), c(HI, LO),
     col="white", ylab="",
     axes = FALSE,
     xlab="Amostras aleatórias",
     main="", xaxs="i")

abline(h=mu, col="green2", cex=2)

for(i in 1:m) {
  bar="blue"
  if (cover[i]==F) {
    bar="red"
    }
  lines(c(i,i), c(UCL[i], LCL[i]), col=bar, lwd=2)
  points(i, x.bar[i], col = "black", pch = 1)
}

# Add X-axis
axis(1, at = seq(0, 100, 10))
#axis(2, at = c(seq(135, 139, 1), seq(141, 144, 1)))

#axis(side=2, at = c(130, 140, 2))
mtext(expression(mu), side = 2, line=3.5, at=140,
      adj = 1, cex=2, las = 1)
# mtext("= 140", side = 2, line=1, at=140,
#       adj = 1, cex=1.4, las = 1)
mtext("= ?", side = 2, line=1, at=140,
      adj = 1, cex=1.7, las = 1)

dev.off()

##############
