# f <- function(x) cos(x*20)
f <- function(x, w = pi/10) as.numeric(((x) %/% (w)) %% 2 == 0)
# f <- function(t, w = pi/2) 1*(t > -w/2 & t < w/2)
plot(f)
expi <- function(x) cos(x) + sin(x)*1i
L <- 40
medias_Re <- c()
fs <- c()
as <- seq(0.01, 1, l = 140)

animation::saveGIF({
  par(mfrow = c(1,2))
  for(a in as) {
    x <- seq(0, a*40, l = 500)
    ft <- expi(x)*f(x, w = pi/10 * (a*L))
    media_Re <- mean(Re(ft))
    plot(Re(ft), Im(ft), xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2), main = "cos(20t) - Mass Center on Complex Plane")
    points(media_Re, mean(Im(ft)), col = "red", cex = 3, pch = 16 )
    medias_Re <- c(medias_Re, media_Re)
    fs <- c(fs, a*L)
    plot(fs, medias_Re, type = "l", xlim = c(0, 40), ylim = c(-0.11, 0.51))
    points(a*L, media_Re, col = "red", cex = 3, pch = 16)
    Sys.sleep(0.1)
  }

}, interval = 0.1, ani.width = 600, ani.height = 300)

