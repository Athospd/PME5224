animation::saveGIF({
  f <- function(t, l = 1) (t>0)*exp(-t*l)
  h <- function(t) as.numeric(abs(t) < 1)
  g <- function(t, tau = seq(-2,10,l=1000)) mean(f(tau) * h(t-tau))

  par(mfrow = c(2,1))
  tau <- seq(-2,10,l=1000)
  ts  <- seq(-2,10,l=100)
  gs <- c()

  for(t in ts) {

    plot(tau, h(t-tau), type = "l")
    lines(tau, f(tau), col = "red")
    gs <- c(gs, g(t))
    plot(ts[1:length(gs)], gs, xlim = range(ts), ylim = c(0,0.08), type = "l", xlab = "t", ylab = "g(t)")
    Sys.sleep(0.1)
  }
}, interval = 0.05)

