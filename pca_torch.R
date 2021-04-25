library(torch)
library(mvtnorm)
library(datasauRus)

# set.seed(42)
# x <- mvtnorm::rmvnorm(100, c(1,2), sigma = matrix(c(1.2,1,1,1.1), 2))
# plot(x)

animation::saveGIF({
  for(dataset in datasaurus_dozen$dataset){
  x <- as.matrix(datasauRus::datasaurus_dozen[datasaurus_dozen$dataset == dataset,c("x", "y")])
  x <- torch_tensor(x)

  rot <- function(theta) {
    linha1 <- torch_stack(list(cos(theta), -sin(theta)))
    linha2 <- torch_stack(list(sin(theta), cos(theta)))
    resp <- torch_stack(list(linha1, linha2))
    resp$squeeze()
  }

  rotacionar <- function(x, theta) {
    r <- rot(theta)
    m <- torch_mean(x, dim = 1)

    (torch_mm(r, (x - m)$t()))$t() + m
  }

  theta <- torch_tensor(0, requires_grad = TRUE)
  optim <- torch::optim_adam(theta, lr = 0.1)
  m <- as.numeric(torch_mean(x, 1))
  s <- as.numeric(torch_std(x, 1))

  for(i in 1:100) {
    optim$zero_grad()
    x_rot <- rotacionar(x, theta)
    loss <- -torch_std(x_rot[,1])
    loss$backward()
    optim$step()

    plot(x_rot[,1], x_rot[,2], xlim = m + 2*s[2]*c(1, -1), ylim = m + 2*s[1]*c(1, -1))

    segments(x0 = m[1] - (-as.numeric(loss)), y0 = m[2], x1 = m[1] + (-as.numeric(loss)), y1 = m[2], col = "red", lwd = 3)
    Sys.sleep(0.2)
  }
}
}, interval = 0.1, ani.width = 600, ani.height = 600)
