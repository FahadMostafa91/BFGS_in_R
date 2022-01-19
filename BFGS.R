library(readr)
library(tidyr)
library(ggplot2)

###### data input 
dat <- read_excel("~/Downloads/plotval_data.xlsx")
#https://bookdown.org/rdpeng/advstatcomp/quasi-newton.html

ggplot(dat, aes(x = no2)) + 
  geom_density()

nll_one <- deriv(~ -log(dnorm((x - mu)/s) / s) + log(0.5),
                 c("mu", "s"), 
                 function.arg = TRUE)
#First the negative log-likelihood.

nll <- function(p) {
  v <- nll_one(p[1], p[2])
  sum(v)
}

# Then the gradient function.

nll_grad <- function(p) {
  v <- nll_one(p[1], p[2])
  colSums(attr(v, "gradient"))
}

#numerically calculate the Hessian matrix at the optimum point.
x <- dat$no2
res <- optim(c(1, 5), nll, gr = nll_grad, 
             method = "BFGS", hessian = TRUE)
res   # print hessian

## lower bound for all parameters to be 0 but allow the upper bound to be infinity
res <- optim(c(1, 10), nll, gr = nll_grad, 
             method = "L-BFGS-B", hessian = TRUE,
             lower = 0)
res
solve(res$hessian) %>%
  diag %>%
  sqrt

# we will evaluate the fitted model at 100 points between 0 and 100.

xpts <- seq(0, 50, len = 100)
dens <- data.frame(xpts = xpts,
                   ypts = dnorm(xpts, res$par[1], res$par[2]))

### fitted model on top of the density
ggplot(dat, aes(x = no2)) + 
  geom_density() + 
  geom_line(aes(x = xpts, y = ypts), data = dens, col = "steelblue",
            lty = 2)

nll_one <- deriv(~ -log(lambda * dnorm((x-mu1)/s1)/s1 + (1-lambda)*dnorm((x-mu2)/s2)/s2), 
                 c("mu1", "mu2", "s1", "s2", "lambda"), 
                 function.arg = TRUE)
######------------------------###########
nll <- function(p) {
  p <- as.list(p)
  v <- do.call("nll_one", p)
  sum(v)
}
nll_grad <- function(p) {
  v <- do.call("nll_one", as.list(p))
  colSums(attr(v, "gradient"))
}

#########-----------------##############

x <- dat$no2
pstart <- c(5, 10, 2, 3, 0.5)
res <- optim(pstart, nll, gr = nll_grad, method = "L-BFGS-B",
             control = list(parscale = c(2, 2, 1, 1, 0.1)),
             lower = 0, upper = c(Inf, Inf, Inf, Inf, 1))
res

xpts <- seq(0, 100, len = 100)
dens <- with(res, {
  data.frame(xpts = xpts, 
             ypts = par[5]*dnorm(xpts, par[1], par[3]) + (1-par[5])*dnorm(xpts, par[2], par[4]))
})
ggplot(dat, aes(x = no2)) + 
  geom_density() + 
  geom_line(aes(x = xpts, y = ypts), data = dens, col = "steelblue",
            lty = 2)


