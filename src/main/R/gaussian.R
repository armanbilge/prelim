library(partitions)

x3 <- c(-0.3736164938224994,1.3113304976688522,-0.16655755314238938)
x4 <- c(-0.16655755314238938,-0.7006214643695872,0.8883330492829065,1.3598570775521472)
x5 <- c(0.8883330492829065,1.3598570775521472,-0.8293949318787277,-0.2648362783966567,0.37252597479221194)

f <- function(x) function(mu) sapply(mu, function(m) prod(dnorm(x, m, 1))) * dnorm(mu, 0, 1)
g <- function(x) integrate(f(x), -Inf, Inf)$value
h <- function(x) {
  n <- length(x)
  parts <- listParts(n)
  q <- rep(0, n)
  for (p in parts) {
    m <- length(p)
    q[m] <- q[m] + prod(sapply(p, function(y) g(x[y]) * factorial(length(y) - 1)))
  }
  p <- q / sum(q)
  return(sum(1:n * p))
}

h(x3)
h(x4)
h(x5)
