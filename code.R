#
# 1.7
#

theta = 1000
n = 20
x = sample(1:theta, n, replace=T)

# Histogramme
draw_hist(x) <- function(x) {
  n = length(x)
  k = floor(1 + log2(n))
  x1_star = min(x)
  xn_star = max(x)
  a0 = floor(x1_star - 0.025 * (xn_star - x1_star))
  ak = ceiling(xn_star + 0.025 * (xn_star - x1_star))
  h = (ak - a0)/k
  hist(x, prob=T, breaks=seq(a0, ak, h))
}

# Graphe de probabilite
plot(sort(x)[1:n], seq(1:n)/n, ylim=c(0, 1))

# Estimations
get_estimations <- function(x) {
  xn_star = max(x)
  n = length(x)
  slope = lm(seq(1:n)/n ~ sort(x)[1:n])$coefficients[[2]]
  
  res = list(
    theta_mean = 2 * mean(x) - 1,
    theta_med = 2 * median(x) - 1,
    theta_max = xn_star,
    theta_graph = 1/slope,
    theta_best = (xn_star^(n+1) - (xn_star - 1)^(n+1))/(xn_star^n - (xn_star - 1)^n)
  )
  return(res)
}

#
# 1.8
#

