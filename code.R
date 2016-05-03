#
# 1.7
#

# Histogramme
draw_hist <- function(x) {
  n = length(x)
  k = floor(1 + log2(n))
  x1_star = min(x)
  xn_star = max(x)
  a0 = floor(x1_star - 0.025 * (xn_star - x1_star))
  ak = ceiling(xn_star + 0.025 * (xn_star - x1_star))
  h = (ak - a0)/k
  hist(x, prob=T, breaks=seq(a0, ak, h))
}

# Estimations
get_estimates <- function(x) {
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

question7 <- function() {
  theta = 1000
  n = 20
  x = sample(1:theta, n, replace=T)
  
  draw_hist(x)
  
  # Graphe de probabilite
  plot(sort(x)[1:n], seq(1:n)/n, ylim=c(0, 1))
  
  estimates = get_estimates(x)
}

question7()

#
# 1.8
#

bias_error_estimators <- function(n, m, theta) {
  biases = list(
    theta_mean = 0,
    theta_med = 0,
    theta_max = 0,
    theta_graph = 0,
    theta_best = 0
  )
  variances = list(
    theta_mean = 0,
    theta_med = 0,
    theta_max = 0,
    theta_graph = 0,
    theta_best = 0
  )
  errors = list(
    theta_mean = 0,
    theta_med = 0,
    theta_max = 0,
    theta_graph = 0,
    theta_best = 0
  )
  
  for(i in 1:m) {
    x = sample(1:theta, n, replace=T)
    estimates = get_estimates(x)
    
    for(name in names(biases)) {
      biases[[name]] = biases[[name]] + (estimates[[name]] - theta)/m
    }
  }
  
  for(name in names(errors)) {
    # EQM = variance + biais^2
    errors[[name]] = variances[[name]] + biases[[name]]^2
  }
  
  res = list(
    biases = biases,
    errors = errors
  )
  return(res)
}

question8 <- function() {
  m_vals = c(2, 5, 10)
  n_vals = c(10, 20, 40)
  theta = 1000
  n = 10
  
  for(m in m_vals) {
    measures = bias_error_estimators(n, m, theta)
    plot(m, measures$biases$theta_mean)
  }
}

question8()

#
# 1.10
#

is_in_conf_int <- function(theta, n, alpha) {
  x = sample(1:theta, n, replace=T)
  ualpha = qnorm(1 - alpha/2)
  theta_tilde = get_estimates(x)$theta_mean
  
  a = 3 * n^2 - ualpha^2
  b = 6 * n^2 * theta_tilde
  delta = -4 * (3 * n^2 * ualpha^2 * (1 - theta_tilde^2) - ualpha^4)
  
  return((b - sqrt(delta))/(2 * a) <= theta && theta <= (b + sqrt(delta))/(2 * a))
}

question10 <- function() {
  theta = 1000
  n_vals = c(10, 100, 1000)
  m_vals = c(10, 50, 100)
  alpha_vals = c(0.1, 0.05, 0.01)
  
  freqs = vector(length=length(m_vals))
  
  for(alpha in alpha_vals) {
    for(n in n_vals) {
      for(m_index in 1:length(m_vals)) {
        m = m_vals[m_index]
        nb_in = 0
        
        for(i in 1:m) {
          if(is_in_conf_int(theta, n, alpha)) {
            nb_in = nb_in + 1
          }
        }
        freqs[m_index] = nb_in/m * 100
      }
      
      plot(m_vals, freqs)
      title(main=paste('alpha=', toString(alpha), ' ; n=', toString(n)))
    }
  }
}

question10()
