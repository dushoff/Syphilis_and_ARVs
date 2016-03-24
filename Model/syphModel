gfun <- function(parameters, experimental=FALSE) {
  pp <- expand(parameters)
  
  g <- function(t,yini,parameters) {
      with(as.list(c(yini,pp)), 
      { 
          S <- yini[1:2]
          I <- yini[3:4]
          T <- yini[5:6]
          
          N = S + I + T
          
          J = I + eps_b * T
          
          Nsum_b = sum(beta* N)
          Jsum_b = sum(beta* J)
          
          lambda = rho * beta * J/N + (1-rho) * beta * Jsum_b/Nsum_b
          
          dS <- mu * (N - S) - lambda * S 
          
          dI <- lambda * S  - tau * I + sigma * T - mu * I - alpha * I
          
          dT <- tau * I - sigma * T - mu * T - eps_a * alpha * T
          
          list(c(dS, dI, dT))
      })
  }

  return(g)
}

yini <- list(
	S = c(0.499, 0.499),
	I = c(0.001, 0.001),
	T = c(0, 0)
)
