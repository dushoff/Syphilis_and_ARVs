
## Array or variables?

## Systematic, simplified variable names: SS SI ST IS

##I looked at Milner and Zhao 2010 for simple SIR syphilis model... I think it seems reasonable..?

g <- function(t,yini,parameters) {
	with(as.list(c(yini,expand(parameters))), 
	{ 
		 S <- yini[1:2]
		 I <- yini[3:4]
		 T <- yini[5:6]
		 
		 N = S + I + T
		 
		 J = I + eps_b * T ##Treatment decreases the rate of transmission
		 
		 Nsum_b = sum(beta_h* N)
		 Jsum_b = sum(beta_h* J)
		 
		 lambda = rho * beta_h * J/N + (1-rho) * beta_h * Jsum_b/Nsum_b
		 
		 dS <- mu * N0 - lambda * S - mu * S
		 
		 dI <- lambda * S  - tau * I + sigma * T - mu * I - alpha_h * I
		 
		 dT <- tau * I - sigma * T - mu * T - eps_a * alpha_h * T
		 
		 list(c(dS, dI, dT))
	})
}

gx <- function(t,yini,parameters) {
	with(as.list(c(yini,expand(parameters))), { 
		S <- yini[1:2]
		I_h <- yini[3:4]
		T_h <- yini[5:6]
		I_s <- yini[7:8]
		I_hs <- yini[9:10]
		T_hs <- yini[11:12]
		R_s <- yini[13:14]
		I_hR <- yini[15:16]
		T_hR <- yini[17:18]

		N <- S + I_h + T_h + I_s + I_hs + T_hs + R_s + I_hR + T_hR

		##Does coinfection affect rate of transmission of both diseases?
		J_h <- I_h + I_hs + I_hR + eps_b * (T_h +  T_hs + T_hR)

		J_s <- I_s + I_hs + T_hs

		lambda_h <- (
			rho * beta_h * J_h/N
			+ (1-rho) * beta_h * sum(beta_h*J_h)/sum(beta_h*N)
		)

		lambda_s <- rho * beta_s * J_s/N + (1-rho) * beta_s * sum(beta_s*J_s)/sum(beta_s*N)

		dS <- mu * N0 -(lambda_s + lambda_h) * S - mu*S + p * gamma * I_s

		dI_h <- -lambda_s * I_h + lambda_h * S - (mu + tau + alpha_h) * I_h + sigma * T_h + p * gamma * I_hs

		dT_h <- - lambda_s * T_h -(mu + sigma + eps_a * alpha_h) * T_h + tau * I_h + p * gamma * T_hs

		dI_s <- lambda_s * S - lambda_h * I_s + eps_R * lambda_s * R_s - mu * I_s - gamma * I_s 

		dI_hs <- lambda_s * I_h + lambda_h * I_s + eps_R * lambda_s * I_hR - (mu + tau + alpha_h) * I_hs + sigma * T_hs - gamma * I_hs

		dT_hs <- lambda_s * T_h + eps_R * lambda_s * T_hR - (mu + sigma + eps_a * alpha_h) * T_hs + tau * I_hs - gamma * T_hs

		##individuals with partial immunity can be infected again at a lower transmission rate... 
		dR_s <- -lambda_h * R_s - eps_R * lambda_s * R_s - mu * R_s + (1-p) * gamma * I_s
			
		dI_hR <- lambda_h * R_s - eps_R * lambda_s * I_hR - (mu + tau + alpha_h) * I_hR + sigma * T_hR  + (1-p) * gamma * I_hs
			
		dT_hR <- - eps_R * lambda_s * T_hR - (mu + sigma + eps_a * alpha_h) * T_hR + tau * I_hR + (1-p) * gamma * T_hs

		return(list(c(
			dS, dI_h, dT_h, dI_s, dI_hs, dT_hs, dR_s, dI_hR, dT_hR
		)))
	})
}

gfun <- function(parameters, syph=FALSE) {
	if(syph) return(gx) else return(g)
}

calc_yini <- function(parameters, syph = FALSE){
	with(c(expand(parameters)),{
		yini <- list(
			S  = (1 - iniI) * N0,
			I = iniI * N0,
			T = c(0,0))
		
		yini2 <- list(
			S = (1 - 2 * iniI) * N0,
			I_h = iniI * N0,
			T_h = c(0,0),
			I_s = iniI * N0,
			I_hs = c(0,0),
			T_hs = c(0,0),
			R_s = c(0,0),
			I_hR = c(0,0),
			T_hR = c(0,0)
			)
		if(syph) return(yini2) else return(yini)
	})
}
