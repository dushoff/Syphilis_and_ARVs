
## Array or variables?

## Systematic, simplified variable names: SS SI ST IS

##I looked at Milner and Zhao 2010 for simple SIR syphilis model... I think it seems reasonable..?

FOI.fun <- function(N, Y, beta, rho, c){
	FOI <- rho * beta * Y/N + (1 - rho) * beta * sum(c * Y)/sum(c * N)
}

g.base <- function(t,yini,parameters) {
	with(as.list(c(yini,expand(parameters))), 
	{ 
		 S <- yini[1:2]
		 I <- yini[3:4]
		 T <- yini[5:6]
		 
		 N = S + I + T
		 
		 J = I + eps_b * T ##Treatment decreases the rate of transmission
		 
		 FOI.H = FOI.fun(N, J, beta.HIV, rho, c)
		 
		 dS <- mu * N0 - FOI.H * S - mu * S
		 
		 dI <- FOI.H * S  - tau * I + sigma * T - mu * I - alpha.H * I
		 
		 dT <- tau * I - sigma * T - mu * T - eps_a * alpha.H * T
		 
		 return(list(c(dS, dI, dT)))
	})
}

g.syph <- function(t,yini,parameters) {
	with(as.list(c(yini,expand(parameters))), { 
		SS <- yini[1:2]
		IS <- yini[3:4]
		TS <- yini[5:6]
		SI <- yini[7:8]
		II <- yini[9:10]
		TI <- yini[11:12]
		ST <- yini[13:14]
		IT <- yini[15:16]
		TT <- yini[17:18]

		N <- SS + IS + TS + SI + II + TI + ST + IT + TT

		##Does coinfection affect rate of transmission of both diseases?
		J.H <- IS + II + IT + eps_b * (TS + TI + TT)

		J.S <- SI + II + TI

		FOI.H = FOI.fun(N, J.H, beta.HIV, rho, c)
		
		FOI.S = FOI.fun(N, J.S, beta.syph, rho, c)
		
		dSS <- mu * N0 -(FOI.S + FOI.H) * SS - mu*SS + p * gamma * SI + delta * ST

		dIS <- -FOI.S * IS + FOI.H * SS - (mu + tau + alpha.H) * IS + sigma * TS + p * gamma * II + delta * IT

		dTS <- - FOI.S * TS -(mu + sigma + eps_a * alpha.H) * TS + tau * IS + p * gamma * TI + delta * TT

		dSI <- FOI.S * SS - FOI.H * SI - mu * SI - gamma * SI 

		dII <- FOI.S * IS + FOI.H * SI - (mu + tau + alpha.H) * II + sigma * TI - gamma * II

		dTI <- FOI.S * TS - (mu + sigma + eps_a * alpha.H) * TI + tau * II - gamma * TI

		dST <- -FOI.H * ST - mu * ST + (1-p) * gamma * SI - delta * ST
			
		dIT <- FOI.H * ST - (mu + tau + alpha.H) * IT + sigma * TT  + (1-p) * gamma * II - delta * IT
			
		dTT <- - (mu + sigma + eps_a * alpha.H) * TT + tau * IT + (1-p) * gamma * TI - delta * TT

		return(list(c(
			dSS, dIS, dTS, dSI, dII, dTI, dST, dIT, dTT
		),N = N))
	})
}

gfun <- function(parameters) {
	pp <- expand(parameters)
	
	StateMat <- function(vec){
		mat <- matrix(0, nrow = 9, ncol = 2)
		for(i in vec){
			mat[vec,] = 1
		}
		return(mat)
	}
	
	sweep2 <- function(mat,vec){
		return(sweep(mat, 2, vec, "*"))
	}
	
	flow <- function(sourceMat, targetVec){
		flowMat <- matrix(0, nrow = 9, ncol = 2)
		for(i in 1:length(targetVec)){
			flowMat[targetVec[i],] = sourceMat[i,]
		}
		return(flowMat)
	}
	
	S.HIV <- c(1,4,7)
	I.HIV <- c(2,5,8)
	T.HIV <- c(3,6,9)
	S.syph <- c(1:3)
	I.syph <- c(4:6)
	T.syph <- c(7:9)
	
	g.syph2 <- function(t,yini,parameters) {
		with(as.list(c(yini,expand(parameters))), { 
			yMat <- matrix(yini, nrow = 9, ncol =2, byrow = TRUE)
			
			N <- colSums(yMat)
			J.H <- colSums(yMat[I.HIV,] + eps_b * yMat[T.HIV,])
			J.S <- colSums(yMat[I.syph,])
			
			n.birth <- sweep2(StateMat(1), mu * N0)
			n.death <- mu * yMat
			
			FOI.H <- FOI.fun(N, J.H, beta.HIV, rho, c)
			HIV.infection <- sweep2(yMat[S.HIV,],FOI.H)
			H.inf.flow <- -flow(HIV.infection, S.HIV) + 
				flow(HIV.infection, I.HIV)
			
			FOI.S <- FOI.fun(N, J.S, beta.syph, rho, c)
			syph.infection <- sweep2(yMat[S.syph,],FOI.S)
			S.inf.flow <- -flow(syph.infection, S.syph) +
				flow(syph.infection, I.syph)
			
			infection <- H.inf.flow + S.inf.flow
			
			H.death <- yMat * (StateMat(I.HIV) 
				+ eps_a * StateMat(T.HIV)) * alpha.H
			
			H.treat <- yMat[I.HIV,] * tau
			H.treat.flow <- -flow(H.treat, I.HIV) +
				flow(H.treat, T.HIV)
			
			H.treat.fail <- yMat[T.HIV,] * sigma
			H.treat.fail.flow <- -flow(H.treat.fail, T.HIV) +
				flow(H.treat.fail, I.HIV)
			
			S.treat <- yMat[I.syph,] * gamma
			S.treat.flow <- -flow(S.treat, I.syph) +
				(1-p) * flow(S.treat, T.syph) + p * flow(S.treat, S.syph)
			
			S.treat.fail <- yMat[T.syph,] * delta
			S.treat.fail.flow <- -flow(S.treat.fail, T.syph) +
				flow(S.treat.fail, S.syph)
			
			treatment <- H.treat.flow + S.treat.flow + 
				H.treat.fail.flow + S.treat.fail.flow
			
			dy <- n.birth - n.death + infection - H.death + treatment
			
			dSS <- dy[1,]; dIS <- dy[2,]; dTS <- dy[3,]
			dSI <- dy[4,]; dII <- dy[5,]; dTI <- dy[6,]
			dST <- dy[7,]; dIT <- dy[8,]; dTT <- dy[9,]
			
			return(list(c(
				dSS, dIS, dTS, dSI, dII, dTI, dST, dIT, dTT
			),N = N))
		})
	}
	return(g.syph2)
}

calc_yini <- function(parameters, syph = FALSE){
	with(c(expand(parameters)),{
		yini <- list(
			S  = (1 - iniI) * N0,
			I = iniI * N0,
			T = c(0,0))
		
		yini2 <- list(
			SS = (1 - 2 * iniI) * N0,
			IS = iniI * N0,
			TS = c(0,0),
			SI = iniI * N0,
			II = c(0,0),
			TI = c(0,0),
			ST = c(0,0),
			IT = c(0,0),
			TT = c(0,0)
			)
		if(syph) return(yini2) else return(yini)
	})
}
