animals.em.da = function (theta = 0.3, n = 10, m = 100) 
{	# Data augmentation algorithm for the genetic linkage example.
    	x3 <- 18
    	x4 <- 20
    	x5 <- 34
    	x2 <- rbinom(m, 125, theta/(theta + 2))
    	for (i in 1:n) {
        	v1 <- x2 + x5 + 1
        	v2 <- rep(x3 + x4 + 1, m)
        	for (j in 1:m) {
            	k <- sample(seq(1, m), 1)
            	theta <- rbeta(1, v1[k], v2[k])
            	x2[j] <- rbinom(1, 125, theta/(theta + 2))
        	}
		# The next 7-8 lines are mostly just to give a nice plot of the distribution at step i. 
        	theta <- seq(0.001, 0.999, by = 0.001)
        	dens <- rep(0, length(theta))
        	for (k in 1:length(theta)) {
           	for (j in 1:m) dens[k] <- dens[k] + dbeta(theta[k], v1[j],v2[j])
        		dens[k] <- dens[k]/m
        	}
         	if (i==1) plot(theta,dens,type="l")
         	else (lines(theta,dens))
    	}
    	E = sum(theta*dens)             # Estimated posterior expectation.
    	V = sum(theta^2*dens) - E^2     # Estimated posterior variance.
    	print(c(E,sqrt(V)))
    	return(data.frame(theta, dens))
}





animals.em.da2 = function (theta = 0.5, n = 10, m = 100) 
{	# Another variant of the data augmentation algorithm for the genetic linkage example.
    	x3 <- 18
    	x4 <- 20
    	x5 <- 34
    	x2 <- rbinom(m, 125, theta/(theta + 2))
    	for (i in 1:n) {
        	v1 <- x2 + x5 + 1
        	v2 <- rep(x3 + x4 + 1, m)
        	for (j in 1:m) {
            	k <- sample(seq(1, m), 1)
            	theta <- rbeta(1, v1[k], v2[k])
            	x2[j] <- rbinom(1, 125, theta/(theta + 2))
        	}
		# The next 6-7 lines are mostly just to give a nice plot of the distribution at step i. 
		denstheta = function(theta){ # Function takes the mean over beta densities for all parameter values in v1, v2.
			temp = vector(length=length(theta))			
			for (i in 1:length(theta)) temp[i] = mean(dbeta(theta[i],v1,v2))
			return(temp)
		}
		if (i==1) curve(denstheta(x),from=0,to=1)
		else curve(denstheta(x),add=TRUE)
  	}
	E = integrate(function(theta) theta*denstheta(theta),lower=0,upper=1)$value
	V = integrate(function(theta) theta^2*denstheta(theta),lower=0,upper=1)$value - E^2
	return(c(E,sqrt(V)))
}







mci.expo = function(n=10000)
{	# Computes the MCI approximation to E(X^2) for X~expo(1).
	x = rexp(n,1)
	return(mean(x^2))
}


riemann.expo = function(n=10000)
{	# Computes the approximation by Riemann sums to E(X^2) for X~expo(1).
	x = rexp(n,1)
	x = sort(x)
	return(sum(x[-n]^2*exp(-x[-n])*(x[-1]-x[-n])))
}



laplace.gamma = function()
{ 	# Computes a Laplace approximation to P(7<X<9) for X~gamma(alpha=5,beta=2).
	alpha = 5
	beta = 2
	h = function(x,alpha=5,beta=2) (alpha-1)*log(x) - x/beta
#	hdiff = function(x,alpha=5,beta=2) (alpha-1)/x - 1/beta
	hdiffdiff = function(x,alpha=5,beta=2) (1-alpha)/x^2 
	x_0 = (alpha-1)*beta
	sigma = sqrt(-1/hdiffdiff(x_0))
	I1 = exp(h(x_0))*sqrt(2*pi)*sigma*(pnorm((9-x_0)/sigma)-pnorm((7-x_0)/sigma))
	print(I1)
	I = 1/gamma(alpha)/beta^alpha*exp(h(x_0))*sqrt(2*pi)*sigma*(pnorm((9-x_0)/sigma)-pnorm((7-x_0)/sigma))
	return(I)
}





simanneal.geom = function(n=1000)
{ 	# Finds the optimum of the log-likelihood function by simulated annealing.
	newp = function(p) max(min(p + runif(1,-0.05,0.05),1),0)
	geom.loglikelihood = function(p,x)
	{
	        loglikelihood = length(x)*log(p) + (sum(x)-length(x))*log(1-p)
	        return(-loglikelihood)
        }
	T = 1000
	p = 0.75
	p.vec = NULL
	for (i in 1:n){
		nu = newp(p)
		rho = min(1,exp(-(geom.loglikelihood(nu,c(2,1,1,3,1))-geom.loglikelihood(p,c(2,1,1,3,1)))/T))
		if (rbinom(1,1,rho)==1) p = nu
		T = T*0.9
#		print(c(T,p,rho))
		p.vec = c(p.vec,p)
	}
	return(p.vec)
}







